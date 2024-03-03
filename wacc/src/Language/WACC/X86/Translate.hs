{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- |
Translates Generated TAC program to X86 assembly.
-}
module Language.WACC.X86.Translate where

import Control.Monad (unless)
import Control.Monad.RWS
  ( RWS
  , asks
  , execRWS
  , gets
  , local
  , modify
  , tell
  )
import qualified Data.Array as A
import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.Int (Int64)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.WACC.TAC.FType
  ( FType
  , pattern FBool
  , pattern FChar
  , pattern FInt
  , pattern FPtr
  , pattern FString
  )
import Language.WACC.TAC.TAC
  ( BasicBlock (BasicBlock)
  , BinOp (..)
  , Jump (CJump, Jump)
  , TAC (BinInstr, LoadCI, LoadCS, LoadM, Print, Read, Store, UnInstr)
  , TACFunc (..)
  , TACProgram
  , UnOp (..)
  , Var
  )
import qualified Language.WACC.TAC.TAC as TAC
import Language.WACC.X86.IntLit (IntLit (..))
import Language.WACC.X86.Label
import qualified Language.WACC.X86.Label as X86
import Language.WACC.X86.Lib (runtimeLib)
import Language.WACC.X86.Memory (Memory (..))
import Language.WACC.X86.Operand (Operand (..), OperandQMM)
import Language.WACC.X86.OperandTH (genOperandWrappers, genRegOperand)
import Language.WACC.X86.Register (Register (..))
import qualified Language.WACC.X86.Runtime as X86
import Language.WACC.X86.X86 as X86

$(genRegOperand)

zeroQ = Imm (IntLitQ 0)

zeroD = Imm (IntLitD 0)

oneD = Imm (IntLitD 1)

-- | The state of RWS monad for translation of each function
data TransST = TransST
  { alloc :: Bimap (Var Integer) OperandQMM
  -- ^ bijection between variables in TAC to register/memory in X86
  , translated :: Set Integer
  -- ^ Set of basic blocks that have been translated
  , stackVarNum :: Integer
  -- ^ The number of stack variables used so far (not incl. saving callee saved reg)
  , labelCounter :: Integer
  -- ^ Counter for generating unique labels
  }

type TransW =
  ( DList Instruction
  , -- \^ The translated code
    DList Instruction
  , -- \^ Strings literals stored at the end of the program (before runtime lib)
    Set X86.Runtime
  )

-- \^ Set of runtime functions which need to be included

-- | Reader maps labels to basic blocks. Writer holds output X86 program
type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    TransW
    TransST

tellInstr :: Instruction -> Analysis ()
tellInstr i = tell (D.singleton i, mempty, mempty)

$(genOperandWrappers 'tellInstr)

stackElemSize :: Integer
stackElemSize = 8

translateFunc :: TACFunc Integer Integer -> Analysis ()
translateFunc (TACFunc l vs bs) = do
  resetState
  local
    (const bs)
    ( do
        lab (I l) -- label for the function = label of the first block
        pushq rbp
        movq rsp rbp -- set the stack base pointer
        mapM_ setupStackArgs (zip vs [0 ..])
        translateBlocks l startBlock -- Translate main part of code
        movq rbp rsp -- restore stack pointer
        popq rbp
        unless (l == 0) ret -- return only if not main method
        -- assigning arg vars to stack
    )
  where
    stackOffset = 16
    startBlock = bs ! l
    setupStackArgs :: (Var Integer, Integer) -> Analysis ()
    setupStackArgs (v, n) = modify (bindVarToLoc v (Mem (MRegI (stackElemSize * n + stackOffset) Rbp)))
    resetState :: Analysis ()
    resetState = modify (\x -> x {alloc = B.empty, translated = S.empty, stackVarNum = 0})

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Program
translateProg p = D.toList $ preamble <> codeSeg <> dataSeg' <> runtimeIs
  where
    runtimeIs :: DList Instruction
    runtimeIs = D.concat $ D.fromList . runtimeLib <$> S.toList runtime
    runtime :: Set X86.Runtime -- Including all the dependencies
    runtime = S.unions $ S.map (runtimeDeps A.!) rs
    dataSeg' = [Dir DirSection] <> dataSeg <> [Dir DirText]
    (_, (codeSeg, dataSeg, rs)) =
      execRWS (mapM_ translateFunc (M.elems p)) mempty (TransST B.empty S.empty 0 0)
    preamble :: DList Instruction
    preamble =
      [ Dir $ DirGlobl (S "main")
      , Dir DirSection
      , Dir DirText
      , Lab (S "main")
      ]

{- | translate each statement of the block. then figure out which block to go to
labels are printed right before this function is called
-}
translateBlocks
  :: Integer
  -> BasicBlock Integer Integer
  -> Analysis ()
translateBlocks l (BasicBlock is next) = do
  modify (setTranslated l) -- include label in translated set
  subq (Imm (IntLitQ lenStack)) rsp -- allocate space for local variables
  mapM_ translateTAC is
  translateNext next
  where
    lenStack :: Int64
    lenStack = toInt64 $ 8 * (\x -> if even x then x else x + 1) (length is)
    toInt64 :: Int -> Int64
    toInt64 = fromIntegral . toInteger

-- | Mark the block as translated, so its not re-translated
setTranslated :: Integer -> TransST -> TransST
setTranslated l x@(TransST {translated}) = x {translated = S.insert l translated}

-- | Utility to create label
mapLab :: Integer -> X86.Label
mapLab = I

-- | Utility to query state to check if label is translated
isTranslated :: Integer -> Analysis Bool
isTranslated l = gets (S.member l . translated)

{- | Flow control: For CJump translate l2 first then translate l1 after returning from the recursive call
This generates weird (but correct) assembly with else first then if. The if block appears roughly at the
end of the recursive call.
-}
translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump l) = do
  nextBlock <- asks (! l)
  t <- isTranslated l
  ( if t
      then jmp (mapLab l)
      else lab (mapLab l) >> translateBlocks l nextBlock
    )
translateNext (CJump v l1 l2) = do
  operand <- gets ((B.! v) . alloc)
  cmpq zeroQ operand
  jne (mapLab l1) -- jump to l1 if v != 0. Otherwise keep going
  initStackVarNum <- gets stackVarNum
  translateNext (Jump l2)
  t <- isTranslated l1
  ( if t
      then pure ()
      else do
        -- reset the stackVarNum to the value before else branch
        modify (\x -> x {stackVarNum = initStackVarNum})
        translateNext (Jump l1)
    )
translateNext (TAC.Ret var) = do
  retVal <- gets ((B.! var) . alloc)
  movq retVal argRet
  movq rbp rsp -- restore stack pointer
  popq rbp
  ret
translateNext (TAC.Exit x) = do
  operand <- gets ((B.! x) . alloc)
  movl operand edi
  call exit

-- | Inserts given variable into allocation map with associated location.
bindVarToLoc :: Var Integer -> OperandQMM -> TransST -> TransST
bindVarToLoc v o x@(TransST {alloc}) = x {alloc = B.insert v o alloc}

-- | Allocates memory for given variable
allocate :: Var Integer -> Analysis OperandQMM
allocate v = do
  -- increase the stackVarNum
  modify (\x@(TransST {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
  -- insert the variable into the allocation map
  stackAddr <- gets ((* (-stackElemSize)) . stackVarNum)
  modify (bindVarToLoc v (Mem (MRegI stackAddr Rbp)))
  return (Mem (MRegI stackAddr Rbp))

-- | Sanity check. Variable must not already be allocated in three address code
allocate' :: Var Integer -> Analysis OperandQMM
allocate' v = do
  -- check if the variable is already allocated
  alloc' <- gets (B.lookup v . alloc)
  case alloc' of
    Just o -> return o
    Nothing -> allocate v

-- | Gets label and updates the next fresh label available
getLabel :: Analysis X86.Label
getLabel = do
  n <- gets labelCounter
  modify (\x -> x {labelCounter = n + 1})
  return (S (".TAC_L" ++ show n))

-- | Queries the allocation map to get operand of given variable
getOperand :: Var Integer -> Analysis OperandQMM
getOperand v = gets ((B.! v) . alloc)

-------------------------------------

-- | Translate a TAC statement to X86 Instructions
translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr v1 v2 op v3) =
  withComment
    "BinInstr"
    (showD v1 <> " := " <> showD v2 <> " " <> showD op <> " " <> showD v3)
    $ do
      operand <- allocate' v1
      operand1 <- getOperand v2
      operand2 <- getOperand v3
      translateBinOp operand op operand1 operand2
translateTAC (UnInstr v1 op v2) =
  withComment "UnInstr" (showD v1 <> " := " <> showD op <> " " <> showD v2) $ do
    operand <- allocate' v1
    operand' <- getOperand v2
    translateUnOp operand op operand'
translateTAC (Store v1 off v2 w) =
  withComment "Store" (showD v1 <> " := " <> showD v2 <> "[" <> showD off <> "]") $
    translateStore v1 off v2 w
translateTAC (LoadCI v i) =
  withComment "LoadCI" (showD v <> " := " <> showD i) $ do
    operand <- allocate' v
    movq (Imm (IntLitQ $ fromIntegral i)) operand
translateTAC (LoadCS v s) =
  withComment "LoadCS" (showD v <> " := " <> showD s) $ do
    o <- allocate' v
    l <- getLabel
    tellString l s -- store the string in the data section
    leaq (Mem (MRegL l Rip)) rax
    movq rax o
translateTAC (LoadM v1 v2 off w) =
  withComment "LoadM" (showD v1 <> " := " <> showD v2 <> "[" <> showD off <> "]") $
    translateLoadM v1 v2 off w
translateTAC (TAC.Call v1 l vs) =
  withComment
    "Call"
    (showD v1 <> " := call " <> showD l <> "(" <> showD vs <> ")")
    $ do
      -- push all registers on to stack
      o <- allocate' v1
      os <- mapM getOperand vs
      mapM_ pushq (reverse os)
      -- call the function
      call (I l)
      -- pop all registers off the stack
      mapM_ popq os
      movq argRet o
translateTAC (Print v w) =
  withComment "Print" ("print" <> showD v) $ do
    operand <- getOperand v
    movq operand arg1
    translatePrint w
translateTAC (TAC.PrintLn v w) =
  withComment "PrintLn" ("println " <> showD v) $ do
    translateTAC (Print v w)
    call printLn
translateTAC (Read v w) =
  withComment "Read" (showD v <> " := read") $ do
    operand <- allocate' v
    movq operand rdi
    translateRead operand w
translateTAC (TAC.Malloc lv rv) =
  withComment "Malloc" (showD lv <> " := malloc " <> showD rv) $ do
    operand' <- getOperand rv
    movq operand' arg1
    call malloc
    operand <- allocate' lv
    movq argRet operand
translateTAC (TAC.Free v) =
  withComment "Freefree" (showD v) $ do
    operand <- getOperand v
    movq operand arg1
    cmpq zeroQ arg1
    je errNull
    call free

-- > assert 0 <= <var> < <max>
translateTAC (TAC.CheckBounds v vm reason) =
  withComment "CheckBounds" ("assert 0 <= " <> showD v <> " < " <> showD vm) $ do
    l3 <- getLabel
    l4 <- getLabel
    o <- getOperand v
    om <- getOperand vm
    cmpl zeroD o
    js l3
    movl o eax
    cmpl om eax
    jl l4
    lab l3
    movl o esi
    call
      ( case reason of
          TAC.ChrCheck -> errBadChar
          TAC.ArrayIndexCheck -> errOutOfBounds
      )
    lab l4
translateTAC (TAC.Move v1 v2) = withComment "Move" (showD v1 <> " := " <> showD v2) $ do
  operand1 <- allocate' v1
  operand2 <- getOperand v2
  movq operand2 rax
  movq rax operand1

tellString :: X86.Label -> String -> Analysis ()
tellString l s =
  tell
    ( mempty
    ,
      [ Dir $ DirInt (fromIntegral $ length s)
      , Lab l
      , Dir $ DirAsciz s
      ]
    , mempty
    )

divModPrefix o1 o2 = do
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl zeroD ebx -- check for division by zero
  je errDivByZero
  cltd -- sign extend eax into edx
  idivl ebx -- divide edx:eax by ebx

addMullSub o o1 o2 action = do
  movl o1 eax
  movl o2 ebx
  _ <- action
  jo errOverflow
  movslq eax rax
  movq rax o

logicalOPs o o1 o2 cmp set = do
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  _ <- cmp ebx eax -- compare %ebx and %eax
  _ <- set al -- set al to 1 if %eax <set flag> %ebx
  movzbq al rax
  movq rax o -- %o := %al

eqOps o o1 o2 cmp set = do
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  _ <- cmp rbx rax -- compare %ebx and %eax
  _ <- set al -- set al to 1 if %eax <set flag> %ebx
  movzbq al rax
  movq rax o -- %o := %al

{- | Translate a binary operation
| <o> := <o1> <binop> <o2>
-}
translateBinOp
  :: OperandQMM -> BinOp -> OperandQMM -> OperandQMM -> Analysis ()
translateBinOp o Add o1 o2 =
  withComment
    "Binary Addition"
    (showD o <> " := " <> showD o1 <> " + " <> showD o2)
    $ addMullSub o o1 o2 (addl ebx eax)
translateBinOp o PtrAdd o1 o2 =
  withComment
    "Binary Pointer Addition"
    (showD o <> " := " <> showD o1 <> " + " <> showD o2)
    $ do
      movq o1 rax
      movq o2 rbx
      addq rbx rax
      jo errOverflow
      movq rax o
translateBinOp o Sub o1 o2 =
  withComment
    "Binary Subtraction"
    (showD o <> " := " <> showD o1 <> " - " <> showD o2)
    $ addMullSub o o1 o2 (subl ebx eax)
translateBinOp o Mul o1 o2 =
  withComment
    "Binary Multiplication"
    (showD o <> " := " <> showD o1 <> " * " <> showD o2)
    $ do
      addMullSub o o1 o2 (imull ebx eax)
      commentD "End Binary Multiplication"
translateBinOp o Div o1 o2 =
  withComment
    "Binary Division"
    (showD o <> " := " <> showD o1 <> " / " <> showD o2)
    $ do
      divModPrefix o1 o2
      movslq eax rax
      movq rax o
translateBinOp o Mod o1 o2 =
  withComment "Binary Modulo" (showD o <> " := " <> showD o1 <> " % " <> showD o2) $ do
    divModPrefix o1 o2
    movslq edx rdx
    movq rdx o
translateBinOp o And o1 o2 =
  withComment "Binary And" (showD o <> " := " <> showD o1 <> " && " <> showD o2) $ do
    l2 <- getLabel
    l3 <- getLabel
    cmpl zeroD o1
    je l2
    cmpl zeroD o2
    je l2
    movl oneD eax
    jmp l3
    lab l2
    movl zeroD eax
    lab l3
    movzbq al rax
    movq rax o
translateBinOp o Or o1 o2 =
  withComment "Binary Or" (showD o <> " := " <> showD o1 <> " || " <> showD o2) $ do
    l2 <- getLabel
    l3 <- getLabel
    l4 <- getLabel
    cmpl zeroD o1
    jne l2
    cmpl zeroD o2
    je l3
    lab l2
    movl oneD eax
    jmp l4
    lab l3
    movl zeroD eax
    lab l4
    movzbq al rax
    movq rax o
translateBinOp o TAC.LT o1 o2 =
  withComment
    "Binary Less Than"
    (showD o <> " := " <> showD o1 <> " < " <> showD o2)
    $ logicalOPs o o1 o2 cmpl setl
translateBinOp o TAC.LTE o1 o2 =
  withComment
    "Binary Less Than or Equal"
    (showD o <> " := " <> showD o1 <> " <= " <> showD o2)
    $ logicalOPs o o1 o2 cmpl setle
translateBinOp o TAC.GT o1 o2 =
  withComment
    "Binary Greater Than"
    (showD o <> " := " <> showD o1 <> " > " <> showD o2)
    $ logicalOPs o o1 o2 cmpl setg
translateBinOp o TAC.GTE o1 o2 =
  withComment
    "Binary Greater Than or Equal"
    (showD o <> " := " <> showD o1 <> " >= " <> showD o2)
    $ logicalOPs o o1 o2 cmpl setge
translateBinOp o TAC.Eq o1 o2 =
  withComment "Binary Equal" (showD o <> " := " <> showD o1 <> " == " <> showD o2) $
    eqOps o o1 o2 cmpq sete
translateBinOp o TAC.Ineq o1 o2 =
  withComment
    "Binary Not Equal"
    (showD o <> " := " <> showD o1 <> " != " <> showD o2)
    $ eqOps o o1 o2 cmpq setne

-- | <var> := <unop> <var>
translateUnOp :: OperandQMM -> UnOp -> OperandQMM -> Analysis ()
translateUnOp o Not o' =
  withComment "Unary Not" (showD o <> " := ! " <> showD o') $ do
    movl o' eax
    cmpl zeroD eax
    sete al
    movzbq al rax
    movq rax o
translateUnOp o Negate o' =
  withComment "Unary Negate" (showD o <> " := - " <> showD o') $ do
    movl o' eax
    negl eax
    jo errOverflow
    movslq eax rax
    movq rax o

-- | Utility which calls appropriate print runtime library based on given type
translatePrint :: FType -> Analysis ()
translatePrint FInt = call printi
translatePrint FBool = call printb
translatePrint FChar = call printc
translatePrint FString = call prints
translatePrint _ = call printp

-- | Utility generates appropriate read instruction based on given type
translateRead :: OperandQMM -> FType -> Analysis ()
translateRead o FInt = do
  call readI
  movq argRet o
translateRead o FChar = do
  call readC
  movq argRet o
translateRead _ w =
  error $
    "Invalid type for read, only int and char are supported, got: " ++ show w

-- | Utility used to generate load instructions based on given variables for location and offset.
translateLoadM
  :: Var Integer -> Var Integer -> Var Integer -> FType -> Analysis ()
translateLoadM v1 v2 off t = do
  o1 <- allocate' v1
  o2 <- getOperand v2
  offset <- getOperand off
  movq o2 rax
  cmpq zeroQ rax
  je errNull
  movq offset rbx
  moveT (Mem (MTwoReg Rax Rbx)) o1 t
  where
    moveT :: OperandQMM -> OperandQMM -> FType -> Analysis ()
    moveT s d FChar = movzbq s rcx >> movq rcx d
    moveT s d FBool = movb s cl >> movb cl d
    moveT s d FInt = movslq s rcx >> movq rcx d
    moveT s d FString = movq s rcx >> movq rcx d
    moveT s d FPtr = movq s rcx >> movq rcx d

-- | > <var>[<offset>] := <var>
translateStore
  :: Var Integer -> Var Integer -> Var Integer -> FType -> Analysis ()
translateStore v1 off v2 t = do
  o1 <- getOperand v1
  offset <- getOperand off
  o2 <- getOperand v2
  movq o1 rax
  cmpq zeroQ rax
  je errNull
  movq offset rbx
  moveT o2 (Mem (MTwoReg Rax Rbx)) t
  where
    moveT :: OperandQMM -> OperandQMM -> FType -> Analysis ()
    moveT s d FChar = movb s cl >> movb cl d
    moveT s d FBool = movb s cl >> movb cl d
    moveT s d FInt = movl s ecx >> movl ecx d
    moveT s d FString = movq s rcx >> movq rcx d
    moveT s d FPtr = movq s rcx >> movq rcx d

jump :: (X86.Label -> Instruction) -> X86.Label -> Analysis ()
jump s l@(X86.R r) = do
  tellInstr (s l)
  useRuntimeFunc r
jump s l = tellInstr (s l)

jo = jump Jo

js = jump Js

jl = jump Jl

je = jump Je

jne = jump Jne

jmp = jump Jmp

call = jump X86.Call

arg1 = rdi

arg3 = rdx

arg4 = rcx

arg5 = r8

arg6 = r9

argRet = rax

section :: Analysis ()
section = tellInstr (Dir DirSection)

commentD :: DList Char -> Analysis ()
commentD = comment . D.toList

withComment :: DList Char -> DList Char -> Analysis () -> Analysis ()
withComment name semantics action = do
  commentD $ name <> ": " <> semantics
  action
  commentD $ "End " <> name

showD :: (Show a) => a -> DList Char
showD = D.fromList . show

useRuntimeFunc :: X86.Runtime -> Analysis ()
useRuntimeFunc r = tell (mempty, mempty, S.singleton r)
