{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

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
import Language.WACC.X86.Label (Label (..))
import qualified Language.WACC.X86.Label as X86
import Language.WACC.X86.Lib (runtimeLib)
import Language.WACC.X86.Memory (Memory (..))
import Language.WACC.X86.Operand (Operand (..), OperandQMM)
import Language.WACC.X86.OperandTH (genRegOperand)
import Language.WACC.X86.Register (Register (..))
import qualified Language.WACC.X86.Runtime as X86
import Language.WACC.X86.X86 as X86

$(genRegOperand)

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Program
translateProg p = D.toList $ preamble <> codeSeg <> dataSeg <> runtimeIs
  where
    runtimeIs :: DList Instruction
    runtimeIs = D.concat $ (runtimeLib !) <$> (S.toList runtime)
    runtime :: Set X86.Runtime -- Including all the dependencies
    runtime = S.unions $ S.map (runtimeDeps A.!) rs
    (_, (codeSeg, dataSeg, rs)) =
      execRWS (mapM_ translateFunc (M.elems p)) mempty (TransST B.empty S.empty 0 0)
    preamble :: DList Instruction
    preamble =
      [ Dir $ DirGlobl (S "main")
      , Dir DirSection
      , Dir DirText
      , Lab (S "main")
      ]

{- | When registers run out and values in the stack are
required for a computation, they put in this register.
A caller saved register.
-}
swapReg = R10

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

stackElemSize :: Integer
stackElemSize = 8

{- | Rbp points to the location just before the first stack variable of a frame
This means the last callee saved register pushed onto the stack

Assuming stack looks like this: (initial value of Rsp and constant value of Rbp)

--------------
Args: 1
.
.
Args: n     <-- Rsp , Rbp
--------------
Local Regs: 1
.
.
-}
translateFunc :: TACFunc Integer Integer -> Analysis ()
translateFunc (TACFunc l vs bs) = do
  resetState
  local
    (const bs)
    ( do
        tellInstr (Lab (I l)) -- label for the function = label of the first block
        pushq rbp
        movq rsp rbp -- set the stack base pointer
        mapM_ setupStackArgs (zip vs [0 ..])
        translateBlocks l startBlock -- Translate main part of code
        movq rbp rsp -- restore stack pointer
        popq rbp
        unless (l == 0) (tellInstr X86.Ret) -- return only if not main method
        -- assigning arg vars to stack
    )
  where
    startBlock = bs ! l
    setupStackArgs :: (Var Integer, Integer) -> Analysis ()
    setupStackArgs (v, n) = modify (bindVarToLoc v (Mem (MRegI (stackElemSize * n + 16) Rbp)))
    resetState :: Analysis ()
    resetState = modify (\x -> x {alloc = B.empty, translated = S.empty, stackVarNum = 0})

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
    lenStack = toInt64 $ 8 * (\x -> if (even x) then x else x + 1) (length is)
    toInt64 :: Int -> Int64
    toInt64 = fromIntegral . toInteger

-- | Mark the block as translated, so its not re-translated
setTranslated :: Integer -> TransST -> TransST
setTranslated l x@(TransST {translated}) = x {translated = S.insert l translated}

mapLab :: Integer -> X86.Label
mapLab = I

tellInstr :: Instruction -> Analysis ()
tellInstr i = tell (D.singleton i, mempty, mempty)

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
      then tellInstr (Jmp (mapLab l))
      else tellInstr (Lab (mapLab l)) >> translateBlocks l nextBlock
    )
translateNext (CJump v l1 l2) = do
  operand <- gets ((B.! v) . alloc)
  tellInstr (Cmpq (Imm (IntLitQ 0)) operand)
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
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
  tellInstr X86.Ret
translateNext (TAC.Exit x) = do
  operand <- gets ((B.! x) . alloc)
  tellInstr (Movl operand (Reg Edi))
  call (R X86.Exit)

bindVarToLoc :: Var Integer -> OperandQMM -> TransST -> TransST
bindVarToLoc v o x@(TransST {alloc}) = x {alloc = B.insert v o alloc}

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

getLabel :: Analysis X86.Label
getLabel = do
  n <- gets labelCounter
  modify (\x -> x {labelCounter = n + 1})
  return (S (".TAC_L" ++ show n))

getOperand :: Var Integer -> Analysis OperandQMM
getOperand v = gets ((B.! v) . alloc)

-------------------------------------

-- | Translate a TAC statement to X86 Instructions
translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr v1 v2 op v3) = do
  comment $
    "BinInstr: " ++ show v1 ++ " := " ++ show v2 ++ " " ++ show op ++ " " ++ show v3
  operand <- allocate' v1
  operand1 <- getOperand v2
  operand2 <- getOperand v3
  translateBinOp operand op operand1 operand2
  comment "End BinInstr"
translateTAC (UnInstr v1 op v2) = do
  comment $ "UnInstr: " ++ show v1 ++ " := " ++ show op ++ " " ++ show v2
  operand <- allocate' v1
  operand' <- getOperand v2
  translateUnOp operand op operand'
  comment "End UnInstr"
translateTAC (Store v1 off v2 w) = do
  comment $ "Store: " ++ show v1 ++ " := " ++ show v2 ++ "[" ++ show off ++ "]"
  translateStore v1 off v2 w
  comment "End Store"
translateTAC (LoadCI v i) = do
  comment $ "LoadCI: " ++ show v ++ " := " ++ show i
  operand <- allocate' v
  movq (Imm (IntLitQ $ fromIntegral i)) operand
  comment "End LoadCI"
translateTAC (LoadCS v s) = do
  comment $ "LoadCS: " ++ show v ++ " := " ++ show s
  o <- allocate' v
  l <- getLabel
  tellString l s -- store the string in the data section
  leaq (Mem (MRegL l Rip)) rax
  movq rax o
  comment "End LoadCS"
translateTAC (LoadM v1 v2 off w) = do
  comment $ "LoadM: " ++ show v1 ++ " := " ++ show v2 ++ "[" ++ show off ++ "]"
  translateLoadM v1 v2 off w
  comment "End LoadM"
translateTAC (TAC.Call v1 l vs) = do
  comment $ "Call: " ++ show v1 ++ " := call " ++ show l ++ "(" ++ show vs ++ ")"
  -- push all registers on to stack
  o <- allocate' v1
  os <- mapM getOperand vs
  mapM_ pushq (reverse os)
  -- call the function
  call (I l)
  -- pop all registers off the stack
  mapM_ popq os
  movq argRet o
  comment "End Call"
translateTAC (Print v w) = do
  comment $ "Print: print " ++ show v
  operand <- getOperand v
  movq operand arg1
  translatePrint w
  comment "End Print"
translateTAC (TAC.PrintLn v w) = do
  comment $ "PrintLn: println " ++ show v
  translateTAC (Print v w)
  call printLn
  comment "End PrintLn"
{-
translateTAC (TAC.Exit v) = do
  comment $ "Exit: exit " ++ show v
  operand <- getOperand v
  movq operand arg1
  call (R X86.Exit)
  comment "End Exit"
-}
translateTAC (Read v w) = do
  comment $ "Read: " ++ show v ++ " := read"
  operand <- allocate' v
  movq operand rdi
  translateRead operand w
  comment "End Read"
translateTAC (TAC.Malloc lv rv) = do
  comment $ "Malloc: " ++ show lv ++ " := malloc " ++ show rv
  operand' <- getOperand rv
  movq operand' arg1
  call (R X86.Malloc)
  operand <- allocate' lv
  movq argRet operand
  comment "End Malloc"
translateTAC (TAC.Free v) = do
  comment $ "Free: free " ++ show v
  operand <- getOperand v
  movq operand arg1
  cmpq (Imm (IntLitQ 0)) arg1
  je errNull
  call (R X86.Free)
  comment "End Free"
-- > assert 0 <= <var> < <max>
translateTAC (TAC.CheckBounds v vm) = do
  comment $ "CheckBounds: assert 0 <= " ++ show v ++ " < " ++ show vm
  l3 <- getLabel
  l4 <- getLabel
  o <- getOperand v
  om <- getOperand vm
  cmpl (Imm (IntLitD 0)) o
  js l3
  movl o eax
  cmpl om eax
  jl l4
  lab l3
  movl om edi
  movl o esi
  call (R X86.ErrOutOfBounds)
  lab l4
translateTAC (TAC.Move v1 v2) = do
  comment $ "Move: " ++ show v1 ++ " := " ++ show v2
  operand1 <- allocate' v1
  operand2 <- getOperand v2
  movq operand2 rax
  movq rax operand1
  comment "End Move"

tellString :: X86.Label -> String -> Analysis ()
tellString l s =
  tell
    ( mempty
    ,
      [ Dir DirSection
      , Dir $ DirInt (fromIntegral $ length s)
      , Lab l
      , Dir $ DirAsciz s
      , Dir DirText
      ]
    , mempty
    )

-------------------------------------

{- | Translate a binary operation
| <o> := <o1> <binop> <o2>
-}
translateBinOp
  :: OperandQMM -> BinOp -> OperandQMM -> OperandQMM -> Analysis ()
translateBinOp o Add o1 o2 = do
  comment $ "Binary Addition: " ++ show o ++ " := " ++ show o1 ++ " + " ++ show o2
  movl o1 eax
  movl o2 ebx
  addl ebx eax
  jo errOverflow
  movslq eax rax
  movq rax o
  comment "End Binary Addition"
translateBinOp o PtrAdd o1 o2 = do
  comment $
    "Binary Pointer Addition: " ++ show o ++ " := " ++ show o1 ++ " + " ++ show o2
  movq o1 rax
  movq o2 rbx
  addq rbx rax
  jo errOverflow
  movq rax o
  comment "End Pointer Binary Addition"
translateBinOp o Sub o1 o2 = do
  comment $
    "Binary Subtraction: " ++ show o ++ " := " ++ show o1 ++ " - " ++ show o2
  movl o1 eax
  movl o2 ebx
  subl ebx eax
  jo errOverflow
  movslq eax rax
  movq rax o
  comment "End Binary Subtraction"
translateBinOp o Mul o1 o2 = do
  comment $
    "Binary Multiplication: " ++ show o ++ " := " ++ show o1 ++ " * " ++ show o2
  movl o1 eax
  movl o2 ebx
  imull ebx eax
  jo errOverflow
  movslq eax rax
  movq rax o
  comment "End Binary Multiplication"
translateBinOp o Div o1 o2 = do
  comment $ "Binary Division: " ++ show o ++ " := " ++ show o1 ++ " / " ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl (Imm (IntLitD 0)) ebx -- check for division by zero
  je errDivByZero
  cltd -- sign extend eax into edx
  idivl ebx -- divide edx:eax by ebx
  movslq eax rax
  movq rax o
  comment "End Binary Division"
translateBinOp o Mod o1 o2 = do
  comment $ "Binary Modulo: " ++ show o ++ " := " ++ show o1 ++ " % " ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl (Imm (IntLitD 0)) ebx -- check for division by zero
  je errDivByZero
  cltd -- sign extend eax into edx
  idivl ebx -- divide edx:eax by ebx
  movslq edx rdx
  movq rdx o
  comment "End Binary Modulo"
translateBinOp o And o1 o2 = do
  comment $ "Binary And: " ++ show o ++ " := " ++ show o1 ++ " && " ++ show o2
  l2 <- getLabel
  l3 <- getLabel
  cmpl (Imm (IntLitD 0)) o1
  je l2
  cmpl (Imm (IntLitD 0)) o2
  je l2
  movl (Imm (IntLitD 1)) eax
  jmp l3
  lab l2
  movl (Imm (IntLitD 0)) eax
  lab l3
  movzbq al rax
  movq rax o
  comment "End Binary And"
{-
  cmpl $0, -4(%rbp)
  jne .L2
  cmpl $0, -8(%rbp)
  je .L3
.L2:
  movl $1, %eax
  jmp .L4
.L3:
  movl $0, %eax
.L4:
  movzbl %al, %eax
  movl %eax, -12(%rbp)
-}
translateBinOp o Or o1 o2 = do
  comment $ "Binary Or: " ++ show o ++ " := " ++ show o1 ++ " || " ++ show o2
  l2 <- getLabel
  l3 <- getLabel
  l4 <- getLabel
  cmpl (Imm (IntLitD 0)) o1
  jne l2
  cmpl (Imm (IntLitD 0)) o2
  je l3
  lab l2
  movl (Imm (IntLitD 1)) eax
  jmp l4
  lab l3
  movl (Imm (IntLitD 0)) eax
  lab l4
  movzbq al rax
  movq rax o
  comment "End Binary Or"
translateBinOp o TAC.LT o1 o2 = do
  comment $
    "Binary Less Than: " ++ show o ++ " := " ++ show o1 ++ " < " ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setl al -- set al to 1 if %eax < %ebx
  movzbq al rax
  movq rax o -- %o := %al
  comment "End Binary Less Than"
translateBinOp o TAC.LTE o1 o2 = do
  comment $
    "Binary Less Than or Equal: "
      ++ show o
      ++ " := "
      ++ show o1
      ++ " <= "
      ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setle al -- set al to 1 if %eax <= %ebx
  movzbq al rax
  movq rax o -- %o := %al
  comment "End Binary Less Than or Equal"
translateBinOp o TAC.GT o1 o2 = do
  comment $
    "Binary Greater Than: " ++ show o ++ " := " ++ show o1 ++ " > " ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setg al -- set al to 1 if %eax > %ebx
  movzbq al rax
  movq rax o -- %o := %al
  comment "End Binary Greater Than"
translateBinOp o TAC.GTE o1 o2 = do
  comment $
    "Binary Greater Than or Equal: "
      ++ show o
      ++ " := "
      ++ show o1
      ++ " >= "
      ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setge al -- set al to 1 if %eax >= %ebx
  movzbq al rax
  movq rax o -- %o := %al
  comment "End Binary Greater Than or Equal"
translateBinOp o TAC.Eq o1 o2 = do
  comment $ "Binary Equal: " ++ show o ++ " := " ++ show o1 ++ " == " ++ show o2
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  cmpq rbx rax -- compare %ebx and %eax
  sete al -- set al to 1 if %eax == %ebx
  movzbq al rax
  movq rax o -- %o := %al
  comment "End Binary Equal"
translateBinOp o TAC.Ineq o1 o2 = do
  comment $
    "Binary Not Equal: " ++ show o ++ " := " ++ show o1 ++ " != " ++ show o2
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  cmpq rbx rax -- compare %ebx and %eax
  setne al -- set al to 1 if %eax != %ebx
  movzbq al rax
  movq rax o -- %o := %al
  comment "End Binary Not Equal"

-- | <var> := <unop> <var>
translateUnOp :: OperandQMM -> UnOp -> OperandQMM -> Analysis ()
translateUnOp o Not o' = do
  comment $ "Unary Not: " ++ show o ++ " := ! " ++ show o'
  movl o' eax
  cmpl (Imm (IntLitD 0)) eax
  sete al
  movzbq al rax
  movq rax o
  comment "End Unary Not"
translateUnOp o Negate o' = do
  comment $ "Unary Negate: " ++ show o ++ " := - " ++ show o'
  movl o' eax
  negl eax
  jo errOverflow
  movslq eax rax
  movq rax o
  comment "End Unary Negate"

translatePrint :: FType -> Analysis ()
translatePrint FInt = call printi
translatePrint FBool = call printb
translatePrint FChar = call printc
translatePrint FString = call prints
translatePrint _ = call printp

translateRead :: OperandQMM -> FType -> Analysis ()
translateRead o FInt = do
  call (R X86.ReadI)
  movq argRet o
translateRead o FChar = do
  call (R X86.ReadC)
  movq argRet o
translateRead _ w =
  error $
    "Invalid type for read, only int and char are supported, got: " ++ show w

translateLoadM
  :: Var Integer -> Var Integer -> Var Integer -> FType -> Analysis ()
translateLoadM v1 v2 off t = do
  o1 <- allocate' v1
  o2 <- getOperand v2
  offset <- getOperand off
  movq o2 rax
  cmpq (Imm (IntLitQ 0)) rax
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
  cmpq (Imm (IntLitQ 0)) rax
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

leaq o1 o2 = tellInstr (Leaq o1 o2)

mov m o r = tellInstr (m o r)

movb = mov Movb

movl = mov Movl

movq = mov Movq

movslq o r = tellInstr (Movslq o r)

movzbq o r = tellInstr (Movzbq o r)

addq o1 o2 = tellInstr (Addq o1 o2)

andq o1 o2 = tellInstr (Andq o1 o2)

addl o1 o2 = tellInstr (Addl o1 o2)

subq o1 o2 = tellInstr (Subq o1 o2)

subl o1 o2 = tellInstr (Subl o1 o2)

imull o1 o2 = tellInstr (Imull o1 o2)

idivl o = tellInstr (Idivl o)

cmpl o1 o2 = tellInstr (Cmpl o1 o2)

cmpq o1 o2 = tellInstr (Cmpq o1 o2)

pushq o = tellInstr (Pushq o)

popq o = tellInstr (Popq o)

j :: (X86.Label -> Instruction) -> X86.Label -> Analysis ()
j s l@(X86.R r) = do
  tellInstr (s l)
  useRuntimeFunc r
j s l = tellInstr (s l)

jo = j Jo

js = j Js

jl = j Jl

je = j Je

jne = j Jne

jmp = j Jmp

cltd :: Analysis ()
cltd = tellInstr Cltd

set :: (a -> Instruction) -> a -> Analysis ()
set s r = tellInstr (s r)

sete = set Sete

setne = set Setne

setl = set Setl

setle = set Setle

setg = set Setg

setge = set Setge

negl o = tellInstr (Negl o)

call = j X86.Call

arg1 = Reg Rdi

arg3 = Reg Rdx

arg4 = Reg Rcx

arg5 = Reg R8

arg6 = Reg R9

argRet = Reg Rax

printi :: X86.Label
printi = R X86.PrintI

printc :: X86.Label
printc = R X86.PrintC

printb :: X86.Label
printb = R X86.PrintB

printp :: X86.Label
printp = R X86.PrintP

prints :: X86.Label
prints = R X86.PrintS

printLn :: X86.Label
printLn = R X86.PrintLn

errNull :: X86.Label
errNull = R X86.ErrNull

errOverflow :: X86.Label
errOverflow = R X86.ErrOverflow

errDivByZero :: X86.Label
errDivByZero = R X86.ErrDivByZero

lab :: X86.Label -> Analysis ()
lab = tellInstr . Lab

section :: Analysis ()
section = tellInstr (Dir DirSection)

comment :: String -> Analysis ()
comment s = tellInstr (Comment s)

useRuntimeFunc :: X86.Runtime -> Analysis ()
useRuntimeFunc r = tell (mempty, mempty, S.singleton r)
