{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.WACC.X86.Translate where

import Control.Monad (unless)
import Control.Monad.RWS
  ( RWS
  , asks
  , execRWS
  , gets
  , modify
  , tell
  )
import qualified Data.Array as A
import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.WACC.TAC.FType
  ( FType
  , sizeOf
  , pattern FBool
  , pattern FChar
  , pattern FInt
  , pattern FString
  )
import Language.WACC.TAC.TAC
  ( BasicBlock (BasicBlock)
  , BinOp (Add, And, Div, Mod, Mul, Or, Sub)
  , Jump (CJump, Jump)
  , Label (Label)
  , TAC (BinInstr, LoadCI, LoadCS, LoadM, Print, Read, Store, UnInstr)
  , TACFunc (..)
  , TACProgram
  , UnOp (..)
  , Var
  )
import qualified Language.WACC.TAC.TAC as TAC
import Language.WACC.X86.Runtime (runtimeLib)
import Language.WACC.X86.X86
  ( Directive (..)
  , Instr (..)
  , Label (..)
  , Memory (..)
  , Operand (..)
  , Prog
  , Register (..)
  , Runtime (..)
  , runtimeDeps
  )
import qualified Language.WACC.X86.X86 as X86

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg p = D.toList $ preamble <> D.concat is <> D.concat runtime
  where
    runtime :: [DList Instr]
    runtime = (runtimeLib !) <$> S.toList (S.unions runtimeLs)
    (runtimeLs, is) = unzip $ map translateFunc (M.elems p)
    preamble :: DList Instr
    preamble =
      D.fromList
        [ Dir $ DirGlobl (S "main")
        , Dir DirSection
        , Dir DirText
        , Lab (S "main")
        ]

{- | When registers run out and values in the stack are
required for a computation, they put in this register.
A caller saved register.
-}
swapReg :: Register
swapReg = R10

-- | The state of RWS monad for translation of each function
data TransST = TransST
  { alloc :: Bimap (Var Integer) Operand
  -- ^ bijection between variables in TAC to register/memory in X86
  , translated :: Set (TAC.Label Integer)
  -- ^ Set of basic blocks that have been translated
  , stackVarNum :: Integer
  -- ^ The number of stack variables used so far (not incl. saving callee saved reg)
  , runtimeFns :: Set Runtime
  -- ^ Set of runtime functions which need to be included
  , labelCounter :: Integer
  -- ^ Counter for generating unique labels
  }

-- | Reader maps labels to basic blocks. Writer holds output X86 program
type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    (DList Instr)
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
translateFunc :: TACFunc Integer Integer -> (Set Runtime, DList Instr)
translateFunc (TACFunc l vs bs) = (runtimeFns st, is)
  where
    (st, is) =
      execRWS
        funcRWS
        bs
        (TransST B.empty S.empty 0 S.empty 0) -- Not empty regs list
    startBlock = bs M.! l

    funcRWS = do
      tellInstr (Lab (I l)) -- label for the function = label of the first block
      movq rsp rbp -- set the stack base pointer
      -- if more than 6 arguments, subtract from rsp (more than the number of callee saved) to get stack position
      mapM_ setupStackArgs (zip vs [0 ..])
      translateBlocks (TAC.Label l) startBlock -- Translate main part of code
      movq rbp rsp -- restore stack pointer
      unless (l == 0) (tellInstr X86.Ret) -- return only if not main method
      -- assigning arg vars to stack
    setupStackArgs :: (Var Integer, Integer) -> Analysis ()
    setupStackArgs (v, n) = modify (bindVarToLoc v (Mem (MRegI (stackElemSize * n) Rbp)))

{- | translate each statement of the block. then figure out which block to go to
labels are printed right before this function is called
-}
translateBlocks
  :: TAC.Label Integer
  -> BasicBlock Integer Integer
  -> Analysis ()
translateBlocks l (BasicBlock is next) = do
  modify (setTranslated l) -- include label in translated set
  mapM_ translateTAC is
  translateNext next

-- | Mark the block as translated, so its not re-translated
setTranslated :: TAC.Label Integer -> TransST -> TransST
setTranslated l x@(TransST {translated}) = x {translated = S.insert l translated}

mapLab :: TAC.Label Integer -> X86.Label
mapLab (Label x) = I x

tellInstr :: Instr -> Analysis ()
tellInstr = tell . D.singleton

isTranslated :: TAC.Label Integer -> Analysis Bool
isTranslated l = gets (S.member l . translated)

{- | Flow control: For CJump translate l2 first then translate l1 after returning from the recursive call
This generates weird (but correct) assembly with else first then if. The if block appears roughly at the
end of the recursive call.
-}
translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump l1@(Label n)) = do
  nextBlock <- asks (! n)
  t <- isTranslated l1
  ( if t
      then tellInstr (Jmp (mapLab l1))
      else (tellInstr (Lab (mapLab l1)) >> translateBlocks l1 nextBlock)
    )
translateNext (CJump v l1 l2) = do
  operand <- gets ((B.! v) . alloc)
  tellInstr (X86.Cmpq operand (Imm 0))
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
  translateNext (Jump l2)
  t <- isTranslated l1
  (if t then pure () else translateNext (Jump l1))
translateNext (TAC.Ret var) = do
  retVal <- gets ((B.! var) . alloc)
  tellInstr (Movl retVal (Reg Rax))

bindVarToLoc :: Var Integer -> Operand -> TransST -> TransST
bindVarToLoc v o x@(TransST {alloc}) = x {alloc = B.insert v o alloc}

allocate :: Var Integer -> Analysis Operand
allocate v = do
  -- increase the stackVarNum
  modify (\x@(TransST {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
  -- insert the variable into the allocation map
  stackAddr <- gets ((* (-8)) . stackVarNum)
  modify (bindVarToLoc v (Mem (MRegI stackAddr Rbp)))
  return (Mem (MRegI stackAddr Rbp))

-- | Sanity check. Variable must not already be allocated in three address code
allocate' :: Var Integer -> Analysis Operand
allocate' v = do
  alloc' <- gets (B.lookup v . alloc)
  case alloc' of
    Just _ -> error "allocate': Variable already allocated"
    Nothing -> allocate v

getLabel :: Analysis X86.Label
getLabel = do
  n <- gets labelCounter
  modify (\x -> x {labelCounter = n + 1})
  return (S (".TAC_L" ++ show n))

saveRegister :: [Register] -> Analysis ()
saveRegister = mapM_ (tellInstr . Pushq . Reg)

restoreRegister :: [Register] -> Analysis ()
restoreRegister = mapM_ (tellInstr . Popq . Reg)

getOperand :: Var Integer -> Analysis Operand
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
  translateStore v1 off v2 (sizeOf w)
  comment "End Store"
translateTAC (LoadCI v i) = do
  comment $ "LoadCI: " ++ show v ++ " := " ++ show i
  operand <- allocate' v
  movq (Imm (fromIntegral i)) operand
  comment "End LoadCI"
translateTAC (LoadCS v s) = do
  comment $ "LoadCS: " ++ show v ++ " := " ++ show s
  o <- allocate' v
  l <- getLabel
  section
  int (fromIntegral $ length s)
  lab l
  asciz s
  text
  leaq (Mem (MRegL l Rip)) rax
  movq rax o
  comment "End LoadCS"
translateTAC (LoadM v1 v2 off w) = do
  comment $ "LoadM: " ++ show v1 ++ " := " ++ show v2 ++ "[" ++ show off ++ "]"
  translateLoadM v1 v2 off (sizeOf w)
  comment "End LoadM"
translateTAC (TAC.Call v1 (Label l) vs) = do
  comment $ "Call: " ++ show v1 ++ " := call " ++ show l ++ "(" ++ show vs ++ ")"
  -- push all registers on to stack
  os <- mapM getOperand vs
  mapM_ pushq (reverse os)
  -- call the function
  call (I l)
  -- pop all registers off the stack
  mapM_ popq os
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
  translateRead operand w
  comment "End Read"
translateTAC (TAC.Malloc lv rv) = do
  comment $ "Malloc: " ++ show lv ++ " := malloc " ++ show rv
  operand <- allocate' lv
  operand' <- getOperand rv
  movq operand' arg1
  call (R X86.Malloc)
  movq argRet operand
  comment "End Malloc"
translateTAC (TAC.Free v) = do
  comment $ "Free: free " ++ show v
  operand <- getOperand v
  movq operand arg1
  call (R X86.Free)
  comment "End Free"
translateTAC (TAC.CheckBounds {}) = undefined

-------------------------------------

{- | Translate a binary operation
| <o> := <o1> <binop> <o2>
-}
translateBinOp :: Operand -> BinOp -> Operand -> Operand -> Analysis ()
translateBinOp o Add o1 o2 = do
  comment $ "Binary Addition: " ++ show o ++ " := " ++ show o1 ++ " + " ++ show o2
  movl o1 eax
  movl o2 ebx
  addl ebx eax
  jo errOverflow
  movl ebx o
  comment "End Binary Addition"
translateBinOp o Sub o1 o2 = do
  comment $
    "Binary Subtraction: " ++ show o ++ " := " ++ show o1 ++ " - " ++ show o2
  movl o1 eax
  movl o2 ebx
  subl ebx eax
  jo errOverflow
  movl ebx o
  comment "End Binary Subtraction"
translateBinOp o Mul o1 o2 = do
  comment $
    "Binary Multiplication: " ++ show o ++ " := " ++ show o1 ++ " * " ++ show o2
  movl o1 eax
  movl o2 ebx
  imull ebx eax
  jo errOverflow
  movl ebx o
  comment "End Binary Multiplication"
translateBinOp o Div o1 o2 = do
  comment $ "Binary Division: " ++ show o ++ " := " ++ show o1 ++ " / " ++ show o2
  movl o1 eax -- %eax := o1
  cmpl (Imm 0) eax -- check for division by zero
  je errDivByZero
  cltd -- sign extend eax into edx
  movl o2 ebx -- %ebx := o2
  idivl ebx -- divide edx:eax by ebx
  movl eax o -- %o := eax
  comment "End Binary Division"
translateBinOp o Mod o1 o2 = do
  comment $ "Binary Modulo: " ++ show o ++ " := " ++ show o1 ++ " % " ++ show o2
  movl o1 eax -- %eax := o1
  cmpl (Imm 0) eax -- check for division by zero
  je errDivByZero
  cltd -- sign extend eax into edx
  movl o2 ebx -- %ebx := o2
  idivl ebx -- divide edx:eax by ebx
  movl edx o -- %o := edx
  comment "End Binary Modulo"
translateBinOp o And o1 o2 = do
  comment $ "Binary And: " ++ show o ++ " := " ++ show o1 ++ " && " ++ show o2
  l2 <- getLabel
  l3 <- getLabel
  cmpl (Imm 0) o1
  je l2
  cmpl (Imm 0) o2
  je l2
  movl (Imm 1) eax
  jmp l3
  lab l2
  movl (Imm 0) eax
  lab l3
  movzbl al eax
  movl eax o
  comment "End Binary And"
translateBinOp o Or o1 o2 = do
  comment $ "Binary Or: " ++ show o ++ " := " ++ show o1 ++ " || " ++ show o2
  l2 <- getLabel
  l3 <- getLabel
  l4 <- getLabel
  cmpl (Imm 0) o1
  jne l2
  cmpl (Imm 0) o2
  je l3
  lab l2
  movl (Imm 1) eax
  jmp l4
  lab l3
  movl (Imm 0) eax
  lab l4
  movzbl al eax
  movl eax o
  comment "End Binary Or"
translateBinOp o TAC.LT o1 o2 = do
  comment $
    "Binary Less Than: " ++ show o ++ " := " ++ show o1 ++ " < " ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setl al -- set al to 1 if %eax < %ebx
  movzbl al o -- %o := %al
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
  movzbl al o -- %o := %al
  comment "End Binary Less Than or Equal"
translateBinOp o TAC.GT o1 o2 = do
  comment $
    "Binary Greater Than: " ++ show o ++ " := " ++ show o1 ++ " > " ++ show o2
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setg al -- set al to 1 if %eax > %ebx
  movzbl al o -- %o := %al
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
  movzbl al o -- %o := %al
  comment "End Binary Greater Than or Equal"
translateBinOp o TAC.Eq o1 o2 = do
  comment $ "Binary Equal: " ++ show o ++ " := " ++ show o1 ++ " == " ++ show o2
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  cmpq rbx rax -- compare %ebx and %eax
  sete al -- set al to 1 if %eax == %ebx
  movzbl al o -- %o := %al
  comment "End Binary Equal"
translateBinOp o TAC.Ineq o1 o2 = do
  comment $
    "Binary Not Equal: " ++ show o ++ " := " ++ show o1 ++ " != " ++ show o2
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  cmpq rbx rax -- compare %ebx and %eax
  setne al -- set al to 1 if %eax != %ebx
  movzbl al o -- %o := %al
  comment "End Binary Not Equal"

-- | <var> := <unop> <var>
translateUnOp :: Operand -> UnOp -> Operand -> Analysis ()
translateUnOp o Not o' = do
  comment $ "Unary Not: " ++ show o ++ " := ! " ++ show o'
  movl o' eax
  cmpl (Imm 0) eax
  sete al
  movzbl al o
  comment "End Unary Not"
translateUnOp o Negate o' = do
  comment $ "Unary Negate: " ++ show o ++ " := - " ++ show o'
  movl o' eax
  negl eax
  movl eax o
  comment "End Unary Negate"

translatePrint :: FType -> Analysis ()
translatePrint FInt = call printi
translatePrint FBool = call printb
translatePrint FChar = call printc
translatePrint FString = call prints
translatePrint _ = call printp

translateRead :: Operand -> FType -> Analysis ()
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
  :: Var Integer -> Var Integer -> Var Integer -> Int -> Analysis ()
translateLoadM v1 v2 off s = do
  -- array ptr passed in R9, index in R10, and return into R9
  o1 <- allocate' v1
  o2 <- getOperand v2
  offset <- getOperand off
  movq o2 r9
  movq offset r10
  call (arrayLoad s)
  movq r9 o1

translateStore
  :: Var Integer -> Var Integer -> Var Integer -> Int -> Analysis ()
translateStore v1 off v2 s = do
  -- array ptr passed in R9, index in R10, and value in R11
  o1 <- getOperand v1
  offset <- getOperand off
  o2 <- getOperand v2
  movq o1 r9
  movq offset r10
  movq o2 r11
  call (arrayStore s)

rbp :: Operand
rbp = Reg Rbp

rsp :: Operand
rsp = Reg Rsp

al :: Operand
al = Reg Rax

r8 :: Operand
r8 = Reg R8

r9 :: Operand
r9 = Reg R9

r10 :: Operand
r10 = Reg R10

r11 :: Operand
r11 = Reg R11

rax :: Operand
rax = Reg Rax

rbx :: Operand
rbx = Reg Rbx

eax :: Operand
eax = Reg Rax

ebx :: Operand
ebx = Reg Rbx

ecx :: Operand
ecx = Reg Rcx

edx :: Operand
edx = Reg Rdx

leaq :: Operand -> Operand -> Analysis ()
leaq o1 o2 = tellInstr (Leaq o1 o2)

mov :: (a -> b -> Instr) -> a -> b -> Analysis ()
mov m o r = tellInstr (m o r)

movl :: Operand -> Operand -> Analysis ()
movl = mov Movl

movq :: Operand -> Operand -> Analysis ()
movq = mov Movq

movzbl :: Operand -> Operand -> Analysis ()
movzbl o r = tellInstr (Movzbl o r)

addl :: Operand -> Operand -> Analysis ()
addl o1 o2 = tellInstr (Addl o1 o2)

subl :: Operand -> Operand -> Analysis ()
subl o1 o2 = tellInstr (Subl o1 o2)

imull :: Operand -> Operand -> Analysis ()
imull o1 o2 = tellInstr (Imull o1 o2)

idivl :: Operand -> Analysis ()
idivl o = tellInstr (Idivl o)

cmpl :: Operand -> Operand -> Analysis ()
cmpl o1 o2 = tellInstr (Cmpl o1 o2)

cmpq :: Operand -> Operand -> Analysis ()
cmpq o1 o2 = tellInstr (Cmpq o1 o2)

pushq :: Operand -> Analysis ()
pushq o = tellInstr (Pushq o)

popq :: Operand -> Analysis ()
popq o = tellInstr (Popq o)

j :: (X86.Label -> Instr) -> X86.Label -> Analysis ()
j s l@(R r) = do
  tellInstr (s l)
  useRuntimeFunc r
j s l = tellInstr (s l)

jo :: X86.Label -> Analysis ()
jo = j Jo

je :: X86.Label -> Analysis ()
je = j Je

jne :: X86.Label -> Analysis ()
jne = j Jne

jmp :: X86.Label -> Analysis ()
jmp = j Jmp

cltd :: Analysis ()
cltd = tellInstr Cltd

set :: (a -> Instr) -> a -> Analysis ()
set s r = tellInstr (s r)

sete :: Operand -> Analysis ()
sete = set Sete

setne :: Operand -> Analysis ()
setne = set Setne

setl :: Operand -> Analysis ()
setl = set Setl

setle :: Operand -> Analysis ()
setle = set Setle

setg :: Operand -> Analysis ()
setg = set Setg

setge :: Operand -> Analysis ()
setge = set Setge

negl :: Operand -> Analysis ()
negl o = tellInstr (Negl o)

call :: X86.Label -> Analysis ()
call = j X86.Call

arg1, argRet :: Operand
arg1 = Reg Rdi
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

errOverflow :: X86.Label
errOverflow = R X86.ErrOverflow

errDivByZero :: X86.Label
errDivByZero = R X86.ErrDivByZero

lab :: X86.Label -> Analysis ()
lab = tellInstr . Lab

section :: Analysis ()
section = tellInstr (Dir DirSection)

int :: Integer -> Analysis ()
int x = tellInstr (Dir (DirInt x))

asciz :: String -> Analysis ()
asciz s = tellInstr (Dir (DirAsciz s))

text :: Analysis ()
text = tellInstr (Dir DirText)

comment :: String -> Analysis ()
comment s = tellInstr (Comment s)

arrayLoad :: (Eq a, Num a) => a -> X86.Label
arrayLoad 1 = R ArrLoad1
arrayLoad 4 = R ArrLoad4
arrayLoad 8 = R ArrLoad8
arrayLoad _ = error "Invalid size for array load"

arrayStore :: (Eq a, Num a) => a -> X86.Label
arrayStore 1 = R ArrStore1
arrayStore 4 = R ArrStore4
arrayStore 8 = R ArrStore8
arrayStore _ = error "Invalid size for array store"

useRuntimeFunc :: Runtime -> Analysis ()
useRuntimeFunc r = modify (\x -> x {runtimeFns = S.union (runtimeDeps A.! r) (runtimeFns x)})
