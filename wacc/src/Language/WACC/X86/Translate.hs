{-# LANGUAGE NamedFieldPuns #-}

module Language.WACC.X86.Translate where

import Control.Monad.RWS
  ( RWS
  , asks
  , execRWS
  , get
  , gets
  , put
  , tell
  )
import Data.Bimap hiding (map, (!))
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.List ((\\))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.WACC.AST.WType
  ( WType (WBool, WChar, WInt, WString)
  )
import Language.WACC.TAC.TAC
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
  , argRegs
  , callee
  , caller
  )
import qualified Language.WACC.X86.X86 as X86

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg p = D.toList $ preamble `D.append` D.concat is `D.append` D.concat runtime
  where
    runtime :: [DList Instr]
    runtime = (runtimeLib !) <$> S.toList (S.unions runtimeLs)
    (runtimeLs, is) = unzip $ map translateFunc (M.elems p)
    preamble :: DList Instr
    preamble =
      D.fromList
        [ Dir $ DirGlobl (S "main")
        , Dir DirSection
        , Dir DirRodata
        , Dir DirText
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
  , freeRegs :: [Register]
  -- ^ Unused registers
  , labelCounter :: Integer
  -- ^ Counter for generating unique labels
  }

-- | Reader maps labels to basic blocks. Writer holds output X86 program
type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    (DList Instr)
    TransST

{- | Rbp points to the location just before the first stack variable of a frame
This means the last callee saved register pushed onto the stack

Assuming stack looks like this: (initial value of Rsp and constant value of Rbp)

--------------
Extra Arg: 1
.
.
Extra Arg: n     <-- Rsp
--------------
Callee Saved: 1
.
.
Callee Saved: m  <-- Rbp
-------------
Local Regs: 1
.
.
-}
translateFunc :: Func Integer Integer -> (Set Runtime, DList Instr)
translateFunc (Func l vs bs) = (runtimeFns st, is)
  where
    (st, is) =
      execRWS
        funcRWS
        bs
        (TransST B.empty S.empty 0 S.empty ((callee \\ [Rbp]) ++ caller) 0) -- Not empty regs list
    (nStart, startBlock) = M.findMin bs

    funcRWS = do
      mapM_ (tellInstr . Pushq . Reg) callee -- callee saving registers
      tellInstr (Movq (Reg Rsp) (Reg Rbp)) -- set the stack base pointer
      mapM_ setupRegArgs (zip vs argRegs)
      puts
        ( \x@(TransST {freeRegs}) -> x {freeRegs = freeRegs ++ drop (length vs) argRegs} -- mark extra arg regs as usable
        )
      -- \| if more than 6 arguments, subtract from rsp (more than the number of callee saved) to get stack position
      mapM_
        setupStackArgs
        ( zip
            (drop (length argRegs) vs)
            (map (\x -> (-x) - toInteger (length callee)) [0 ..])
        )
      translateBlocks (TAC.Label nStart) startBlock -- translate main part of code
      svn <- gets stackVarNum
      tellInstr (X86.Addq (Imm (-svn)) (Reg Rsp)) -- effectively delete local variables on stack
      mapM_ (tellInstr . Popq . Reg) (reverse callee) -- callee saving registers
      -- assigning arg vars to registers
    setupRegArgs :: (Var Integer, Register) -> Analysis ()
    setupRegArgs (v, r) = puts (addRaxloc v (Reg r))
    -- assigning extra arg vars to stack
    setupStackArgs :: (Var Integer, Integer) -> Analysis ()
    setupStackArgs (v, n) = puts (addRaxloc v (Mem (MRegI n Rbp)))

-- | translate each statement of the block. then figure out which block to go to
translateBlocks
  :: TAC.Label Integer
  -> BasicBlock Integer Integer
  -> Analysis ()
translateBlocks l (BasicBlock is next) = do
  translateBlock l is
  translateNext next

-- | modify the state of the RWS monad
puts :: (Monoid w) => (s -> s) -> RWS r w s ()
puts f = do
  s <- get
  put (f s)

-- | Mark the block as translated, so its not re-translated
setTranslated :: TAC.Label Integer -> TransST -> TransST
setTranslated l x@(TransST {translated}) = x {translated = S.insert l translated}

-- | translate each TAC statement
translateBlock
  :: TAC.Label Integer
  -> [TAC Integer Integer]
  -> Analysis ()
translateBlock l is = do
  puts (setTranslated l) -- include label in translated set
  tellInstr (Lab (mapLab l))
  mapM_ translateTAC is

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
  (if t then tellInstr (Jmp (mapLab l1)) else translateBlocks l1 nextBlock)
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

addRaxloc :: Var Integer -> Operand -> TransST -> TransST
addRaxloc v o x@(TransST {alloc}) = x {alloc = B.insert v o alloc}

-- | Free a register if none are available by pushing the swapReg onto stack
getReg :: Analysis Register
getReg = do
  rss <- gets freeRegs
  case rss of
    [] -> do
      tellInstr (Pushq (Reg swapReg))
      swapVar <- gets ((B.!> Reg swapReg) . alloc)
      puts (\x@(TransST {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
      stackAddr <- gets ((* 4) . stackVarNum)
      puts (addRaxloc swapVar (Mem (MRegI stackAddr Rbp)))
      return swapReg
    (r : rs) -> do
      puts (\x -> x {freeRegs = rs})
      return r

allocate :: Var Integer -> Analysis Operand
allocate v = do
  -- increase the stackVarNum
  puts (\x@(TransST {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
  -- insert the variable into the allocation map
  stackAddr <- gets ((* 8) . stackVarNum)
  puts (addRaxloc v (Mem (MRegI stackAddr Rbp)))
  return (Mem (MRegI stackAddr Rbp))

allocate' :: Var Integer -> Analysis Operand
allocate' v = do
  -- check if the variable is already allocated
  alloc' <- gets (B.lookup v . alloc)
  case alloc' of
    Just o -> return o
    Nothing -> allocate v

getLabel :: Analysis X86.Label
getLabel = do
  n <- gets labelCounter
  puts (\x -> x {labelCounter = n + 1})
  return (S (".TAC_L" ++ show n))

getSize :: WType -> Int
getSize WInt = 4
getSize WChar = 1
getSize WBool = 1
getSize _ = 8

saveRegister :: [Register] -> Analysis ()
saveRegister = mapM_ (tellInstr . Pushq . Reg)

restoreRegister :: [Register] -> Analysis ()
restoreRegister = mapM_ (tellInstr . Popq . Reg)

getOprand :: Var Integer -> Analysis Operand
getOprand v = gets ((B.! v) . alloc)

translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr v1 v2 op v3) = do
  comment $
    "BinInstr: " ++ show v1 ++ " := " ++ show v2 ++ " " ++ show op ++ " " ++ show v3
  operand <- allocate' v1
  operand1 <- getOprand v2
  operand2 <- getOprand v3
  translateBinOp operand op operand1 operand2
  comment "End BinInstr"
translateTAC (UnInstr v1 op v2) =
  do
    comment $ "UnInstr: " ++ show v1 ++ " := " ++ show op ++ " " ++ show v2
    operand <- allocate' v1
    operand' <- getOprand v2
    translateUnOp operand op operand'
    comment "End UnInstr"
translateTAC (Store v1 off v2 w) = do
  -- \| > <var> := <var>[<Offset>]
  comment $ "Store: " ++ show v1 ++ " := " ++ show v2 ++ "[" ++ show off ++ "]"
  translateStore v1 off v2 (getSize w)
  comment "End Store"
translateTAC (LoadCI v i) = do
  comment $ "LoadCI: " ++ show v ++ " := " ++ show i
  operand <- allocate' v
  movq (Imm (fromIntegral i)) operand
  comment "End LoadCI"
{-
.section .rodata
	.int 11
.L.str0:
	.asciz "hello world"
.text
leaq .L.str0(%rip), o
-}
translateTAC (LoadCS v s) = do
  comment $ "LoadCS: " ++ show v ++ " := " ++ show s
  o <- allocate' v
  l <- getLabel
  section
  rodata
  int (fromIntegral $ length s)
  lab l
  asciz s
  text
  leaq (Mem (MRegL l Rip)) o
  comment "End LoadCS"
translateTAC (LoadM v1 v2 off w) = do
  comment $ "LoadM: " ++ show v1 ++ " := " ++ show v2 ++ "[" ++ show off ++ "]"
  translateLoadM v1 v2 off (getSize w)
  comment "End LoadM"
translateTAC (TAC.Call v1 (Label l) vs) = do
  comment $ "Call: " ++ show v1 ++ " := call " ++ show l ++ "(" ++ show vs ++ ")"
  -- push all registers on to stack
  os <- mapM getOprand vs
  mapM_ (tellInstr . Pushq) (reverse os)
  -- call the function
  call (I l)
  comment "End Call"
translateTAC (Print v w) = do
  comment $ "Print: print " ++ show v
  operand <- getOprand v
  movq operand arg1
  translatePrint w
  comment "End Print"
translateTAC (TAC.PrintLn v w) = do
  comment $ "PrintLn: println " ++ show v
  translateTAC (Print v w)
  call printLn
  comment "End PrintLn"
translateTAC (TAC.Exit v) = do
  comment $ "Exit: exit " ++ show v
  operand <- getOprand v
  movq operand arg1
  call (R X86.Exit)
  comment "End Exit"
translateTAC (Read v w) = do
  comment $ "Read: " ++ show v ++ " := read"
  operand <- allocate' v
  translateRead operand w
  comment "End Read"
translateTAC (TAC.Malloc lv rv) = do
  comment $ "Malloc: " ++ show lv ++ " := malloc " ++ show rv
  operand <- allocate' lv
  operand' <- getOprand rv
  movq operand' arg1
  call (R X86.Malloc)
  movq argRet operand
  comment "End Malloc"
translateTAC (TAC.Free v) = do
  comment $ "Free: free " ++ show v
  operand <- getOprand v
  movq operand arg1
  call (R X86.Free)
  comment "End Free"

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
{-
  cmpl $0, o1
  je .L2
  cmpl $0, o2
  je .L2
  movl $1, %eax
  jmp .L3
.L2:
  movl $0, %eax
.L3:
  movzbl %al, %eax
  movl %eax, o
-}
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
{-
  cmpl $0, o1
  jne .L2
  cmpl $0, o2
  je .L3
.L2:
  movl $1, %eax
  jmp .L4
.L3:
  movl $0, %eax
.L4:
  movzbl %al, %eax
  movl %eax, o
-}
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

translatePrint :: WType -> Analysis ()
translatePrint WInt = do call printi
translatePrint WBool = do call printb
translatePrint WChar = do call printc
translatePrint WString = do call prints
translatePrint _ = do call printp

translateRead :: Operand -> WType -> Analysis ()
translateRead o WInt = do
  call (R X86.ReadI)
  movq argRet o
translateRead o WChar = do
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
  o2 <- getOprand v2
  offset <- getOprand off
  movq o2 r9
  movq offset r10
  call (arrayLoad s)
  movq r9 o1

translateStore
  :: Var Integer -> Var Integer -> Var Integer -> Int -> Analysis ()
translateStore v1 off v2 s = do
  -- array ptr passed in R9, index in R10, and value in R11
  o1 <- getOprand v1
  offset <- getOprand off
  o2 <- getOprand v2
  movq o1 r9
  movq offset r10
  movq o2 r11
  call (arrayStore s)

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

jo :: X86.Label -> Analysis ()
jo l = tellInstr (Jo l)

je :: X86.Label -> Analysis ()
je l = tellInstr (Je l)

jne :: X86.Label -> Analysis ()
jne l = tellInstr (Jne l)

jmp :: X86.Label -> Analysis ()
jmp l = tellInstr (Jmp l)

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
call l = tellInstr (X86.Call l)

arg1 :: Operand
arg1 = Reg Rdi

arg2 :: Operand
arg2 = Reg Rsi

arg3 :: Operand
arg3 = Reg Rdx

arg4 :: Operand
arg4 = Reg Rcx

arg5 :: Operand
arg6 :: Operand
arg5 = Reg R8

arg6 = Reg R9

argRet :: Operand
argRet = Reg Rax

printi :: X86.Label
printi = R X86.PrintI

printc :: X86.Label
printc = R X86.PrintC

printb :: X86.Label
printp :: X86.Label
printb = R X86.PrintB

printp = R X86.PrintP

prints :: X86.Label
prints = R X86.PrintS

printLn :: X86.Label
printLn = R X86.PrintLn

errOverflow = R X86.ErrOverflow

errDivByZero = R X86.ErrDivByZero

lab :: X86.Label -> Analysis ()
lab = tellInstr . Lab

section :: Analysis ()
section = tellInstr (Dir DirSection)

rodata :: Analysis ()
rodata = tellInstr (Dir DirRodata)

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
