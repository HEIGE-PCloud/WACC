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
        (TransST B.empty S.empty 0 S.empty ((callee \\ [Rbp]) ++ caller)) -- Not empty regs list
    (n, startBlock) = M.findMin bs

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
      translateBlocks (TAC.Label n) startBlock -- translate main part of code
      svn <- gets stackVarNum
      tellInstr (Addq (Imm (-svn)) (Reg Rsp)) -- effectively delete local variables on stack
      mapM (tellInstr . Popq . Reg) (reverse callee) -- callee saving registers
    setupRegArgs :: (Var Integer, Register) -> Analysis ()
    setupRegArgs (v, r) = puts (addAlloc v (Reg r))
    setupStackArgs :: (Var Integer, Integer) -> Analysis ()
    setupStackArgs (v, n) = puts (addAlloc v (Mem (MRegI n Rbp)))

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
translateBlock l@(TAC.Label n) is = do
  puts (setTranslated l) -- include label in translated set
  tellInstr (Lab (mapLab l))
  mapM_ translateTAC is

mapLab :: TAC.Label Integer -> X86.Label
mapLab (Label x) = I x

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
translateNext (CJump v l1 l2@(TAC.Label n2)) = do
  operand <- gets ((B.! v) . alloc)
  tellInstr (Cmpq operand (Imm 0))
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
  translateNext (Jump l2)
  t <- isTranslated l1
  (if t then pure () else translateNext (Jump l1))
translateNext (TAC.Ret var) = pure ()

addAlloc :: Var Integer -> Operand -> TransST -> TransST
addAlloc v o x@(TransST {alloc}) = x {alloc = B.insert v o alloc}

-- | Free a register if none are available by pushing the swapReg onto stack
getReg :: Analysis Register
getReg = do
  rs <- gets freeRegs
  case rs of
    [] -> do
      tellInstr (Pushq (Reg swapReg))
      swapVar <- gets ((B.!> Reg swapReg) . alloc)
      puts (\x@(TransST {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
      stackAddr <- gets ((* 4) . stackVarNum)
      puts (addAlloc swapVar (Mem (MRegI stackAddr Rbp)))
      return swapReg
    (r : rs) -> do
      puts (\x -> x {freeRegs = rs})
      return r

getReg' v = do
  r <- getReg
  let
    reg = Reg r
  puts (addAlloc v reg)
  return reg

saveRegister :: [Register] -> Analysis ()
saveRegister = mapM_ (tellInstr . Pushq . Reg)

restoreRegister :: [Register] -> Analysis ()
restoreRegister = mapM_ (tellInstr . Popq . Reg)

getOprand :: Var Integer -> Analysis Operand
getOprand v = gets ((B.! v) . alloc)

translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr v1 v2 op v3) = do
  operand <- getReg' v1
  operand1 <- getOprand v2
  operand2 <- getOprand v3
  translateBinOp operand op operand1 operand2
translateTAC (UnInstr v1 op v2) =
  do
    operand <- getReg' v1
    operand' <- getOprand v2
    translateUnOp operand op operand'
translateTAC (Store v1 off v2 w) = undefined
translateTAC (LoadCI v i) = do
  operand <- getReg' v
  tellInstr (Movq (Imm (fromIntegral i)) operand)
translateTAC (LoadCS v s) = undefined
translateTAC (LoadM v1 v2 off w) = undefined
translateTAC (TAC.Call v1 (Label l) vs) = undefined
translateTAC (Print v w) = undefined
translateTAC (TAC.PrintLn v w) = undefined
translateTAC (Exit v) = undefined
translateTAC (Read v w) = undefined
translateTAC (TAC.Malloc lv rv) = do
  operand <- getReg' lv
  operand' <- getOprand rv
  movq operand' arg1
  call (R X86.Malloc)
  movq argRet operand
translateTAC (TAC.Free v) = do
  operand <- getOprand v
  movq operand arg1
  call (R X86.Free)

{- | Translate a binary operation
| <o> := <o1> <binop> <o2>
-}
translateBinOp :: Operand -> BinOp -> Operand -> Operand -> Analysis ()
translateBinOp o Add o1 o2 = do
  movl o1 eax
  movl o2 ebx
  addl ebx eax
  jo ErrOverflow
  movl ebx o
translateBinOp o Sub o1 o2 = do
  movl o1 eax
  movl o2 ebx
  subl ebx eax
  jo ErrOverflow
  movl ebx o
translateBinOp o Mul o1 o2 = do
  movl o1 eax
  movl o2 ebx
  imull ebx eax
  jo ErrOverflow
  movl ebx o
translateBinOp o Div o1 o2 = do
  movl o1 eax -- %eax := o1
  cmpl (Imm 0) eax -- check for division by zero
  je ErrDivByZero
  cltd -- sign extend eax into edx
  movl o2 ebx -- %ebx := o2
  idivl ebx -- divide edx:eax by ebx
  movl eax o -- %o := eax
translateBinOp o Mod o1 o2 = do
  movl o1 eax -- %eax := o1
  cmpl (Imm 0) eax -- check for division by zero
  je ErrDivByZero
  cltd -- sign extend eax into edx
  movl o2 ebx -- %ebx := o2
  idivl ebx -- divide edx:eax by ebx
  movl edx o -- %o := edx
translateBinOp o And o1 o2 = undefined -- TODO: needs unique labels
translateBinOp o Or o1 o2 = undefined
translateBinOp o TAC.LT o1 o2 = do
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setl al -- set al to 1 if %eax < %ebx
  movzbl al o -- %o := %al
translateBinOp o TAC.LTE o1 o2 = do
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setle al -- set al to 1 if %eax <= %ebx
  movzbl al o -- %o := %al
translateBinOp o TAC.GT o1 o2 = do
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setg al -- set al to 1 if %eax > %ebx
  movzbl al o -- %o := %al
translateBinOp o TAC.GTE o1 o2 = do
  movl o1 eax -- %eax := o1
  movl o2 ebx -- %ebx := o2
  cmpl ebx eax -- compare %ebx and %eax
  setge al -- set al to 1 if %eax >= %ebx
  movzbl al o -- %o := %al
translateBinOp o TAC.Eq o1 o2 = do
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  cmpq rbx rax -- compare %ebx and %eax
  sete al -- set al to 1 if %eax == %ebx
  movzbl al o -- %o := %al
translateBinOp o TAC.Ineq o1 o2 = do
  movq o1 rax -- %eax := o1
  movq o2 rbx -- %ebx := o2
  cmpq rbx rax -- compare %ebx and %eax
  setne al -- set al to 1 if %eax != %ebx
  movzbl al o -- %o := %al

-- | <var> := <unop> <var>
translateUnOp :: Operand -> UnOp -> Operand -> Analysis ()
translateUnOp o Not o' = do
  movl o' eax
  cmpl (Imm 0) eax
  sete al
  movzbl al o
translateUnOp o Negate o' = do
  movl o' eax
  negl eax
  movl eax o

al :: Operand
al = Reg Al

rax :: Operand
rax = Reg Rax

rbx :: Operand
rbx = Reg Rbx

eax :: Operand
eax = Reg Eax

ebx :: Operand
ebx = Reg Ebx

ecx :: Operand
ecx = Reg Ecx

edx :: Operand
edx = Reg Edx

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

jo :: X86.Runtime -> Analysis ()
jo l = tellInstr (Jo (R l))

je :: X86.Runtime -> Analysis ()
je l = tellInstr (Je (R l))

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

arg1 = Reg Rdi

arg2 = Reg Rsi

arg3 = Reg Rdx

arg4 = Reg Rcx

arg5 = Reg R8

arg6 = Reg R9

argRet = Reg Rax