{-# LANGUAGE NamedFieldPuns #-}

module Language.WACC.X86.Translate where

import Control.Monad.RWS
  ( RWS
  , asks
  , evalRWS
  , get
  , gets
  , put
  , tell
  )
import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.WACC.TAC.TAC
import qualified Language.WACC.TAC.TAC as TAC
import Language.WACC.X86.X86
  ( Instr (..)
  , Label (..)
  , Memory (..)
  , Operand (..)
  , Prog
  , Register (..)
  , Runtime (..)
  )
import qualified Language.WACC.X86.X86 as X86

{- | A caller saved register. When registers run out and values in the stack are
required for a computation, they are swapped with the value in this register.
This is only required when there are both operands are in memory.
-}
swapReg :: Register
swapReg = R10

data Allocation = Allocation
  { getAlloc :: Bimap (Var Integer) Operand -- a bidirectional map between variables and their locations
  , translated :: Set (TAC.Label Integer)
  , stackVarNum :: Integer
  , runtimeFns :: Set Runtime
  , freeRegs :: [Register]
  }

type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    (DList Instr)
    Allocation

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg = D.toList . foldr (D.append . translateFunc) D.empty . M.elems

{- | Rbp points to the location just before the first stack variable of a frame
This means the last callee saved register pushed onto the stack
-}
translateFunc :: Func Integer Integer -> DList Instr
translateFunc (Func l v bs) = is
  where
    ((), is) =
      evalRWS
        (translateBlocks (TAC.Label n) startBlock)
        bs
        (Allocation B.empty S.empty 0 S.empty []) -- Not empty regs list
    (n, startBlock) = M.findMin bs

translateBlocks
  :: TAC.Label Integer
  -> BasicBlock Integer Integer
  -> Analysis ()
translateBlocks l (BasicBlock is next) = do
  translateBlock l is
  translateNext next

puts :: (Monoid w) => (s -> s) -> RWS r w s ()
puts f = do
  s <- get
  put (f s)

setTranslated :: TAC.Label Integer -> Allocation -> Allocation
setTranslated l x@(Allocation {translated}) = x {translated = S.insert l translated}

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

translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump l1@(Label n)) = do
  nextBlock <- asks (! n)
  t <- isTranslated l1
  (if t then tellInstr (Jmp (mapLab l1)) else translateBlocks l1 nextBlock)
translateNext (CJump v l1 l2@(TAC.Label n2)) = do
  operand <- gets ((B.! v) . getAlloc)
  tellInstr (Cmpq operand (Imm 0))
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
  translateNext (Jump l2)
  t <- isTranslated l1
  (if t then pure () else translateNext (Jump l1))
translateNext (TAC.Ret var) = pure ()

-- getVar :: Var Integer -> Analysis Register
-- getVar v = do
--  vLoc <- gets (! v)
--  case vLoc of
--    Imm i -> error "This is not supposed to happen by pre-condition"
--    Reg r -> return (Reg r)
--    Mem m -> do undefined

addAlloc :: Var Integer -> Operand -> Allocation -> Allocation
addAlloc v o x@(Allocation {getAlloc}) = x {getAlloc = B.insert v o getAlloc}

-- | Get the next free register
getReg :: Analysis Register
getReg = do
  rs <- gets freeRegs
  case rs of
    [] -> do
      tellInstr (Pushq (Reg swapReg))
      swapVar <- gets ((B.!> Reg swapReg) . getAlloc)
      puts (\x@(Allocation {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
      stackAddr <- gets ((* 4) . stackVarNum)
      puts (addAlloc swapVar (Mem (MRegI stackAddr Rbp)))
      return swapReg
    (r : rs) -> do
      puts (\x -> x {freeRegs = rs})
      return r

translateBinOp :: BinOp -> Operand -> Operand -> [Instr]
translateBinOp Add o1 o2 = [Addl o1 o2, Jo (R ErrOverflow)]
translateBinOp Sub o1 o2 = [Subl o1 o2, Jo (R ErrOverflow)]
translateBinOp Mul o1 o2 = [Imull o1 o2, Jo (R ErrOverflow)]
{-
43		movl %r12d, %eax
44		cmpl $0, %r13d
45		je _errDivZero
46		# sign extend EAX into EDX
47		cltd
48		idivl %r13d
-}
translateBinOp Div o1 o2 =
  [ Movl o1 (Reg Eax)
  , Cmpl (Imm 0) o2
  , Je (R ErrDivByZero)
  , Cltd
  , Idivl o2
  ]
translateBinOp Mod o1 o2 = []
translateBinOp And o1 o2 = []
translateBinOp Or o1 o2 = []
translateBinOp Lt o1 o2 = [Cmpq o1 o2]
translateBinOp Gt o1 o2 = []
translateBinOp Le o1 o2 = []
translateBinOp Ge o1 o2 = []

translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr v1 v2 op v3) = do
  -- always assign new variables to a register
  r <- getReg
  let
    reg = Reg r
  puts (addAlloc v1 reg)
  operand1 <- gets ((B.! v2) . getAlloc)
  operand2 <- gets ((B.! v3) . getAlloc)
  tellInstr (Movq operand1 reg)
  mapM_ tellInstr (translateBinOp op operand2 reg)
translateTAC (EqR v1 v2 v3) = undefined
translateTAC (IneqR v1 v2 v3) = undefined
translateTAC (EqV v1 v2 v3) = undefined
translateTAC (IneqV v1 v2 v3) = undefined
translateTAC (UnInstr v1 op v2) = undefined
translateTAC (Store v1 off v2 w) = undefined
translateTAC (LoadCI v i) = undefined
translateTAC (LoadCS v s) = undefined
translateTAC (LoadM v1 v2 off w) = undefined
translateTAC (TAC.Call v1 (Label l) vs) = undefined
translateTAC (Print v w) = undefined
translateTAC (TAC.PrintLn v w) = undefined
translateTAC (Exit v) = undefined
translateTAC (Read v w) = undefined
translateTAC (TAC.Malloc lv rv) = undefined
translateTAC (TAC.Free v) = undefined
