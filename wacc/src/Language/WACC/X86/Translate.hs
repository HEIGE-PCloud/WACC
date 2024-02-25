{-# LANGUAGE NamedFieldPuns #-}

module Language.WACC.X86.Translate where

import Control.Monad.RWS
  ( RWS
  , asks
  , evalRWS
  , get
  , gets
  , local
  , modify
  , put
  , tell
  )
import Data.Bimap hiding ((!))
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.Map (Map, findMin, (!))
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
  , Register (R10, Rbp)
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
  { getAlloc :: Bimap (Var Integer) Operand
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

setTranslated :: (TAC.Label Integer) -> Allocation -> Allocation
setTranslated l x@(Allocation {translated}) = x {translated = (S.insert l) translated}

translateBlock
  :: TAC.Label Integer
  -> [TAC Integer Integer]
  -> Analysis ()
translateBlock l@(TAC.Label n) is = do
  puts (setTranslated l) -- include label in translated set
  tellInstr (Lab (mapLab l))
  mapM translateTAC is
  return ()

mapLab :: (TAC.Label Integer) -> X86.Label
mapLab (Label x) = I x

tellInstr = tell . D.singleton

isTranslated :: TAC.Label Integer -> Analysis Bool
isTranslated l = gets ((S.member l) . translated)

translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump l1@(Label n)) = do
  nextBlock <- asks (! n)
  t <- isTranslated l1
  case t of
    False -> translateBlocks l1 nextBlock
    True -> tellInstr (Jmp (mapLab l1))
translateNext (CJump v l1 l2@(TAC.Label n2)) = do
  operand <- gets ((B.! v) . getAlloc)
  tellInstr (Cmpq operand (Imm 0))
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
  translateNext (Jump l2)
  t <- isTranslated l1
  case t of
    False -> translateNext (Jump l1)
    True -> pure ()
translateNext (TAC.Ret var) = pure ()

-- getVar :: Var Integer -> Analysis Register
-- getVar v = do
--  vLoc <- gets (! v)
--  case vLoc of
--    Imm i -> error "This is not supposed to happen by pre-condition"
--    Reg r -> return (Reg r)
--    Mem m -> do undefined

addAlloc :: Var Integer -> Operand -> Allocation -> Allocation
addAlloc v o x@(Allocation {getAlloc}) = x {getAlloc = (B.insert v o) getAlloc}

getReg :: Analysis Register
getReg = do
  rs <- gets freeRegs
  case rs of
    [] -> do
      tellInstr (Pushq (Reg swapReg))
      swapVar <- gets ((B.!> (Reg swapReg)) . getAlloc)
      puts (\x@(Allocation {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
      stackAddr <- (gets ((* 4) . stackVarNum))
      puts (addAlloc swapVar (Mem (MRegI stackAddr Rbp)))
      return swapReg
    (r : rs) -> do
      puts (\x -> x {freeRegs = rs})
      return r

translateBinOp :: BinOp -> (Operand -> Operand -> Instr)
translateBinOp = undefined -- TODO deal with division separately

translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr lv rv1 op rv2) = do
  -- always assign new variables to a register
  r <- getReg
  puts (addAlloc lv (Reg r))
  op1 <- gets ((B.! rv1) . getAlloc)
  op2 <- gets ((B.! rv2) . getAlloc)
  tellInstr (Movq op1 (Reg r))
  tellInstr ((translateBinOp op) op2 (Reg r))
  tellInstr (Jo (R ErrOverflow))

-- translateTAC (EqR (Var ident) (Var ident) (Var ident)
-- translateTAC (IneqR (Var ident) (Var ident) (Var ident)
-- translateTAC (EqV (Var ident) (Var ident) (Var ident)
-- translateTAC (IneqV (Var ident) (Var ident) (Var ident)
-- translateTAC (UnInstr (Var ident) UnOp (Var ident)
-- translateTAC (Store (Var ident) (Offset ident) (Var ident) WType
-- translateTAC (LoadCI (Var ident) Int
-- translateTAC (LoadCS (Var ident) String
-- translateTAC (LoadM (Var ident) (Var ident) (Offset ident) WType
-- translateTAC (Call (Var ident) (Label lident) [Var ident]
-- translateTAC (Print (Var ident) WType
-- translateTAC (PrintLn (Var ident) WType
-- translateTAC (Exit (Var ident)
-- translateTAC (Read (Var ident) WType
-- translateTAC (TAC.Malloc lv rv) = translateTAC (TAC.Call lv (R X86.Malloc) [rv])

-- translateTAC (Free (Var ident)
