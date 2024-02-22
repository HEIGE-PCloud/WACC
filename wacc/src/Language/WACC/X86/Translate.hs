module Language.WACC.X86.Translate where

import Control.Monad.RWS
  ( RWS
  , asks
  , evalRWS
  , gets
  , local
  , modify
  , tell
  )
import Data.DList (DList)
import qualified Data.DList as D
import Data.Map (Map, findMin, (!))
import qualified Data.Map as M
import Language.WACC.TAC.TAC
import qualified Language.WACC.TAC.TAC as TAC
import Language.WACC.X86.X86
  ( Instr (..)
  , Label (..)
  , Operand (..)
  , Prog
  , Register (R10)
  )
import qualified Language.WACC.X86.X86 as X86

{- | A caller saved register. When registers run out and values in the stack are
required for a computation, they are swapped with the value in this register.
This is only required when there are both operands are in memory.
-}
swapReg :: Register
swapReg = R10

data Allocation = Allocation
  {swapVar :: Maybe (Var Integer), getAlloc :: Map (Var Integer) Operand}

type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    (DList Instr)
    Allocation

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg = D.toList . foldr (D.append . translateFunc) D.empty . M.elems

translateFunc :: Func Integer Integer -> DList Instr
translateFunc (Func l v bs) = is
  where
    ((), is) = evalRWS (translateBlocks startBlock) bs (Allocation Nothing M.empty)
    startBlock = snd (findMin bs)

translateBlocks
  :: BasicBlock Integer Integer
  -> Analysis ()
translateBlocks (BasicBlock is next) = do
  mapM translateTAC is
  translateNext next

mapLab :: (TAC.Label Integer) -> X86.Label
mapLab (Label x) = I x

tellInstr = tell . D.singleton

translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump (Label l)) = asks (! l) >>= translateBlocks
translateNext (CJump v l1 l2) = do
  operand <- gets ((! v) . getAlloc)
  tellInstr (Cmpq operand (Imm 0))
  tellInstr (Je (mapLab l2)) -- jump to l2 if v == 0. Otherwise keep going
  translateNext (Jump l1)
translateNext (TAC.Ret var) = tellInstr X86.Ret -- TODO deal with arguments

-- getVar :: Var Integer -> Analysis Register
-- getVar v = do
--  vLoc <- gets (! v)
--  case vLoc of
--    Imm i -> error "This is not supposed to happen by pre-condition"
--    Reg r -> return (Reg r)
--    Mem m -> do undefined

translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC = undefined
