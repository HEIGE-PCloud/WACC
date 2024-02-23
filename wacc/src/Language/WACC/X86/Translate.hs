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
  { swapVar :: Maybe (Var Integer)
  , getAlloc :: Map (Var Integer) Operand
  , translated :: Set TAC.Label
  }

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
  :: TAC.Label Integer
  -> BasicBlock Integer Integer
  -> Analysis ()
translateBlocks l (BasicBlock is next) = do
  translateBlock l
  translateNext next

translateBlock
  :: TAC.Label Integer
  -> [TAC Integer Integer]
  -> Analysis ()
translateBlock l is = do
  puts (S.insert l . translated) -- include label in translated set
  tellInstr (Lab (mapLab l))
  mapM translateTAC is

mapLab :: (TAC.Label Integer) -> X86.Label
mapLab (Label x) = I x

tellInstr = tell . D.singleton

isTranslated :: Label Integer -> Analaysis Boolean
isTranslated l = gets ((member l) . translated)

translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump l1@(Label n)) = do
  nextBlock <- asks (! n)
  t <- isTranslated l1
  case t of
    False -> translateBlocks l1 nextBlock
    True -> tellInstr (Jmp l1)
translateNext (CJump v l1@(TAC.Label n1) l2) = do
  operand <- gets ((! v) . getAlloc)
  tellInstr (Cmpq operand (Imm 0))
  tellInstr (Je (mapLab l2)) -- jump to l2 if v == 0. Otherwise keep going
  translateNext (Jump l1)
  t <- isTranslated l2
  case t of
    False -> translateNext (Jump l2)
    True -> pure ()
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
