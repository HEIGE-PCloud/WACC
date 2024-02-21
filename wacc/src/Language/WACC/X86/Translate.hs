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
import Data.Map (Map, findMin)
import qualified Data.Map as M
import Language.WACC.TAC.TAC
import Language.WACC.X86.X86 (Instr, Operand, Prog)

type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    (DList Instr)
    (Map (Var Integer) Operand)

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg = D.toList . foldr (D.append . translateFunc) D.empty . M.elems

translateFunc :: Func Integer Integer -> DList Instr
translateFunc (Func l v bs) = translateBlocks startBlock bs
  where
    startBlock = snd (findMin bs)

-- begin with no memory + reg instructions for binary operations. Then optimise

translateBlocks
  :: BasicBlock Integer Integer
  -> Map Integer (BasicBlock Integer Integer)
  -> DList Instr
translateBlocks = undefined

-- translateBlocks (BasicBlock is next) bs = map translateTAC is `append` translateNext next
--  where
--  translateNext :: Jump Integer Integer
--    Jump (Jump (Label l))) = translateBlocks (bs ! l) bs
--    Jump (CJump
--      Temp varIdent
--    Jump (Ret var) =
