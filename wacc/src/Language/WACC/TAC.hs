{- |
Three-address code intermediate representation.
-}
module Language.WACC.TAC (generateTAC, module Language.WACC.TAC.TAC) where

import Data.List (genericLength)
import Language.WACC.AST (Prog (Main), WType)
import Language.WACC.TAC.Class (fnToTAC)
import Language.WACC.TAC.Prog ()
import Language.WACC.TAC.State (evalTACM)
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking (BType)

{- |
Translate a type checked program into three-address code.
-}
generateTAC
  :: (Enum fnident, Enum ident, Eq ident, Num fnident, Num ident, Ord fnident)
  => Prog WType fnident ident BType
  -> TACProgram ident fnident
generateTAC program@(Main fs _ _) = evalTACM nextBlockId (fnToTAC program)
  where
    nextBlockId = genericLength fs + 1
