{- |
Three-address code intermediate representation.
-}
module Language.WACC.TAC (generateTAC, module Language.WACC.TAC.TAC) where

import Language.WACC.AST (Prog (Main), WType)
import Language.WACC.TAC.Class (fnToTAC)
import Language.WACC.TAC.Prog ()
import Language.WACC.TAC.State (evalTACM)
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking (BType)

{- |
Translate a type checked program into three-address code.
-}
generateTAC :: Prog WType Integer Integer BType -> TACProgram Integer Integer
generateTAC program@(Main fs _ _) = evalTACM nextBlockId (fnToTAC program)
  where
    nextBlockId = toInteger (length fs + 1)
