{- |
WACC program type checker.
-}
module Language.WACC.TypeChecking (checkTypes) where

import Data.Foldable (toList)
import Data.Map ((!))
import Language.WACC.AST (Prog, WType)
import Language.WACC.Error (Error)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident)
import Language.WACC.TypeChecking.BType (fix)
import Language.WACC.TypeChecking.Class (fnCheck)
import Language.WACC.TypeChecking.Error (convertTypeError)
import Language.WACC.TypeChecking.Prog ()
import Language.WACC.TypeChecking.State (runTypingM)
import Text.Gigaparsec.Position (Pos)

{- |
Run the type checker on a renamed AST.
-}
checkTypes :: Prog Pos WType Fnident Vident -> VarST -> [Error]
checkTypes program vs =
  case runTypingM (fnCheck program) (fix . fst . (vs !)) mempty of
    (_, _, errors) -> toList $ convertTypeError <$> errors
