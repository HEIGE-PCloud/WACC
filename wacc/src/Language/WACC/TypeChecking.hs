{- |
WACC program type checker.
-}
module Language.WACC.TypeChecking (checkTypes) where

import Data.Foldable (toList)
import Data.Map ((!))
import Language.WACC.AST (Prog)
import Language.WACC.Error (Error)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident)
import Language.WACC.TypeChecking.BType (fix)
import Language.WACC.TypeChecking.Error (convertTypeError)
import Language.WACC.TypeChecking.Prog (checkProg)
import Language.WACC.TypeChecking.State (runTypingM)

{- |
Run the type checker on a renamed AST.
-}
checkTypes :: Prog Fnident Vident -> VarST -> [Error]
checkTypes program vs =
  case runTypingM (checkProg program) (fix . fst . (vs !)) mempty of
    (_, _, errors) -> toList $ convertTypeError <$> errors
