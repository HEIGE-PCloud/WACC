{- |
Type checking actions for WACC programs.
-}
module Language.WACC.TypeChecking.Prog (checkFunc, checkProg) where

import Control.Monad (when)
import Data.Map ((!))
import Language.WACC.AST.Prog (Func (..), Prog (..))
import Language.WACC.Error (Error)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident)
import Language.WACC.TypeChecking.BType (BType (BAny), FnType (FnType), fix)
import Language.WACC.TypeChecking.State
  ( TypingM
  , abortActual
  , reportAt
  , setFnType
  )
import Language.WACC.TypeChecking.Stmt (unifyStmts, unifyStmtsAt)
import Text.Gigaparsec (Result (Failure, Success))

{- |
Type check a WACC function.

Records the type of the function in the 'TypingM' state on success.
-}
checkFunc :: (Ord fnident) => Func fnident ident -> TypingM fnident ident ()
checkFunc (Func rwt f pwts ss p) = do
  let
    rt = fix rwt
  setFnType f (FnType (fix . fst <$> pwts) rt)
  _ <- unifyStmtsAt p rt ss
  pure ()

{- |
Type check a WACC program.
-}
checkProg :: (Ord fnident) => Prog fnident ident -> TypingM fnident ident ()
checkProg (Main fs ss p) = reportAt p BAny $ do
  mapM_ checkFunc fs
  t <- unifyStmts BAny ss
  when (t /= BAny) $ abortActual t
