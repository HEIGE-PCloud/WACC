{- |
Type checking actions for WACC programs.
-}
module Language.WACC.TypeChecking.Prog where

import Control.Monad (when)
import Language.WACC.AST.Prog
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.State
import Language.WACC.TypeChecking.Stmt

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
