{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Type checking actions for WACC programs.
-}
module Language.WACC.TypeChecking.Prog () where

import Control.Monad (when)
import Language.WACC.AST (Func (..), Prog (..), WType)
import Language.WACC.TypeChecking.BType (BType (BAny), FnType (FnType), fix)
import Language.WACC.TypeChecking.Class
import Language.WACC.TypeChecking.Error (TypeError (ReturnFromMainError))
import Language.WACC.TypeChecking.State (abortWith, reportAt, setFnType)
import Language.WACC.TypeChecking.Stmt (unifyStmts, unifyStmtsAt)
import Text.Gigaparsec.Position (Pos)

type instance Typed (Func typ fnident ident ann) = Func typ fnident ident BType

instance (Ord fnident) => FnTypeChecked (Func WType fnident ident Pos) where
  type TypingFnIdent (Func WType fnident ident Pos) = fnident
  fnCheck (Func rwt f pwts ss p) = do
    (_, ss') <- unifyStmtsAt p (fix rwt) ss
    pure $ Func rwt f pwts ss' BAny

type instance Typed (Prog typ fnident ident ann) = Prog typ fnident ident BType

instance (Ord fnident) => FnTypeChecked (Prog WType fnident ident Pos) where
  type TypingFnIdent (Prog WType fnident ident Pos) = fnident
  fnCheck (Main fs ss p) = reportAt p BAny $ do
    mapM_ setFnType' fs
    fs' <- mapM fnCheck fs
    (t, ss') <- unifyStmts BAny ss
    when (t /= BAny) (abortWith $ ReturnFromMainError p)
    pure $ Main fs' ss' BAny
    where
      setFnType' (Func rwt f pwts _ _) =
        setFnType f (FnType (fix . fst <$> pwts) (fix rwt))
