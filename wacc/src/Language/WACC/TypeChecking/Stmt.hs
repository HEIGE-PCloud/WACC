{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Type checking actions for WACC statements.
-}
module Language.WACC.TypeChecking.Stmt
  ( unifyStmts
  , unifyStmtsAt
  )
where

import Control.Monad (foldM, unless, when, zipWithM_)
import Data.List.NonEmpty (sort)
import Language.WACC.AST
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Class
import Language.WACC.TypeChecking.Expr
import Language.WACC.TypeChecking.State
import Text.Gigaparsec.Position (Pos)

unifyPair :: LValue Pos ident -> TypingM fnident ident (BType, BType)
unifyPair lv = do
  lvt <- check lv
  pt <- tryUnify (BKnownPair BAny BAny) lvt
  case pt of
    BKnownPair t1 t2 -> pure (t1, t2)
    _ -> abort

instance TypeChecked (PairElem Pos ident) where
  check (FstElem lv _) = fst <$> unifyPair lv
  check (SndElem lv _) = snd <$> unifyPair lv

instance TypeChecked (LValue Pos ident) where
  check (LVIdent v _) = typeOf v
  check (LVArrayElem ai _) = check ai
  check (LVPairElem pe _) = check pe

instance (Ord fnident) => FnTypeChecked (RValue Pos fnident ident) where
  type TypingFnIdent (RValue Pos fnident ident) = fnident
  fnCheck (RVExpr x _) = check x
  fnCheck (RVArrayLit xs p) = do
    mt <- tryUnifyExprs BAny xs
    case mt of
      Just t -> pure (BArray t)
      Nothing -> abortWith $ HeterogeneousArrayError p
  fnCheck (RVNewPair x1 x2 _) = BKnownPair <$> check x1 <*> check x2
  fnCheck (RVPairElem pe _) = check pe
  fnCheck (RVCall f xs p) = do
    FnType {..} <- typeOfFn f
    let
      actN = length xs
      expN = length paramTypes
    unless (actN == expN) (abortWith $ FunctionCallArityError actN expN p)
    ts <- mapM check xs
    zipWithM_ (\xt pt -> reportAt p pt $ tryUnify xt pt) ts paramTypes
    pure retType

{- |
@unifyStmts t0 (s1 :| [s2, ..., sn])@ attempts to unify @s1@ with @t0@ to obtain
@t1@, which is unified with @s2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyStmts
  :: (Ord fnident)
  => BType
  -> Stmts Pos fnident ident
  -> TypingM fnident ident BType
unifyStmts t ss =
  traverse (resumeAfter . fnCheck) (unwrap ss)
    >>= foldM (flip tryUnify) t . sort

{- |
Associate a 'Pos' with a @unifyStmts@ action.
-}
unifyStmtsAt
  :: (Ord fnident)
  => Pos
  -> BType
  -> Stmts Pos fnident ident
  -> TypingM fnident ident BType
unifyStmtsAt p t ss = reportAt p t $ unifyStmts t ss

{- |
@check (Return x)@ returns the type of @x@.

@check (IfElse x ifBody elseBody)@ unifies the types returned by
@unifyStmts ifBody@ and @unifyStmts elseBody@.

@check (While x body)@ and @check (BeginEnd body)@ return the result of
@unifyStmts body@.

Otherwise, 'BAny' is returned.
-}
instance (Ord fnident) => FnTypeChecked (Stmt Pos fnident ident) where
  type TypingFnIdent (Stmt Pos fnident ident) = fnident
  fnCheck (Skip _) = pure BAny
  fnCheck (Decl wt v rv p) =
    [ BAny
    | vt <- typeOf v
    , t' <- reportAt p t $ tryUnify vt t
    , rvt <- fnCheck rv
    , _ <- reportAt rv t $ tryUnify rvt t'
    ]
    where
      t = fix wt
  fnCheck (Asgn lv rv p) = reportAt p BAny $ do
    rvt <- fnCheck rv
    lvt <- check lv
    t <- reportAt rv lvt $ tryUnify rvt lvt
    when (t == BAny) (abortWith $ UnknownAssignmentError p)
    pure BAny
  fnCheck (Read lv p) = reportAt p BAny $ do
    t <- check lv
    unless
      (t `elem` readableTypes)
      (abortWith $ ExpectedReadableTypeError t (getPos lv))
    pure BAny
  fnCheck (Free x p) = reportAt p BAny $ do
    t <- check x
    unless
      (isHeapAllocated t)
      (abortWith $ ExpectedHeapAllocatedTypeError t (getPos x))
    pure BAny
  fnCheck (Return x _) = check x
  fnCheck (Exit x _) = BAny <$ unifyExprsAt x BInt [x]
  fnCheck (Print x _) = BAny <$ check x
  fnCheck (PrintLn x _) = BAny <$ check x
  fnCheck (IfElse x ifBody elseBody p) = do
    _ <- unifyExprsAt x BBool [x]
    t <- unifyStmtsAt p BAny ifBody
    unifyStmtsAt p t elseBody
  fnCheck (While x body p) =
    unifyExprsAt x BBool [x] *> unifyStmtsAt p BAny body
  fnCheck (BeginEnd body p) = unifyStmtsAt p BAny body
