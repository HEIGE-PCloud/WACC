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

type instance Typed (PairElem ident ann) = PairElem ident BType

instance TypeChecked (PairElem ident Pos) where
  check (FstElem lv _) = do
    lv' <- check lv
    pt <- tryUnify (BKnownPair BAny BAny) (getAnn lv')
    t <- case pt of
      BKnownPair t _ -> pure t
      _ -> abort
    pure $ FstElem lv' t
  check (SndElem lv _) = do
    lv' <- check lv
    pt <- tryUnify (BKnownPair BAny BAny) (getAnn lv')
    t <- case pt of
      BKnownPair _ t -> pure t
      _ -> abort
    pure $ FstElem lv' t

type instance Typed (LValue ident ann) = LValue ident BType

instance TypeChecked (LValue ident Pos) where
  check (LVIdent v _) = LVIdent v <$> typeOf v
  check (LVArrayElem ai _) =
    [ LVArrayElem ai' t
    | (t, ai') <- check ai
    ]
  check (LVPairElem pe _) =
    [ LVPairElem pe' (getAnn pe')
    | pe' <- check pe
    ]

type instance Typed (RValue fnident ident ann) = RValue fnident ident BType

instance (Ord fnident) => FnTypeChecked (RValue fnident ident Pos) where
  type TypingFnIdent (RValue fnident ident Pos) = fnident
  fnCheck (RVExpr x _) =
    [ RVExpr x' (getAnn x')
    | x' <- check x
    ]
  fnCheck (RVArrayLit xs p) = do
    mtxs' <- tryUnifyExprs BAny xs
    case mtxs' of
      Just (t, xs') -> pure $ RVArrayLit xs' (BArray t)
      Nothing -> abortWith $ HeterogeneousArrayError p
  fnCheck (RVNewPair x1 x2 _) =
    [ RVNewPair x1' x2' (BKnownPair (getAnn x1') (getAnn x2'))
    | x1' <- check x1
    , x2' <- check x2
    ]
  fnCheck (RVPairElem pe _) =
    [ RVPairElem pe' (getAnn pe')
    | pe' <- check pe
    ]
  fnCheck (RVCall f xs p) = do
    FnType {..} <- typeOfFn f
    let
      actN = length xs
      expN = length paramTypes
    unless (actN == expN) (abortWith $ FunctionCallArityError actN expN p)
    xs' <- mapM check xs
    zipWithM_
      (\xt pt -> reportAt p pt $ tryUnify xt pt)
      (getAnn <$> xs')
      paramTypes
    pure $ RVCall f xs' retType

{- |
@unifyStmts t0 (s1 :| [s2, ..., sn])@ attempts to unify @s1@ with @t0@ to obtain
@t1@, which is unified with @s2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyStmts
  :: (Ord fnident)
  => BType
  -> Stmts fnident ident Pos
  -> TypingM fnident ident (BType, Stmts fnident ident BType)
unifyStmts t ss = do
  ss' <- traverse (resumeAfter (Skip BAny) . fnCheck) (unwrap ss)
  t' <- foldM (flip tryUnify) t $ sort (getAnn <$> ss')
  pure (t', Stmts ss')

{- |
Associate a 'Pos' with a @unifyStmts@ action.
-}
unifyStmtsAt
  :: (Ord fnident)
  => Pos
  -> BType
  -> Stmts fnident ident Pos
  -> TypingM fnident ident (BType, Stmts fnident ident BType)
unifyStmtsAt p t ss = reportAt p t $ unifyStmts t ss

type instance Typed (Stmt fnident ident ann) = Stmt fnident ident BType

{- |
@check (Return x)@ returns the type of @x@.

@check (IfElse x ifBody elseBody)@ unifies the types returned by
@unifyStmts ifBody@ and @unifyStmts elseBody@.

@check (While x body)@ and @check (BeginEnd body)@ return the result of
@unifyStmts body@.

Otherwise, 'BAny' is returned.
-}
instance (Ord fnident) => FnTypeChecked (Stmt fnident ident Pos) where
  type TypingFnIdent (Stmt fnident ident Pos) = fnident
  fnCheck (Skip _) = pure $ Skip BAny
  fnCheck (Decl wt v rv p) =
    [ Decl wt v rv' BAny
    | vt <- typeOf v
    , t' <- reportAt p t $ tryUnify vt t
    , rv' <- fnCheck rv
    , _ <- reportAt rv t $ tryUnify (getAnn rv') t'
    ]
    where
      t = fix wt
  fnCheck (Asgn lv rv p) = reportAt p BAny $ do
    rv' <- fnCheck rv
    lv' <- check lv
    let
      rvt = getAnn rv'
      lvt = getAnn lv'
    t <- reportAt rv lvt $ tryUnify rvt lvt
    when (t == BAny) (abortWith $ UnknownAssignmentError p)
    pure $ Asgn lv' rv' BAny
  fnCheck (Read lv p) = reportAt p BAny $ do
    lv' <- check lv
    let
      t = getAnn lv'
    unless
      (t `elem` readableTypes)
      (abortWith $ ExpectedReadableTypeError t (getPos lv))
    pure $ Read lv' BAny
  fnCheck (Free x p) = reportAt p BAny $ do
    x' <- check x
    let
      t = getAnn x'
    unless
      (isHeapAllocated t)
      (abortWith $ ExpectedHeapAllocatedTypeError t (getPos x))
    pure $ Free x' BAny
  fnCheck (Return x _) =
    [ Return x' (getAnn x')
    | x' <- check x
    ]
  fnCheck (Exit x _) =
    [ Exit x' BAny
    | (_, [x']) <- unifyExprsAt x BInt [x]
    ]
  fnCheck (Print x _) =
    [ Print x' BAny
    | x' <- check x
    ]
  fnCheck (PrintLn x _) =
    [ PrintLn x' BAny
    | x' <- check x
    ]
  fnCheck (IfElse x ifBody elseBody p) =
    [ IfElse x' ifBody' elseBody' t'
    | (_, [x']) <- unifyExprsAt x BBool [x]
    , (t, ifBody') <- unifyStmtsAt p BAny ifBody
    , (t', elseBody') <- unifyStmtsAt p t elseBody
    ]
  fnCheck (While x body p) =
    [ While x' body' t
    | (_, [x']) <- unifyExprsAt x BBool [x]
    , (t, body') <- unifyStmtsAt p BAny body
    ]
  fnCheck (BeginEnd body p) =
    [ BeginEnd body' t
    | (t, body') <- unifyStmtsAt p BAny body
    ]
