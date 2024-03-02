{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Type checking actions for WACC expressions.
-}
module Language.WACC.TypeChecking.Expr
  ( unifyExprs
  , unifyExprsAt
  , tryUnifyExprs
  )
where

import Control.Monad (foldM, unless)
import Data.List (sort)
import Language.WACC.AST
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Class
import Language.WACC.TypeChecking.State
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT)

type instance Typed (ArrayIndex ident ann) = (BType, ArrayIndex ident BType)

{- |
@(t, ai') = check ai@ returns the element type @t@ and the 'ArrayIndex' AST node
annotated with the array type.
-}
instance TypeChecked (ArrayIndex ident Pos) where
  check (ArrayIndex v xs p) = do
    vt <- typeOf v
    let
      go (BArray t) x = t <$ reportAt x BInt (unifyExprs BInt [x])
      -- Move the caret one character to the left to align it with the opening
      -- bracket.
      go t x = abortActualOverridePos t (tryPred <$> getPos x)
      tryPred 0 = 0
      tryPred n = pred n
    t <- reportAt p (BArray BAny) $ foldM go vt xs
    xs' <- mapM check xs
    pure (t, ArrayIndex v xs' vt)

type instance Typed (WAtom ident ann) = WAtom ident BType

instance TypeChecked (WAtom ident Pos) where
  check i@(IntLit _ _) = pure $ BInt <$ i
  check b@(BoolLit _ _) = pure $ BBool <$ b
  check c@(CharLit _ _) = pure $ BChar <$ c
  check s@(StringLit _ _) = pure $ BString <$ s
  check (Null _) = pure . Null $ BKnownPair BAny BAny
  check (Ident v _) = Ident v <$> typeOf v
  check (ArrayElem ai _) = do
    (t, ai') <- check ai
    pure $ ArrayElem ai' t

{- |
@unifyExprs t0 [x1, x2, ..., xn]@ attempts to unify @x1@ with @t0@ to obtain
@t1@, which is unified with @x2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyExprs
  :: BType
  -> [Expr ident Pos]
  -> TypingM fnident ident (BType, [Expr ident BType])
unifyExprs t xs =
  [ (t', xs')
  | xs' <- traverse check xs
  , t' <- foldM (flip tryUnify) t $ sort (getAnn <$> xs')
  ]

{- |
Associate a 'Text.Gigaparsec.Position.Pos' with a @unifyExprs@ action.
-}
unifyExprsAt
  :: (HasPos a)
  => a
  -> BType
  -> [Expr ident Pos]
  -> TypingM fnident ident (BType, [Expr ident BType])
unifyExprsAt x t xs = reportAt (getPos x) t $ unifyExprs t xs

{- |
@tryUnifyExprs@ unifies multiple expressions like @unifyExprsAt@ but does not
report a type error on failure.
-}
tryUnifyExprs
  :: BType
  -> [Expr ident Pos]
  -> TypingM fnident ident (Maybe (BType, [Expr ident BType]))
tryUnifyExprs t xs =
  [ (,xs') <$> foldM unify t (getAnn <$> xs')
  | xs' <- traverse check xs
  ]

type instance Typed (Expr ident ann) = Expr ident BType

instance TypeChecked (Expr ident Pos) where
  check (WAtom atom _) =
    [ WAtom atom' (getAnn atom')
    | atom' <- check atom
    ]
  check (Not x p) =
    [ Not x' BBool
    | (_, [x']) <- unifyExprsAt p BBool [x]
    ]
  check (Negate x p) =
    [ Negate x' BInt
    | (_, [x']) <- unifyExprsAt p BInt [x]
    ]
  check (Len x p) =
    [ Len x' BInt
    | (_, [x']) <- unifyExprsAt p (BArray BAny) [x]
    ]
  check (Ord x p) =
    [ Ord x' BInt
    | (_, [x']) <- unifyExprsAt p BChar [x]
    ]
  check (Chr x p) =
    [ Chr x' BChar
    | (_, [x']) <- unifyExprsAt p BInt [x]
    ]
  check (Mul x y p) =
    [ Mul x' y' BInt
    | (_, [x']) <- unifyExprsAt p BInt [x]
    , (_, [y']) <- unifyExprsAt p BInt [y]
    ]
  check (Div x y p) =
    [ Div x' y' BInt
    | (_, [x']) <- unifyExprsAt p BInt [x]
    , (_, [y']) <- unifyExprsAt p BInt [y]
    ]
  check (Mod x y p) =
    [ Mod x' y' BInt
    | (_, [x']) <- unifyExprsAt p BInt [x]
    , (_, [y']) <- unifyExprsAt p BInt [y]
    ]
  check (Add x y p) =
    [ Add x' y' BInt
    | (_, [x']) <- unifyExprsAt p BInt [x]
    , (_, [y']) <- unifyExprsAt p BInt [y]
    ]
  check (Sub x y p) =
    [ Sub x' y' BInt
    | (_, [x']) <- unifyExprsAt p BInt [x]
    , (_, [y']) <- unifyExprsAt p BInt [y]
    ]
  check (GT x y p) = reportAt p BAny $ do
    (t, [x', y']) <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortWith $ ExpectedOrderedTypeError t p)
    pure $ GT x' y' BBool
  check (GTE x y p) = reportAt p BAny $ do
    (t, [x', y']) <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortWith $ ExpectedOrderedTypeError t p)
    pure $ GTE x' y' BBool
  check (LT x y p) = reportAt p BAny $ do
    (t, [x', y']) <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortWith $ ExpectedOrderedTypeError t p)
    pure $ LT x' y' BBool
  check (LTE x y p) = reportAt p BAny $ do
    (t, [x', y']) <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortWith $ ExpectedOrderedTypeError t p)
    pure $ LTE x' y' BBool
  check (Eq x y p) =
    [ Eq x' y' BBool
    | (_, [x', y']) <- unifyExprsAt p BAny [x, y]
    ]
  check (Ineq x y p) =
    [ Ineq x' y' BBool
    | (_, [x', y']) <- unifyExprsAt p BAny [x, y]
    ]
  check (And x y p) =
    [ And x' y' BBool
    | (_, [x']) <- unifyExprsAt p BBool [x]
    , (_, [y']) <- unifyExprsAt p BBool [y]
    ]
  check (Or x y p) =
    [ Or x' y' BBool
    | (_, [x']) <- unifyExprsAt p BBool [x]
    , (_, [y']) <- unifyExprsAt p BBool [y]
    ]
