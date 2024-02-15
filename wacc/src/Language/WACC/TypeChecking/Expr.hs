{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
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

instance TypeChecked (ArrayIndex Pos ident) where
  check (ArrayIndex v xs p) =
    reportAt p (BArray BAny) $ typeOf v >>= flip (foldM go) xs
    where
      go (BArray t) x = t <$ reportAt x BInt (unifyExprs BInt [x])
      -- Move the caret one character to the left to align it with the opening
      -- bracket.
      go t x = abortActualOverridePos t (tryPred <$> getPos x)
      tryPred 0 = 0
      tryPred n = pred n

instance TypeChecked (WAtom Pos ident) where
  check (IntLit _ _) = pure BInt
  check (BoolLit _ _) = pure BBool
  check (CharLit _ _) = pure BChar
  check (StringLit _ _) = pure BString
  check (Null _) = pure (BKnownPair BAny BAny)
  check (Ident v _) = typeOf v
  check (ArrayElem ai _) = check ai

{- |
@unifyExprs t0 [x1, x2, ..., xn]@ attempts to unify @x1@ with @t0@ to obtain
@t1@, which is unified with @x2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyExprs :: BType -> [Expr Pos ident] -> TypingM fnident ident BType
unifyExprs t xs = traverse check xs >>= foldM (flip tryUnify) t . sort

{- |
Associate a 'Text.Gigaparsec.Position.Pos' with a @unifyExprs@ action.
-}
unifyExprsAt
  :: (HasPos a) => a -> BType -> [Expr Pos ident] -> TypingM fnident ident BType
unifyExprsAt x t xs = reportAt (getPos x) t $ unifyExprs t xs

{- |
@tryUnifyExprs@ unifies multiple expressions like @unifyExprsAt@ but does not
report a type error on failure.
-}
tryUnifyExprs
  :: BType -> [Expr Pos ident] -> TypingM fnident ident (Maybe BType)
tryUnifyExprs t xs = foldM unify t <$> traverse check xs

instance TypeChecked (Expr Pos ident) where
  check (WAtom atom _) = check atom
  check (Not x p) = unifyExprsAt p BBool [x]
  check (Negate x p) = unifyExprsAt p BInt [x]
  check (Len x p) = BInt <$ unifyExprsAt p (BArray BAny) [x]
  check (Ord x p) = BInt <$ unifyExprsAt p BChar [x]
  check (Chr x p) = BChar <$ unifyExprsAt p BInt [x]
  check (Mul x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
  check (Div x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
  check (Mod x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
  check (Add x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
  check (Sub x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
  check (GT x y p) = reportAt p BAny $ do
    t <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortWith $ ExpectedOrderedTypeError t p)
    pure BBool
  check (GTE x y p) = reportAt p BAny $ do
    t <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortActual t)
    pure BBool
  check (LT x y p) = reportAt p BAny $ do
    t <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortActual t)
    pure BBool
  check (LTE x y p) = reportAt p BAny $ do
    t <- unifyExprsAt p BAny [x, y]
    unless (t `elem` orderedTypes) (abortActual t)
    pure BBool
  check (Eq x y p) = BBool <$ unifyExprsAt p BAny [x, y]
  check (Ineq x y p) = BBool <$ unifyExprsAt p BAny [x, y]
  check (And x y p) = unifyExprsAt p BBool [x] *> unifyExprsAt p BBool [y]
  check (Or x y p) = unifyExprsAt p BBool [x] *> unifyExprsAt p BBool [y]
