{-# LANGUAGE MonadComprehensions #-}

{- |
Type checking actions for WACC expressions.
-}
module Language.WACC.TypeChecking.Expr
  ( checkAtom
  , checkExpr
  , unifyExprs
  , checkArrayIndex
  )
where

import Control.Applicative (empty)
import Control.Monad (foldM)
import Language.WACC.AST.Expr
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.State
import Prelude hiding (GT, LT)

{- |
Type check a WACC array indexing subexpression.
-}
checkArrayIndex :: (Ord ident) => ArrayIndex ident -> TypingM ident BType
checkArrayIndex (ArrayIndex v xs) = typeOf v >>= flip (foldM go) xs
  where
    go (BArray t) x = t <$ unifyExprs BInt [x]
    go _ _ = empty

{- |
Type check an atomic WACC expression.
-}
checkAtom :: (Ord ident) => WAtom ident -> TypingM ident BType
checkAtom (IntLit _ _) = pure BInt
checkAtom (BoolLit _ _) = pure BBool
checkAtom (CharLit _) = pure BChar
checkAtom (StringLit _) = pure BString
checkAtom Null = pure (BKnownPair BAny BAny)
checkAtom (Ident v) = typeOf v
checkAtom (ArrayElem ai) = checkArrayIndex ai

{- |
@unifyExprs t0 [x1, x2, ..., xn]@ attempts to unify @x1@ with @t0@ to obtain
@t1@, which is unified with @x2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyExprs :: (Ord ident) => BType -> [Expr ident] -> TypingM ident BType
unifyExprs t xs = traverse checkExpr xs >>= foldM (flip tryUnify) t

{- |
Type check a composite WACC expression.
-}
checkExpr :: (Ord ident) => Expr ident -> TypingM ident BType
checkExpr (WAtom atom) = checkAtom atom
checkExpr (Not x) = unifyExprs BBool [x]
checkExpr (Negate x) = unifyExprs BInt [x]
checkExpr (Len x) = BInt <$ unifyExprs (BArray BAny) [x]
checkExpr (Ord x) = BInt <$ unifyExprs BChar [x]
checkExpr (Chr x) = BChar <$ unifyExprs BInt [x]
checkExpr (Mul x y) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Div x y) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Mod x y) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Add x y) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Sub x y) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (GT x y) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (GTE x y) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (LT x y) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (LTE x y) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (Eq x y) = BBool <$ unifyExprs BAny [x, y]
checkExpr (Ineq x y) = BBool <$ unifyExprs BAny [x, y]
checkExpr (And x y) = unifyExprs BBool [x] *> unifyExprs BBool [y]
checkExpr (Or x y) = unifyExprs BBool [x] *> unifyExprs BBool [y]
