{- |
Type checking actions for WACC expressions.
-}
module Language.WACC.TypeChecking.Expr
  ( checkAtom
  , checkExpr
  , unifyExprs
  , unifyExprsAt
  , checkArrayIndex
  )
where

import Control.Monad (foldM, unless)
import Data.List (sort)
import Language.WACC.AST.Expr
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.State
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT)

{- |
Type check a WACC array indexing subexpression.
-}
checkArrayIndex :: ArrayIndex ident -> TypingM fnident ident BType
checkArrayIndex (ArrayIndex v xs p) =
  reportAt p (BArray BAny) $ typeOf v >>= flip (foldM go) xs
  where
    go (BArray t) x = t <$ unifyExprs BInt [x]
    go t _ = abortActual t

{- |
Type check an atomic WACC expression.
-}
checkAtom :: WAtom ident -> TypingM fnident ident BType
checkAtom (IntLit _ _) = pure BInt
checkAtom (BoolLit _ _) = pure BBool
checkAtom (CharLit _ _) = pure BChar
checkAtom (StringLit _ _) = pure BString
checkAtom (Null _) = pure (BKnownPair BAny BAny)
checkAtom (Ident v _) = typeOf v
checkAtom (ArrayElem ai _) = checkArrayIndex ai

{- |
@unifyExprs t0 [x1, x2, ..., xn]@ attempts to unify @x1@ with @t0@ to obtain
@t1@, which is unified with @x2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyExprs :: BType -> [Expr ident] -> TypingM fnident ident BType
unifyExprs t xs = traverse checkExpr xs >>= foldM (flip tryUnify) t . sort

{- |
Associate a 'Pos' with a @unifyExprs@ action.
-}
unifyExprsAt :: Pos -> BType -> [Expr ident] -> TypingM fnident ident BType
unifyExprsAt p t xs = reportAt p t $ unifyExprs t xs

{- |
Type check a composite WACC expression.
-}
checkExpr :: Expr ident -> TypingM fnident ident BType
checkExpr (WAtom atom _) = checkAtom atom
checkExpr (Not x p) = unifyExprsAt p BBool [x]
checkExpr (Negate x p) = unifyExprsAt p BInt [x]
checkExpr (Len x p) = BInt <$ unifyExprsAt p (BArray BAny) [x]
checkExpr (Ord x p) = BInt <$ unifyExprsAt p BChar [x]
checkExpr (Chr x p) = BChar <$ unifyExprsAt p BInt [x]
checkExpr (Mul x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
checkExpr (Div x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
checkExpr (Mod x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
checkExpr (Add x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
checkExpr (Sub x y p) = unifyExprsAt p BInt [x] *> unifyExprsAt p BInt [y]
-- FIXME: add specialised error message for expected comparable type
checkExpr (GT x y p) = reportAt p BAny $ do
  t <- unifyExprsAt p BAny [x, y]
  unless (t `elem` orderedTypes) (abortActual t)
  pure BBool
checkExpr (GTE x y p) = reportAt p BAny $ do
  t <- unifyExprsAt p BAny [x, y]
  unless (t `elem` orderedTypes) (abortActual t)
  pure BBool
checkExpr (LT x y p) = reportAt p BAny $ do
  t <- unifyExprsAt p BAny [x, y]
  unless (t `elem` orderedTypes) (abortActual t)
  pure BBool
checkExpr (LTE x y p) = reportAt p BAny $ do
  t <- unifyExprsAt p BAny [x, y]
  unless (t `elem` orderedTypes) (abortActual t)
  pure BBool
checkExpr (Eq x y p) = BBool <$ unifyExprsAt p BAny [x, y]
checkExpr (Ineq x y p) = BBool <$ unifyExprsAt p BAny [x, y]
checkExpr (And x y p) = unifyExprsAt p BBool [x] *> unifyExprsAt p BBool [y]
checkExpr (Or x y p) = unifyExprsAt p BBool [x] *> unifyExprsAt p BBool [y]
