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
checkArrayIndex :: ArrayIndex ident -> TypingM fnident ident BType
checkArrayIndex (ArrayIndex v xs _) = typeOf v >>= flip (foldM go) xs
  where
    go (BArray t) x = t <$ unifyExprs BInt [x]
    go _ _ = empty

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
unifyExprs t xs = traverse checkExpr xs >>= foldM (flip tryUnify) t

{- |
Type check a composite WACC expression.
-}
checkExpr :: Expr ident -> TypingM fnident ident BType
checkExpr (WAtom atom _) = checkAtom atom
checkExpr (Not x _) = unifyExprs BBool [x]
checkExpr (Negate x _) = unifyExprs BInt [x]
checkExpr (Len x _) = BInt <$ unifyExprs (BArray BAny) [x]
checkExpr (Ord x _) = BInt <$ unifyExprs BChar [x]
checkExpr (Chr x _) = BChar <$ unifyExprs BInt [x]
checkExpr (Mul x y _) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Div x y _) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Mod x y _) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Add x y _) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (Sub x y _) = unifyExprs BInt [x] *> unifyExprs BInt [y]
checkExpr (GT x y _) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (GTE x y _) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (LT x y _) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (LTE x y _) =
  [BBool | t <- unifyExprs BAny [x, y], t `elem` orderedTypes]
checkExpr (Eq x y _) = BBool <$ unifyExprs BAny [x, y]
checkExpr (Ineq x y _) = BBool <$ unifyExprs BAny [x, y]
checkExpr (And x y _) = unifyExprs BBool [x] *> unifyExprs BBool [y]
checkExpr (Or x y _) = unifyExprs BBool [x] *> unifyExprs BBool [y]
