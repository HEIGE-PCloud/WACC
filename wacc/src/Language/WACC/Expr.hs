{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{- |
WACC expressions.
-}
module Language.WACC.Expr (UnExpr (..), BinExpr (..)) where

import Data.Kind (Type)
import Language.WACC.Annotation (Ann)
import Language.WACC.WType (Ordered, WType (..))

{- |
Unary WACC expressions.
-}
class UnExpr (expr :: WType erasure -> Type) where
  -- | > !<expr>
  not :: Ann UnExpr expr (expr WBool -> expr WBool)

  -- | > -<expr>
  negate :: Ann UnExpr expr (expr WInt -> expr WInt)

  -- | > len <expr>
  len :: Ann UnExpr expr (expr (WArray t) -> expr WInt)

  -- | > ord <expr>
  ord :: Ann UnExpr expr (expr WChar -> expr WInt)

  -- | > chr <expr>
  chr :: Ann UnExpr expr (expr WInt -> expr WChar)

{- |
Binary WACC expressions.
-}
class BinExpr (expr :: WType erasure -> Type) where
  -- | > <expr> * <expr>
  mul :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> / <expr>
  div :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> % <expr>
  mod :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> + <expr>
  add :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> - <expr>
  sub :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> > <expr>
  gt :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> >= <expr>
  gte :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> < <expr>
  lt :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> <= <expr>
  lte :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> == <expr>
  eq :: Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> != <expr>
  ineq :: Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> && <expr>
  and :: Ann BinExpr expr (expr WBool -> expr WBool -> expr WBool)

  -- | > <expr> || <expr>
  or :: Ann BinExpr expr (expr WBool -> expr WBool -> expr WBool)
