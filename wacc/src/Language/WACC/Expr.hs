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
  -- | @!x@
  not :: Ann UnExpr expr (expr WBool -> expr WBool)

  -- | @-x@
  negate :: Ann UnExpr expr (expr WInt -> expr WInt)

  -- | @len x@
  len :: Ann UnExpr expr (expr (WArray t) -> expr WInt)

  -- | @ord x@
  ord :: Ann UnExpr expr (expr WChar -> expr WInt)

  -- | @chr x@
  chr :: Ann UnExpr expr (expr WInt -> expr WChar)

{- |
Binary WACC expressions.
-}
class BinExpr (expr :: WType erasure -> Type) where
  -- | @x * y@
  mul :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | @x / y@
  div :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | @x % y@
  mod :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | @x + y@
  add :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | @x - y@
  sub :: Ann BinExpr expr (expr WInt -> expr WInt -> expr WInt)

  -- | @x > y@
  gt :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | @x >= y@
  gte :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | @x < y@
  lt :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | @x <= y@
  lte :: (Ordered t) => Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | @x == y@
  eq :: Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | @x != y@
  ineq :: Ann BinExpr expr (expr t -> expr t -> expr WBool)

  -- | @x && y@
  and :: Ann BinExpr expr (expr WBool -> expr WBool -> expr WBool)

  -- | @x || y@
  or :: Ann BinExpr expr (expr WBool -> expr WBool -> expr WBool)
