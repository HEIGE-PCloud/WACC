{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
WACC expressions.
-}
module Language.WACC.Expr
  ( AtomExpr (..)
  , UnExpr (..)
  , BinExpr (..)
  , ArrayElem (..)
  )
where

import Data.Kind (Type)
import Language.WACC.Annotation (Ann)
import Language.WACC.WType (Erasure, Ordered, WType (..))

{- |
WACC array elements.
-}
data
  ArrayElem
    (expr :: WType erasure -> Type)
    (ident :: WType erasure -> Type)
    (t :: WType erasure)
  where
  -- |
  -- The array identifier and the first index.
  --
  -- > <ident>[<expr>]
  Index1 :: ident (WArray t) -> expr WInt -> ArrayElem expr ident t
  -- |
  -- A subsequent index after the first.
  --
  -- > <ident>[<expr>][<expr>]...
  -- >                ^^^^^^^^^^^
  IndexN
    :: ArrayElem expr ident (WArray t) -> expr WInt -> ArrayElem expr ident t

{- |
Atomic WACC expressions.
-}
class AtomExpr (expr :: WType erasure -> Type) where
  -- | Identifier type.
  type
    Ident (erasure :: Erasure) (expr :: WType erasure -> Type)
      :: WType erasure -> Type

  -- | @int@ literals.
  intLit :: Ann AtomExpr expr (Int -> expr WInt)

  -- | @bool@ literals.
  boolLit :: Ann AtomExpr expr (Bool -> expr WBool)

  -- | @char@ literals.
  charLit :: Ann AtomExpr expr (Char -> expr WChar)

  -- | @string@ literals.
  stringLit :: Ann AtomExpr expr (String -> expr WString)

  -- | > null
  null :: Ann AtomExpr expr (expr (WKnownPair t1 t2))

  -- | > <ident>
  ident :: Ann AtomExpr expr (Ident erasure expr t -> expr t)

  -- | > <ident>[<expr>]...
  arrayElem
    :: Ann AtomExpr expr (ArrayElem expr (Ident erasure expr) t -> expr t)

  -- | > (<expr>)
  parens :: Ann AtomExpr expr (expr t -> expr t)

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
