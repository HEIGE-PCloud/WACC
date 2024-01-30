{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
WACC expressions.
-}
module Language.WACC.AST.Expr
  ( Expr (..)
  , ArrayElem (..)
  )
where

import Data.Kind (Type)
import Language.WACC.AST.Annotation (Ann)
import Language.WACC.AST.Ident (Ident)
import Language.WACC.AST.WType (Ordered, WType (..))

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

deriving instance
  (Eq (expr WInt), forall t'. Eq (ident (WArray t')))
  => Eq (ArrayElem expr ident t)

deriving instance
  (Show (expr WInt), forall t'. Show (ident (WArray t')))
  => Show (ArrayElem expr ident t)

{- |
WACC expressions.
-}
class Expr (expr :: WType erasure -> Type) where
  -- | @int@ literals.
  intLit :: Ann Expr expr (Int -> expr WInt)

  -- | @bool@ literals.
  boolLit :: Ann Expr expr (Bool -> expr WBool)

  -- | @char@ literals.
  charLit :: Ann Expr expr (Char -> expr WChar)

  -- | @string@ literals.
  stringLit :: Ann Expr expr (String -> expr WString)

  -- | > null
  null :: Ann Expr expr (expr (WKnownPair t1 t2))

  -- | > <ident>
  ident :: Ann Expr expr (Ident Expr expr t -> expr t)

  -- | > <ident>[<expr>]...
  arrayElem
    :: Ann Expr expr (ArrayElem expr (Ident Expr expr) t -> expr t)

  -- | > (<expr>)
  parens :: Ann Expr expr (expr t -> expr t)

  -- | > !<expr>
  not :: Ann Expr expr (expr WBool -> expr WBool)

  -- | > -<expr>
  negate :: Ann Expr expr (expr WInt -> expr WInt)

  -- | > len <expr>
  len :: Ann Expr expr (expr (WArray t) -> expr WInt)

  -- | > ord <expr>
  ord :: Ann Expr expr (expr WChar -> expr WInt)

  -- | > chr <expr>
  chr :: Ann Expr expr (expr WInt -> expr WChar)

  -- | > <expr> * <expr>
  mul :: Ann Expr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> / <expr>
  div :: Ann Expr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> % <expr>
  mod :: Ann Expr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> + <expr>
  add :: Ann Expr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> - <expr>
  sub :: Ann Expr expr (expr WInt -> expr WInt -> expr WInt)

  -- | > <expr> > <expr>
  gt :: (Ordered t) => Ann Expr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> >= <expr>
  gte :: (Ordered t) => Ann Expr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> < <expr>
  lt :: (Ordered t) => Ann Expr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> <= <expr>
  lte :: (Ordered t) => Ann Expr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> == <expr>
  eq :: Ann Expr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> != <expr>
  ineq :: Ann Expr expr (expr t -> expr t -> expr WBool)

  -- | > <expr> && <expr>
  and :: Ann Expr expr (expr WBool -> expr WBool -> expr WBool)

  -- | > <expr> || <expr>
  or :: Ann Expr expr (expr WBool -> expr WBool -> expr WBool)
