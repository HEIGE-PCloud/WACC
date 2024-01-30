{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

{- |
WACC expressions.
-}
module Language.WACC.AST.Expr
  ( Expr (..)
  , ArrayElem (..)
  )
where

import Language.WACC.AST.Annotation (Ann)
import Language.WACC.AST.Ident (Ident)
import Language.WACC.AST.WType (WType (..))

{- |
WACC array elements.
-}
data ArrayElem expr ident
  = -- |
    -- The array identifier and the first index.
    --
    -- > <ident>[<expr>]
    Index1 ident expr
  | -- |
    -- A subsequent index after the first.
    --
    -- > <ident>[<expr>][<expr>]...
    -- >                ^^^^^^^^^^^
    IndexN (ArrayElem expr ident) expr
  deriving (Eq, Show)

{- |
WACC expressions.
-}
class Expr expr where
  -- | @int@ literals.
  intLit :: Ann Expr expr WInt (Int -> expr)

  -- | @bool@ literals.
  boolLit :: Ann Expr expr WBool (Bool -> expr)

  -- | @char@ literals.
  charLit :: Ann Expr expr WChar (Char -> expr)

  -- | @string@ literals.
  stringLit :: Ann Expr expr WString (String -> expr)

  -- | > null
  null :: Ann Expr expr (WKnownPair t1 t2) expr

  -- | > <ident>
  ident :: Ann Expr expr t (Ident Expr expr -> expr)

  -- | > <ident>[<expr>]...
  arrayElem
    :: Ann Expr expr t (ArrayElem expr (Ident Expr expr) -> expr)

  -- | > (<expr>)
  parens :: Ann Expr expr t (expr -> expr)

  -- | > !<expr>
  not :: Ann Expr expr WBool (expr -> expr)

  -- | > -<expr>
  negate :: Ann Expr expr WInt (expr -> expr)

  -- | > len <expr>
  len :: Ann Expr expr WInt (expr -> expr)

  -- | > ord <expr>
  ord :: Ann Expr expr WInt (expr -> expr)

  -- | > chr <expr>
  chr :: Ann Expr expr WChar (expr -> expr)

  -- | > <expr> * <expr>
  mul :: Ann Expr expr WInt (expr -> expr -> expr)

  -- | > <expr> / <expr>
  div :: Ann Expr expr WInt (expr -> expr -> expr)

  -- | > <expr> % <expr>
  mod :: Ann Expr expr WInt (expr -> expr -> expr)

  -- | > <expr> + <expr>
  add :: Ann Expr expr WInt (expr -> expr -> expr)

  -- | > <expr> - <expr>
  sub :: Ann Expr expr WInt (expr -> expr -> expr)

  -- | > <expr> > <expr>
  gt :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> >= <expr>
  gte :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> < <expr>
  lt :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> <= <expr>
  lte :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> == <expr>
  eq :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> != <expr>
  ineq :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> && <expr>
  and :: Ann Expr expr WBool (expr -> expr -> expr)

  -- | > <expr> || <expr>
  or :: Ann Expr expr WBool (expr -> expr -> expr)
