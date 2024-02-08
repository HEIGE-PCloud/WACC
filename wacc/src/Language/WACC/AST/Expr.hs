{-# LANGUAGE DeriveFunctor #-}

{- |
WACC expressions.
-}
module Language.WACC.AST.Expr (WAtom (..), Expr (..), ArrayIndex (..)) where

import Text.Gigaparsec.Position (Pos)

{- |
WACC array indexing subexpressions.

> <ident>[<expr>]...
-}
data ArrayIndex ident = ArrayIndex ident [Expr ident] Pos
  deriving (Eq, Show, Functor)

{- |
Atomic WACC expressions.
-}
data WAtom ident
  = -- | @int@ literals.
    IntLit Integer Pos
  | -- | @bool@ literals.
    BoolLit Bool Pos
  | -- | @char@ literals.
    CharLit Char Pos
  | -- | @string@ literals.
    StringLit String Pos
  | -- | > null
    Null Pos
  | -- | > <ident>
    Ident ident Pos
  | -- | > <ident>[<expr>]...
    ArrayElem (ArrayIndex ident) Pos
  deriving (Eq, Show, Functor)

{- |
Composite WACC expressions.
-}
data Expr ident
  = -- | > <atom>
    WAtom (WAtom ident) Pos
  | -- | > !<expr>
    Not (Expr ident) Pos
  | -- | > -<expr>
    Negate (Expr ident) Pos
  | -- | > len <expr>
    Len (Expr ident) Pos
  | -- | > ord <expr>
    Ord (Expr ident) Pos
  | -- | > chr <expr>
    Chr (Expr ident) Pos
  | -- | > <expr> * <expr>
    Mul (Expr ident) (Expr ident) Pos
  | -- | > <expr> / <expr>
    Div (Expr ident) (Expr ident) Pos
  | -- | > <expr> % <expr>
    Mod (Expr ident) (Expr ident) Pos
  | -- | > <expr> + <expr>
    Add (Expr ident) (Expr ident) Pos
  | -- | > <expr> - <expr>
    Sub (Expr ident) (Expr ident) Pos
  | -- | > <expr> > <expr>
    GT (Expr ident) (Expr ident) Pos
  | -- | > <expr> >= <expr>
    GTE (Expr ident) (Expr ident) Pos
  | -- | > <expr> < <expr>
    LT (Expr ident) (Expr ident) Pos
  | -- | > <expr> <= <expr>
    LTE (Expr ident) (Expr ident) Pos
  | -- | > <expr> == <expr>
    Eq (Expr ident) (Expr ident) Pos
  | -- | > <expr> != <expr>
    Ineq (Expr ident) (Expr ident) Pos
  | -- | > <expr> && <expr>
    And (Expr ident) (Expr ident) Pos
  | -- | > <expr> || <expr>
    Or (Expr ident) (Expr ident) Pos
  deriving (Eq, Show, Functor)
