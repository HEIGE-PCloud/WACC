{-# LANGUAGE DeriveFunctor #-}

{- |
WACC expressions.
-}
module Language.WACC.AST.Expr (WAtom (..), Expr (..), ArrayIndex (..)) where

{- |
WACC array indexing subexpressions.

> <ident>[<expr>]...
-}
data ArrayIndex ann ident = ArrayIndex ident [Expr ann ident] ann
  deriving (Eq, Show, Functor)

{- |
Atomic WACC expressions.
-}
data WAtom ann ident
  = -- | @int@ literals.
    IntLit Integer ann
  | -- | @bool@ literals.
    BoolLit Bool ann
  | -- | @char@ literals.
    CharLit Char ann
  | -- | @string@ literals.
    StringLit String ann
  | -- | > null
    Null ann
  | -- | > <ident>
    Ident ident ann
  | -- | > <ident>[<expr>]...
    ArrayElem (ArrayIndex ann ident) ann
  deriving (Eq, Show, Functor)

{- |
Composite WACC expressions.
-}
data Expr ann ident
  = -- | > <atom>
    WAtom (WAtom ann ident) ann
  | -- | > !<expr>
    Not (Expr ann ident) ann
  | -- | > -<expr>
    Negate (Expr ann ident) ann
  | -- | > len <expr>
    Len (Expr ann ident) ann
  | -- | > ord <expr>
    Ord (Expr ann ident) ann
  | -- | > chr <expr>
    Chr (Expr ann ident) ann
  | -- | > <expr> * <expr>
    Mul (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> / <expr>
    Div (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> % <expr>
    Mod (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> + <expr>
    Add (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> - <expr>
    Sub (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> > <expr>
    GT (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> >= <expr>
    GTE (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> < <expr>
    LT (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> <= <expr>
    LTE (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> == <expr>
    Eq (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> != <expr>
    Ineq (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> && <expr>
    And (Expr ann ident) (Expr ann ident) ann
  | -- | > <expr> || <expr>
    Or (Expr ann ident) (Expr ann ident) ann
  deriving (Eq, Show, Functor)
