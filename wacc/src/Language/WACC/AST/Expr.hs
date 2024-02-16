{-# LANGUAGE DeriveFunctor #-}

{- |
WACC expressions.
-}
module Language.WACC.AST.Expr (WAtom (..), Expr (..), ArrayIndex (..)) where

{- |
WACC array indexing subexpressions.

> <ident>[<expr>]...
-}
data ArrayIndex ident ann = ArrayIndex ident [Expr ident ann] ann
  deriving (Eq, Show, Functor)

{- |
Atomic WACC expressions.
-}
data WAtom ident ann
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
    ArrayElem (ArrayIndex ident ann) ann
  deriving (Eq, Show, Functor)

{- |
Composite WACC expressions.
-}
data Expr ident ann
  = -- | > <atom>
    WAtom (WAtom ident ann) ann
  | -- | > !<expr>
    Not (Expr ident ann) ann
  | -- | > -<expr>
    Negate (Expr ident ann) ann
  | -- | > len <expr>
    Len (Expr ident ann) ann
  | -- | > ord <expr>
    Ord (Expr ident ann) ann
  | -- | > chr <expr>
    Chr (Expr ident ann) ann
  | -- | > <expr> * <expr>
    Mul (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> / <expr>
    Div (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> % <expr>
    Mod (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> + <expr>
    Add (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> - <expr>
    Sub (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> > <expr>
    GT (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> >= <expr>
    GTE (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> < <expr>
    LT (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> <= <expr>
    LTE (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> == <expr>
    Eq (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> != <expr>
    Ineq (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> && <expr>
    And (Expr ident ann) (Expr ident ann) ann
  | -- | > <expr> || <expr>
    Or (Expr ident ann) (Expr ident ann) ann
  deriving (Eq, Show, Functor)
