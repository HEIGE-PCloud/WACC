{- |
WACC expressions.
-}
module Language.WACC.AST.Expr (WAtom (..), Expr (..), ArrayIndex (..)) where

{- |
WACC array indexing subexpressions.

> <ident>[<expr>]...
-}
data ArrayIndex ident = ArrayIndex ident [Expr ident] deriving (Eq, Show)

{- |
Atomic WACC expressions.
-}
data WAtom ident
  = -- | @int@ literals.
    IntLit Integer
  | -- | @bool@ literals.
    BoolLit Bool
  | -- | @char@ literals.
    CharLit Char
  | -- | @string@ literals.
    StringLit String
  | -- | > null
    Null
  | -- | > <ident>
    Ident ident
  | -- | > <ident>[<expr>]...
    ArrayElem (ArrayIndex ident)
  deriving (Eq, Show)

{- |
Composite WACC expressions.
-}
data Expr ident
  = -- | > <atom>
    WAtom (WAtom ident)
  | -- | > !<expr>
    Not (Expr ident)
  | -- | > -<expr>
    Negate (Expr ident)
  | -- | > len <expr>
    Len (Expr ident)
  | -- | > ord <expr>
    Ord (Expr ident)
  | -- | > chr <expr>
    Chr (Expr ident)
  | -- | > <expr> * <expr>
    Mul (Expr ident) (Expr ident)
  | -- | > <expr> / <expr>
    Div (Expr ident) (Expr ident)
  | -- | > <expr> % <expr>
    Mod (Expr ident) (Expr ident)
  | -- | > <expr> + <expr>
    Add (Expr ident) (Expr ident)
  | -- | > <expr> - <expr>
    Sub (Expr ident) (Expr ident)
  | -- | > <expr> > <expr>
    GT (Expr ident) (Expr ident)
  | -- | > <expr> >= <expr>
    GTE (Expr ident) (Expr ident)
  | -- | > <expr> < <expr>
    LT (Expr ident) (Expr ident)
  | -- | > <expr> <= <expr>
    LTE (Expr ident) (Expr ident)
  | -- | > <expr> == <expr>
    Eq (Expr ident) (Expr ident)
  | -- | > <expr> != <expr>
    Ineq (Expr ident) (Expr ident)
  | -- | > <expr> && <expr>
    And (Expr ident) (Expr ident)
  | -- | > <expr> || <expr>
    Or (Expr ident) (Expr ident)
  deriving (Eq, Show)