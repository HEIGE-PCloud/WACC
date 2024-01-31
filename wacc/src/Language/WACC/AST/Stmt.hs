{- |
WACC statements.
-}
module Language.WACC.AST.Stmt
  ( Stmt (..)
  , Stmts
  , LValue (..)
  , RValue (..)
  , PairElem (..)
  )
where

import Data.List.NonEmpty (NonEmpty)
import Language.WACC.AST.Expr (ArrayIndex, Expr)
import Language.WACC.AST.WType (WType)

{- |
The @fst@ or @snd@ element of a WACC @pair@.
-}
data PairElem value ident
  = -- | > fst <value>
    FstElem (value ident)
  | -- | > snd <value>
    SndElem (value ident)
  deriving (Eq, Show)

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data LValue ident
  = -- | > <ident>
    LVIdent ident
  | -- | > <ident>[<expr>]...
    LVArrayElem (ArrayIndex ident)
  | -- |
    -- > fst <lvalue>
    --
    -- or
    --
    -- > snd <lvalue>
    LVPairElem (PairElem LValue ident)
  deriving (Eq, Show)

{- |
A WACC @rvalue@, which is the source of an assignment statement.
-}
data RValue fnident ident
  = -- | > <expr>
    RVExpr (Expr ident)
  | -- | > [<expr>, ...]
    RVArrayLit [Expr ident] 
  | -- | > newpair(<expr>, <expr>)
    RVNewPair (Expr ident) (Expr ident)
  | -- |
    -- > fst <rvalue>
    --
    -- or
    --
    -- > snd <rvalue>
    RVPairElem (PairElem (RValue fnident) ident)
  | -- | > call <ident>(<expr>, ...)
    RVCall fnident [Expr ident]
  deriving (Eq, Show)

{- |
Individual WACC statements.
-}
data Stmt fnident ident
  = -- | > skip
    Skip
  | -- | > <type> <ident> = <rvalue>
    Decl WType ident (RValue fnident ident)
  | -- | > <lvalue> = <rvalue>
    Asgn (LValue ident) (RValue fnident ident)
  | -- | > read <lvalue>
    Read (LValue ident)
  | -- | > free <expr>
    Free (Expr ident)
  | -- | > return <expr>
    Return (Expr ident)
  | -- | > exit <expr>
    Exit (Expr ident)
  | -- | > print <expr>
    Print (Expr ident)
  | -- | > println <expr>
    PrintLn (Expr ident)
  | -- | > if <expr> then <stmt> else <stmt> fi
    IfElse (Expr ident) (Stmts fnident ident) (Stmts fnident ident)
  | -- | > while <expr> do <stmt> done
    While (Expr ident) (Stmts fnident ident)
  | -- | > begin <stmt> end
    BeginEnd (Stmts fnident ident)
  deriving (Eq, Show)

{- |
Sequences of WACC statements separated by @;@.
-}
type Stmts fnident ident = NonEmpty (Stmt fnident ident)
