{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
WACC statements.
-}
module Language.WACC.AST.Stmt
  ( Stmt (..)
  , Stmts (..)
  , LValue (..)
  , RValue (..)
  , PairElem (..)
  )
where

import Data.List.NonEmpty (NonEmpty)
import GHC.IsList
import Language.WACC.AST.Expr (ArrayIndex, Expr)
import Language.WACC.AST.WType (WType)

{- |
The @fst@ or @snd@ element of a WACC @pair@.
-}
data PairElem ident ann
  = -- | > fst <value>
    FstElem (LValue ident ann) ann
  | -- | > snd <value>
    SndElem (LValue ident ann) ann
  deriving (Eq, Functor, Show)

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data LValue ident ann
  = -- | > <ident>
    LVIdent ident ann
  | -- | > <ident>[<expr>]...
    LVArrayElem (ArrayIndex ident ann) ann
  | -- |
    -- > fst <lvalue>
    --
    -- or
    --
    -- > snd <lvalue>
    LVPairElem (PairElem ident ann) ann
  deriving (Eq, Functor, Show)

{- |
A WACC @rvalue@, which is the source of an assignment statement.
-}
data RValue fnident ident ann
  = -- | > <expr>
    RVExpr (Expr ident ann) ann
  | -- | > [<expr>, ...]
    RVArrayLit [Expr ident ann] ann
  | -- | > newpair(<expr>, <expr>)
    RVNewPair (Expr ident ann) (Expr ident ann) ann
  | -- |
    -- > fst <rvalue>
    --
    -- or
    --
    -- > snd <rvalue>
    RVPairElem (PairElem ident ann) ann
  | -- | > call <ident>(<expr>, ...)
    RVCall fnident [Expr ident ann] ann
  deriving (Eq, Functor, Show)

{- |
Individual WACC statements.
-}
data Stmt fnident ident ann
  = -- | > skip
    Skip ann
  | -- | > <type> <ident> = <rvalue>
    Decl WType ident (RValue fnident ident ann) ann
  | -- | > <lvalue> = <rvalue>
    Asgn (LValue ident ann) (RValue fnident ident ann) ann
  | -- | > read <lvalue>
    Read (LValue ident ann) ann
  | -- | > free <expr>
    Free (Expr ident ann) ann
  | -- | > return <expr>
    Return (Expr ident ann) ann
  | -- | > exit <expr>
    Exit (Expr ident ann) ann
  | -- | > print <expr>
    Print (Expr ident ann) ann
  | -- | > println <expr>
    PrintLn (Expr ident ann) ann
  | -- | > if <expr> then <stmt> else <stmt> fi
    IfElse
      (Expr ident ann)
      (Stmts fnident ident ann)
      (Stmts fnident ident ann)
      ann
  | -- | > while <expr> do <stmt> done
    While (Expr ident ann) (Stmts fnident ident ann) ann
  | -- | > begin <stmt> end
    BeginEnd (Stmts fnident ident ann) ann
  deriving (Eq, Functor, Show)

{- |
Sequences of WACC statements separated by @;@.
-}
newtype Stmts fnident ident ann = Stmts
  { unwrap :: NonEmpty (Stmt fnident ident ann)
  }
  deriving (Eq, Functor, IsList, Show)
