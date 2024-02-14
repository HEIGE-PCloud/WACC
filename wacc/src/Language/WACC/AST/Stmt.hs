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
data PairElem ann ident
  = -- | > fst <value>
    FstElem (LValue ann ident) ann
  | -- | > snd <value>
    SndElem (LValue ann ident) ann
  deriving (Eq, Show, Functor)

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data LValue ann ident
  = -- | > <ident>
    LVIdent ident ann
  | -- | > <ident>[<expr>]...
    LVArrayElem (ArrayIndex ann ident) ann
  | -- |
    -- > fst <lvalue>
    --
    -- or
    --
    -- > snd <lvalue>
    LVPairElem (PairElem ann ident) ann
  deriving (Eq, Show, Functor)

{- |
A WACC @rvalue@, which is the source of an assignment statement.
-}
data RValue ann fnident ident
  = -- | > <expr>
    RVExpr (Expr ann ident) ann
  | -- | > [<expr>, ...]
    RVArrayLit [Expr ann ident] ann
  | -- | > newpair(<expr>, <expr>)
    RVNewPair (Expr ann ident) (Expr ann ident) ann
  | -- |
    -- > fst <rvalue>
    --
    -- or
    --
    -- > snd <rvalue>
    RVPairElem (PairElem ann ident) ann
  | -- | > call <ident>(<expr>, ...)
    RVCall fnident [Expr ann ident] ann
  deriving (Eq, Show, Functor)

{- |
Individual WACC statements.
-}
data Stmt ann fnident ident
  = -- | > skip
    Skip ann
  | -- | > <type> <ident> = <rvalue>
    Decl WType ident (RValue ann fnident ident) ann
  | -- | > <lvalue> = <rvalue>
    Asgn (LValue ann ident) (RValue ann fnident ident) ann
  | -- | > read <lvalue>
    Read (LValue ann ident) ann
  | -- | > free <expr>
    Free (Expr ann ident) ann
  | -- | > return <expr>
    Return (Expr ann ident) ann
  | -- | > exit <expr>
    Exit (Expr ann ident) ann
  | -- | > print <expr>
    Print (Expr ann ident) ann
  | -- | > println <expr>
    PrintLn (Expr ann ident) ann
  | -- | > if <expr> then <stmt> else <stmt> fi
    IfElse
      (Expr ann ident)
      (Stmts ann fnident ident)
      (Stmts ann fnident ident)
      ann
  | -- | > while <expr> do <stmt> done
    While (Expr ann ident) (Stmts ann fnident ident) ann
  | -- | > begin <stmt> end
    BeginEnd (Stmts ann fnident ident) ann
  deriving (Eq, Show)

{- |
Sequences of WACC statements separated by @;@.
-}
newtype Stmts ann fnident ident = Stmts
  { unwrap :: NonEmpty (Stmt ann fnident ident)
  }
  deriving (Eq, Show, IsList)
