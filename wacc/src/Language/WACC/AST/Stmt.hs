{-# LANGUAGE DeriveFunctor #-}

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

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Language.WACC.AST.Expr (ArrayIndex, Expr)
import Language.WACC.AST.WType (WType)
import Text.Gigaparsec.Position (Pos)

{- |
The @fst@ or @snd@ element of a WACC @pair@.
-}
data PairElem ident
  = -- | > fst <value>
    FstElem (LValue ident)
  | -- | > snd <value>
    SndElem (LValue ident)
  deriving (Eq, Show, Functor)

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data LValue ident
  = -- | > <ident>
    LVIdent ident Pos
  | -- | > <ident>[<expr>]...
    LVArrayElem (ArrayIndex ident)
  | -- |
    -- > fst <lvalue>
    --
    -- or
    --
    -- > snd <lvalue>
    LVPairElem (PairElem ident)
  deriving (Eq, Show, Functor)

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
    RVPairElem (PairElem ident)
  | -- | > call <ident>(<expr>, ...)
    RVCall fnident [Expr ident] Pos
  deriving (Eq, Show, Functor)

instance Bifunctor RValue where
  -- first :: (a -> b) -> RValue a c -> RValue b c
  first f (RVExpr e) = RVExpr e
  first f (RVArrayLit es) = RVArrayLit es
  first f (RVNewPair e1 e2) = RVNewPair e1 e2
  first f (RVPairElem pe) = RVPairElem pe
  first f (RVCall fnident es pos) = RVCall (f fnident) es pos

  -- second :: (a -> b) -> RValue a c -> RValue b c
  second f (RVExpr e) = RVExpr (f <$> e)
  second f (RVArrayLit es) = RVArrayLit ((f <$>) <$> es)
  second f (RVNewPair e1 e2) = RVNewPair (f <$> e1) (f <$> e2)
  second f (RVPairElem pe) = RVPairElem (f <$> pe)
  second f (RVCall fnident es pos) = RVCall fnident ((f <$>) <$> es) pos

{- |
Individual WACC statements.
-}
data Stmt fnident ident
  = -- | > skip
    Skip
  | -- | > <type> <ident> = <rvalue>
    Decl WType ident (RValue fnident ident) Pos
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
