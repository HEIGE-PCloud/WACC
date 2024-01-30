{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

{- |
WACC statements.
-}
module Language.WACC.AST.Stmt
  ( Stmt (..)
  , LValue (..)
  , RValue (..)
  , PairElem (..)
  , RetType (..)
  )
where

import Data.Kind (Type)
import Language.WACC.AST.Annotation (Ann)
import Language.WACC.AST.Expr (ArrayElem)
import Language.WACC.AST.Ident (Ident)
import Language.WACC.AST.WType (WType)

{- |
The @fst@ or @snd@ element of a WACC @pair@.
-}
data
  PairElem
    (value :: Type -> Type -> Type)
    expr
    ident
  = -- | > fst <value>
    FstElem (value expr ident)
  | -- | > snd <value>
    SndElem (value expr ident)
  deriving (Eq, Show)

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data LValue expr ident
  = -- | > <ident>
    LVIdent ident
  | -- | > <ident>[<expr>]...
    LVArrayElem (ArrayElem expr ident)
  | -- |
    -- > fst <lvalue>
    --
    -- or
    --
    -- > snd <lvalue>
    LVPairElem (PairElem LValue expr ident)
  deriving (Eq, Show)

type LValue' stmt = LValue (StmtExpr stmt) (Ident Stmt stmt)

{- |
A WACC @rvalue@, which is the source of an assignment statement.
-}
data RValue fnident expr ident
  = -- | > <expr>
    RVExpr expr
  | -- | > [<expr>, ...]
    RVArrayLit [expr]
  | -- | > newpair(<expr>, <expr>)
    RVNewPair expr expr
  | -- |
    -- > fst <rvalue>
    --
    -- or
    --
    -- > snd <rvalue>
    RVPairElem (PairElem (RValue fnident) expr ident)
  | -- | > call <ident>(<expr>, ...)
    RVCall fnident [expr]
  deriving (Eq, Show)

type RValue' stmt = RValue (FnIdent stmt) (StmtExpr stmt) (Ident Stmt stmt)

{- |
Return type kind.
-}
type data RetType erasure
  = -- | Main program (not inside a function).
    Main
  | -- | Return value type.
    Ret (WType erasure)

{- |
WACC statements.
-}
class Stmt stmt where
  -- | Expression type.
  type StmtExpr stmt

  -- | Function identifier type.
  type FnIdent stmt

  -- | > skip
  skip :: Ann Stmt stmt ret stmt

  -- | > <type> <ident> = <rvalue>
  decl :: Ann Stmt stmt ret (Ident Stmt stmt -> RValue' stmt -> stmt)

  -- | > <lvalue> = <rvalue>
  asgn :: Ann Stmt stmt ret (LValue' stmt -> RValue' stmt -> stmt)

  -- | > read <lvalue>
  read :: Ann Stmt stmt ret (LValue' stmt -> stmt)

  -- | > free <expr>
  free :: Ann Stmt stmt ret (StmtExpr stmt -> stmt)

  -- | > return <expr>
  return :: Ann Stmt stmt (Ret t) (StmtExpr stmt -> stmt)

  -- | > exit <expr>
  exit :: Ann Stmt stmt ret (StmtExpr stmt -> stmt)

  -- | > print <expr>
  print :: Ann Stmt stmt ret (StmtExpr stmt -> stmt)

  -- | > println <expr>
  println :: Ann Stmt stmt ret (StmtExpr stmt -> stmt)

  -- | > if <expr> then <stmt> else <stmt> fi
  ifElse :: Ann Stmt stmt ret (StmtExpr stmt -> stmt -> stmt -> stmt)

  -- | > while <expr> do <stmt> done
  while :: Ann Stmt stmt ret (StmtExpr stmt -> stmt -> stmt)

  -- | > begin <stmt> end
  beginEnd :: Ann Stmt stmt ret (stmt -> stmt)

  -- | > <stmt>; <stmt>
  seq :: Ann Stmt stmt ret (stmt -> stmt -> stmt)
