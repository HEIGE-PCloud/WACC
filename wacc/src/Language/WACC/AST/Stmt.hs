{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
WACC statements.
-}
module Language.WACC.AST.Stmt
  ( Stmt (..)
  , LValue (..)
  , RValue (..)
  , PairElem (..)
  , FnCall (..)
  , RetType (..)
  )
where

import Data.Kind (Type)
import Language.WACC.AST.Annotation (Ann)
import Language.WACC.AST.Expr (ArrayElem)
import Language.WACC.AST.Ident (Ident)
import Language.WACC.AST.WType (Erasure (Erased), HeapAllocated, WType (..))

{- |
The @fst@ or @snd@ element of a WACC @pair@.
-}
data
  PairElem
    ( value
        :: (WType erasure -> Type)
        -> (WType erasure -> Type)
        -> WType erasure
        -> Type
    )
    (expr :: WType erasure -> Type)
    (ident :: WType erasure -> Type)
    (t :: WType Erased)
  = -- | > fst <value>
    forall t'. FstElem (value expr ident (WKnownPair t t'))
  | -- | > snd <value>
    forall t'. SndElem (value expr ident (WKnownPair t' t))

{- |
This instance can only test constructor equality as both constructors use
existentially quantified type variables.
-}
instance Eq (PairElem value expr ident t) where
  FstElem _ == FstElem _ = True
  SndElem _ == SndElem _ = True
  _ == _ = False

deriving instance
  (forall t'. Show (value expr ident t')) => Show (PairElem value expr ident t)

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data
  LValue
    (expr :: WType erasure -> Type)
    (ident :: WType erasure -> Type)
    (t :: WType erasure)
  where
  -- | > <ident>
  LVIdent :: ident t -> LValue expr ident t
  -- | > <ident>[<expr>]...
  LVArrayElem :: ArrayElem expr ident t -> LValue expr ident t
  -- |
  -- > fst <lvalue>
  --
  -- or
  --
  -- > snd <lvalue>
  LVPairElem :: PairElem LValue expr ident t -> LValue expr ident t

deriving instance
  (Eq (expr WInt), forall t'. Eq (ident t')) => Eq (LValue expr ident t)

deriving instance
  (Show (expr WInt), forall t'. Show (ident t')) => Show (LValue expr ident t)

type LValue' erasure stmt = LValue (Expr erasure stmt) (Ident Stmt stmt)

{- |
An invocation of a WACC function.
-}
data
  FnCall
    (expr :: WType erasure -> Type)
    (args :: [WType erasure])
    (ret :: WType erasure)
  where
  -- | > <ident>()
  Call :: FnCall expr '[] ret
  -- |
  -- > <ident>(<expr>, ...)
  -- >         ^^^^^^^^^^^
  Arg :: FnCall expr args ret -> expr arg -> FnCall expr (arg : args) ret

deriving instance (forall t. Eq (expr t)) => Eq (FnCall expr args ret)

deriving instance (forall t. Show (expr t)) => Show (FnCall expr args ret)

{- |
A WACC @rvalue@, which is the source of an assignment statement.
-}
data
  RValue
    (fnident :: [WType erasure] -> WType erasure -> Type)
    (expr :: WType erasure -> Type)
    (ident :: WType erasure -> Type)
    (t :: WType erasure)
  where
  -- | > <expr>
  RVExpr :: expr t -> RValue fnident expr ident t
  -- | > [<expr>, ...]
  RVArrayLit :: [expr t] -> RValue fnident expr ident (WArray t)
  -- | > newpair(<expr>, <expr>)
  RVNewPair
    :: expr t1 -> expr t2 -> RValue fnident expr ident (WKnownPair t1 t2)
  -- |
  -- > fst <rvalue>
  --
  -- or
  --
  -- > snd <rvalue>
  RVPairElem
    :: PairElem (RValue fnident) expr ident t -> RValue fnident expr ident t
  -- | > call <ident>(<expr>, ...)
  RVCall :: fnident args t -> FnCall expr args t -> RValue fnident expr ident t

deriving instance
  (forall args t'. Show (fnident args t'), forall t'. Show (expr t'))
  => Show (RValue fnident expr ident t)

type RValue' erasure stmt =
  RValue (FnIdent erasure stmt) (Expr erasure stmt) (Ident Stmt stmt)

{- |
Return type kind.
-}
type data RetType erasure
  = -- | Main program (not inside a function).
    Main
  | -- | Return value type.
    Ret (WType erasure)

{- |
WACC statements indexed by function return type.
-}
class Stmt (stmt :: RetType erasure -> Type) where
  -- | Expression type.
  type
    Expr (erasure :: Erasure) (stmt :: RetType erasure -> Type)
      :: WType erasure -> Type

  -- | Function identifier type.
  type
    FnIdent
      (erasure :: Erasure)
      (stmt :: RetType erasure -> Type)
      :: [WType erasure] -> WType erasure -> Type

  -- | > skip
  skip :: Ann Stmt stmt (stmt ret)

  -- | > <type> <ident> = <rvalue>
  decl
    :: Ann
        Stmt
        stmt
        (Ident Stmt stmt t -> RValue' erasure stmt t -> stmt ret)

  -- | > <lvalue> = <rvalue>
  asgn
    :: Ann
        Stmt
        stmt
        (LValue' erasure stmt t -> RValue' erasure stmt t -> stmt ret)

  -- | > read <lvalue>
  read :: Ann Stmt stmt (LValue' erasure stmt t -> stmt ret)

  -- | > free <expr>
  free :: (HeapAllocated t) => Ann Stmt stmt (Expr erasure stmt t -> stmt ret)

  -- | > return <expr>
  return :: Ann Stmt stmt (Expr erasure stmt t -> stmt (Ret t))

  -- | > exit <expr>
  exit :: Ann Stmt stmt (Expr erasure stmt WInt -> stmt ret)

  -- | > print <expr>
  print :: Ann Stmt stmt (Expr erasure stmt t -> stmt ret)

  -- | > println <expr>
  println :: Ann Stmt stmt (Expr erasure stmt t -> stmt ret)

  -- | > if <expr> then <stmt> else <stmt> fi
  ifElse
    :: Ann
        Stmt
        stmt
        (Expr erasure stmt WBool -> stmt ret -> stmt ret -> stmt ret)

  -- | > while <expr> do <stmt> done
  while :: Ann Stmt stmt (Expr erasure stmt WBool -> stmt ret -> stmt ret)

  -- | > begin <stmt> end
  beginEnd :: Ann Stmt stmt (stmt ret -> stmt ret)

  -- | > <stmt>; <stmt>
  seq :: Ann Stmt stmt (stmt ret -> stmt ret -> stmt ret)
