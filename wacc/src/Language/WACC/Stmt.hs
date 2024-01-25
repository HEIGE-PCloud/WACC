{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

{- |
WACC statements.
-}
module Language.WACC.Stmt
  ( Stmt (..)
  , LValue (..)
  , RValue (..)
  , PairElem (..)
  , RetType (..)
  )
where

import Data.Kind (Type)
import Language.WACC.Annotation (Ann)
import Language.WACC.WType (Erasure, HeapAllocated, WType (..))

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
    (t :: WType erasure)
  where
  -- | @fst \<value\>@
  FstElem :: value expr ident (WKnownPair t1 t2) -> PairElem value expr ident t1
  -- | @snd \<value\>@
  SndElem :: value expr ident (WKnownPair t1 t2) -> PairElem value expr ident t2

{- |
A WACC @lvalue@, which is the target of an assignment statement.
-}
data
  LValue
    (expr :: WType erasure -> Type)
    (ident :: WType erasure -> Type)
    (t :: WType erasure)
  where
  -- | @\<ident\>@
  LVIdent :: ident t -> LValue expr ident t
  -- | @\<ident\>[\<expr\>]@
  LVArrayElem :: ident (WArray t) -> expr WInt -> LValue expr ident t
  -- | @fst \<lvalue\>@ or @snd \<lvalue\>@
  LVPairElem :: PairElem LValue expr ident t -> LValue expr ident t

type LValue' erasure stmt = LValue (Expr erasure stmt) (Ident erasure stmt)

{- |
A WACC @rvalue@, which is the source of an assignment statement.
-}
data
  RValue
    (expr :: WType erasure -> Type)
    (ident :: WType erasure -> Type)
    (t :: WType erasure)
  where
  -- | @\<expr\>@
  RVExpr :: expr t -> RValue expr ident t
  -- | @[\<expr\>, ...]@
  RVArrayLit :: [expr t] -> RValue expr ident (WArray t)
  -- | @newpair(\<expr\>, \<expr\>)@
  RVNewPair :: expr t1 -> expr t2 -> RValue expr ident (WKnownPair t1 t2)
  -- | @fst \<rvalue\>@ or @snd \<rvalue\>@
  RVPairElem :: PairElem RValue expr ident t -> RValue expr ident t
  -- | @call \<ident\>(\<expr\>, ...)@
  RVCall :: ident t -> [expr t] -> RValue expr ident t

type RValue' erasure stmt = RValue (Expr erasure stmt) (Ident erasure stmt)

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

  -- | Identifier type.
  type
    Ident (erasure :: Erasure) (stmt :: RetType erasure -> Type)
      :: WType erasure -> Type

  -- | @skip@
  skip :: Ann Stmt stmt (stmt ret)

  -- | @\<type\> \<ident\> = \<rvalue\>@
  decl
    :: Ann
        Stmt
        stmt
        (Ident erasure stmt t -> RValue' erasure stmt t -> stmt ret)

  -- | @\<lvalue\> = \<rvalue\>@
  asgn
    :: Ann
        Stmt
        stmt
        (LValue' erasure stmt t -> RValue' erasure stmt t -> stmt ret)

  -- | @read \<lvalue\>@
  read :: Ann Stmt stmt (LValue' erasure stmt t -> stmt ret)

  -- | @free \<expr\>@
  free :: (HeapAllocated t) => Ann Stmt stmt (Expr erasure stmt t -> stmt ret)

  -- | @return \<expr\>@
  return :: Ann Stmt stmt (Expr erasure stmt t -> stmt (Ret t))

  -- | @exit \<expr\>@
  exit :: Ann Stmt stmt (Expr erasure stmt WInt -> stmt Main)

  -- | @print \<expr\>@
  print :: Ann Stmt stmt (Expr erasure stmt t -> stmt ret)

  -- | @println \<expr\>@
  println :: Ann Stmt stmt (Expr erasure stmt t -> stmt ret)

  -- | @if \<expr\> then \<stmt\> else \<stmt\> fi@
  ifElse
    :: Ann
        Stmt
        stmt
        (Expr erasure stmt WBool -> stmt ret -> stmt ret -> stmt ret)

  -- | @while \<expr\> do \<stmt\> done@
  while :: Ann Stmt stmt (Expr erasure stmt WBool -> stmt ret -> stmt ret)

  -- | @begin \<stmt\> end@
  beginEnd :: Ann Stmt stmt (stmt ret -> stmt ret)

  -- | @\<stmt\>; \<stmt\>@
  seq :: Ann Stmt stmt (stmt ret -> stmt ret -> stmt ret)
