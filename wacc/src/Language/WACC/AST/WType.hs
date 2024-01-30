{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeData #-}

{- |
WACC types.
-}
module Language.WACC.AST.WType
  ( WType (..)
  , Erasure (..)
  )
where

{- |
Erasure kind.
-}
type data Erasure
  = Known
  | -- | A nested @pair@.
    Erased

{- |
A WACC type.

These constructors are used as type-level indices using the @DataKinds@
language extension.
-}
data WType (erasure :: Erasure) where
  -- | > bool
  WBool :: WType erasure
  -- | > char
  WChar :: WType erasure
  -- | > int
  WInt :: WType erasure
  -- | > string
  WString :: WType erasure
  -- | > pair
  WErasedPair :: WType Erased
  -- | > pair(<type>, <type>)
  WKnownPair :: WType Erased -> WType Erased -> WType erasure
  -- | > <type>[]
  WArray :: WType Known -> WType erasure

deriving instance Eq (WType erasure)

deriving instance Show (WType erasure)
