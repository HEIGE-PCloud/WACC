{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeData #-}

-- | WACC types.
module Language.WACC.WType
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
  -- | @bool@
  WBool :: WType erasure
  -- | @char@
  WChar :: WType erasure
  -- | @int@
  WInt :: WType erasure
  -- | @string@
  WString :: WType erasure
  -- | @pair@
  WErasedPair :: WType Erased
  -- | @pair(t1, t2)@
  WKnownPair :: WType Erased -> WType Erased -> WType erasure
  -- | @t[]@
  WArray :: WType Known -> WType erasure
