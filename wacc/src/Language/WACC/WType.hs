{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeData #-}

{- |
WACC types.
-}
module Language.WACC.WType
  ( WType (..)
  , Erasure (..)
  , HeapAllocated
  , Ordered
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

{- |
WACC types which can be compared using @>@, @>=@, @<@ and @<=@.
-}
class Ordered (wtype :: WType erasure)

instance Ordered WChar

instance Ordered WInt

{- |
WACC types which are allocated on the heap.
-}
class HeapAllocated (wtype :: WType erasure)

instance HeapAllocated (WArray t)

instance HeapAllocated WErasedPair

instance HeapAllocated (WKnownPair t1 t2)
