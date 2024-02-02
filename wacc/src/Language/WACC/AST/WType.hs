{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
WACC types.
-}
module Language.WACC.AST.WType
  ( WType (..)
  , WTypeF (..)
  )
where

import Data.Functor.Foldable.TH (makeBaseFunctor)

{- |
A WACC type.

These constructors are used as type-level indices using the @DataKinds@
language extension.
-}
data WType
  = -- | > bool
    WBool
  | -- | > char
    WChar
  | -- | > int
    WInt
  | -- | > string
    WString
  | -- | > pair
    WErasedPair
  | -- | > pair(<type>, <type>)
    WKnownPair WType WType
  | -- | > <type>[]
    WArray WType
  deriving (Eq, Show)

makeBaseFunctor ''WType

deriving instance (Eq r) => Eq (WTypeF r)

deriving instance (Show r) => Show (WTypeF r)
