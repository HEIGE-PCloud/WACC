{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=100 #-}

module Test.Arbitrary where

import Language.WACC.AST.WType (WType, WTypeF (..))
import Language.WACC.TypeChecking.BType (BType)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))

deriving via GenericArbitrary BType instance Arbitrary BType

deriving via GenericArbitrary WType instance Arbitrary WType

instance (Arbitrary r) => Arbitrary (WTypeF r) where
  arbitrary =
    oneof
      [ elements
          [ WBoolF
          , WCharF
          , WIntF
          , WStringF
          , WErasedPairF
          ]
      , WKnownPairF <$> arbitrary <*> arbitrary
      , WArrayF <$> arbitrary
      ]
