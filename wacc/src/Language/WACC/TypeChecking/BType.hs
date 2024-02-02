{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Bounded WACC types with unification.
-}
module Language.WACC.TypeChecking.BType
  ( BType (.., BBool, BChar, BInt, BString, BErasedPair, BKnownPair, BArray)
  , FixedType
  , unify
  )
where

import GHC.Generics (Generic)
import Language.WACC.AST (WTypeF (..))

{- |
A bounded WACC type.
-}
data BType
  = -- | Any type. Unifies with all types.
    BAny
  | -- | Unknown type. Unifies with all types.
    BUnknown
  | -- | Fixed type.
    BFixed FixedType
  deriving (Eq, Generic, Show)

type FixedType = WTypeF BType

{- |
Fixed 'Language.WACC.AST.WType.WBool'.
-}
pattern BBool :: BType
pattern BBool = BFixed WBoolF

{- |
Fixed 'Language.WACC.AST.WType.WChar'.
-}
pattern BChar :: BType
pattern BChar = BFixed WCharF

{- |
Fixed 'Language.WACC.AST.WType.WInt'.
-}
pattern BInt :: BType
pattern BInt = BFixed WIntF

{- |
Fixed 'Language.WACC.AST.WType.WString'.
-}
pattern BString :: BType
pattern BString = BFixed WStringF

{- |
Fixed 'Language.WACC.AST.WType.WErasedPair'.
-}
pattern BErasedPair :: BType
pattern BErasedPair = BFixed WErasedPairF

{- |
Fixed 'Language.WACC.AST.WType.WKnownPair'.
-}
pattern BKnownPair :: BType -> BType -> BType
pattern BKnownPair t1 t2 = BFixed (WKnownPairF t1 t2)

{- |
Fixed 'Language.WACC.AST.WType.WArray'.
-}
pattern BArray :: BType -> BType
pattern BArray t = BFixed (WArrayF t)

{- |
Bounded type unification.

@unify actT expT@ attempts to unify an actual type @actT@ with an expected type
@expT@.
-}
unify :: BType -> BType -> Maybe BType
-- BAny and BUnknown always unify.
unify BAny t = Just t
unify t BAny = Just t
unify BUnknown t = Just t
unify t BUnknown = Just t
unify (BFixed t) (BFixed t') = BFixed <$> unifyW t t'
  where
    unifyW :: FixedType -> FixedType -> Maybe FixedType
    -- Identical types always unify.
    unifyW wt1 wt2
      | wt1 == wt2 = Just wt1
    -- Accept char[] where string is expected.
    unifyW (WArrayF bt) WStringF = WStringF <$ unify bt (BFixed WCharF)
    -- Erased pairs unify with known pairs.
    unifyW WErasedPairF pt@(WKnownPairF _ _) = Just pt
    unifyW pt@(WKnownPairF _ _) WErasedPairF = Just pt
    -- Type constructors distribute over unification.
    unifyW (WArrayF bt1) (WArrayF bt2) = WArrayF <$> unify bt1 bt2
    unifyW (WKnownPairF lbt1 rbt1) (WKnownPairF lbt2 rbt2) =
      WKnownPairF <$> unify lbt1 lbt2 <*> unify rbt1 rbt2
    -- Otherwise unification fails.
    unifyW _ _ = Nothing
