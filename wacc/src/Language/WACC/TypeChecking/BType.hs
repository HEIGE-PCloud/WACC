{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Bounded WACC types with unification.
-}
module Language.WACC.TypeChecking.BType
  ( BType (.., BBool, BChar, BInt, BString, BErasedPair, BKnownPair, BArray)
  , FixedType
  , fix
  , orderedTypes
  , readableTypes
  , isHeapAllocated
  , FnType (..)
  , unify
  )
where

import Data.Functor.Foldable (cata)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Language.WACC.AST (WType, WTypeF (..))

{- |
A bounded WACC type.
-}
data BType
  = -- | Any type. Unifies with all types.
    BAny
  | -- | Fixed type.
    BFixed FixedType
  deriving (Eq, Generic, Ord, Show)

{- |
A fixed WACC type.
-}
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

{-# COMPLETE
  BAny
  , BInt
  , BBool
  , BChar
  , BString
  , BErasedPair
  , BKnownPair
  , BArray
  #-}

{- |
Valid parameter types for @>@, @>=@, @<@ and @<=@.
-}
orderedTypes :: [BType]
orderedTypes = [BInt, BChar]

{- |
Valid @lvalue@ types for @read@.
-}
readableTypes :: [BType]
readableTypes = [BInt, BChar]

{- |
Predicate for valid parameter types for @free@.
-}
isHeapAllocated :: BType -> Bool
isHeapAllocated (BArray _) = True
isHeapAllocated BErasedPair = True
isHeapAllocated (BKnownPair _ _) = True
isHeapAllocated _ = False

{- |
Convert a 'WType' into a fixed 'BType'.
-}
fix :: WType -> BType
fix = cata BFixed

unifyParam :: BType -> BType -> Maybe BType
-- BAny and BUnknown always unify.
unifyParam BAny t = Just t
unifyParam t BAny = Just t
-- Identical types always unify.
unifyParam bt1 bt2
  | bt1 == bt2 = Just bt1
-- Erased pairs unify with known pairs.
unifyParam BErasedPair pt@(BKnownPair _ _) = Just pt
unifyParam pt@(BKnownPair _ _) BErasedPair = Just pt
-- Type constructors distribute over unification.
unifyParam (BArray bt1) (BArray bt2) = BArray <$> unifyParam bt1 bt2
unifyParam (BKnownPair lbt1 rbt1) (BKnownPair lbt2 rbt2) =
  BKnownPair <$> unifyParam lbt1 lbt2 <*> unifyParam rbt1 rbt2
-- Otherwise unification fails.
unifyParam _ _ = Nothing

{- |
Bounded type unification.

@unify actT expT@ attempts to unify an actual type @actT@ with an expected type
@expT@.
-}
unify :: BType -> BType -> Maybe BType
-- Accept char[] where string is expected.
unify (BArray bt) BString = BString <$ unifyParam bt BChar
unify bt1 bt2 = unifyParam bt1 bt2

{- |
This instance is used to provide an 'Control.Applicative.Alternative' instance
for 'Language.WACC.TypeChecking.State.TypingM'.
-}
instance Semigroup BType where
  bt1 <> bt2 = fromMaybe BAny $ unify bt1 bt2

{- |
A WACC function type.
-}
data FnType = FnType {paramTypes :: [BType], retType :: BType}
  deriving (Eq, Show)
