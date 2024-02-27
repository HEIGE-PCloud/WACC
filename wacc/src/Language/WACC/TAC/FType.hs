{-# LANGUAGE PatternSynonyms #-}

{- |
Flattened WACC types.
-}
module Language.WACC.TAC.FType
  ( FType
  , pattern FInt
  , pattern FBool
  , pattern FChar
  , pattern FString
  , pattern FPtr
  , sizeOf
  , flatten
  )
where

import Data.Void (Void)
import Language.WACC.AST (WTypeF (..))
import Language.WACC.TypeChecking (BType (..))

{- |
A flattened WACC type.

'Void' is used to make the 'WKnownPairF' and 'WArrayF' constructors
uninstantiable.
-}
type FType = WTypeF Void

{- |
Flattened 'Language.WACC.AST.WType.WInt'.
-}
pattern FInt :: FType
pattern FInt = WIntF

{- |
Flattened 'Language.WACC.AST.WType.WBool'.
-}
pattern FBool :: FType
pattern FBool = WBoolF

{- |
Flattened 'Language.WACC.AST.WType.WChar'.
-}
pattern FChar :: FType
pattern FChar = WCharF

{- |
Flattened 'Language.WACC.AST.WType.WString'.
-}
pattern FString :: FType
pattern FString = WStringF

{- |
Flattened WACC array or pair.
-}
pattern FPtr :: FType
pattern FPtr = WErasedPairF

{-# COMPLETE FInt, FBool, FChar, FString, FPtr #-}

{- |
Get the size of an 'FType' in bytes.
-}
sizeOf :: FType -> Int
sizeOf FInt = 4
sizeOf FBool = 1
sizeOf FChar = 1
sizeOf FString = 8
sizeOf FPtr = 8

{- |
Flatten a 'BType' into an 'FType'.
-}
flatten :: BType -> FType
flatten BInt = FInt
flatten BBool = FBool
flatten BChar = FChar
flatten BString = FString
flatten BErasedPair = FPtr
flatten (BKnownPair _ _) = FPtr
flatten (BArray _) = FPtr
flatten BAny = error "attempted to flatten BAny"
