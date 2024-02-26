{-# LANGUAGE PatternSynonyms #-}

{- |
Flattened WACC types.
-}
module Language.WACC.TAC.FType
  ( FType
  , pattern FInt
  , pattern FBool
  , pattern FChar
  , pattern FArray
  , pattern FPair
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
Flattened WACC array.
-}
pattern FArray :: FType
pattern FArray = WStringF

{- |
Flattened WACC pair.
-}
pattern FPair :: FType
pattern FPair = WErasedPairF

{-# COMPLETE FInt, FBool, FChar, FArray, FPair #-}

{- |
Get the size of an 'FType' in bytes.
-}
sizeOf :: FType -> Int
sizeOf FInt = 4
sizeOf FBool = 1
sizeOf FChar = 1
sizeOf FArray = 8
sizeOf FPair = 8

{- |
Flatten a 'BType' into an 'FType'.
-}
flatten :: BType -> FType
flatten BInt = FInt
flatten BBool = FBool
flatten BChar = FChar
flatten BString = FArray
flatten BErasedPair = FPair
flatten (BKnownPair _ _) = FPair
flatten (BArray _) = FArray
flatten BAny = error "attempted to flatten BAny"
