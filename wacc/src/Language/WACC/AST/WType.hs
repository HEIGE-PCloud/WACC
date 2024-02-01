{- |
WACC types.
-}
module Language.WACC.AST.WType
  ( WType (..)
  )
where

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
