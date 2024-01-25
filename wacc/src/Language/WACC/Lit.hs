{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

{- |
WACC literals.
-}
module Language.WACC.Lit (Lit (..)) where

import Data.Kind (Type)
import Language.WACC.Annotation (Ann)
import Language.WACC.WType (WType (..))

{- |
WACC literals permitted in expressions.
-}
class Lit (lit :: WType erasure -> Type) where
  -- | @int@ literals.
  int :: Ann Lit lit (Int -> lit WInt)

  -- | @bool@ literals.
  bool :: Ann Lit lit (Bool -> lit WBool)

  -- | @char@ literals.
  char :: Ann Lit lit (Char -> lit WChar)

  -- | @string@ literals.
  string :: Ann Lit lit (String -> lit WString)

  -- | > null
  null :: Ann Lit lit (lit (WKnownPair t1 t2))
