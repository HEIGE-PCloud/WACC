{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
WACC identifier type family.
-}
module Language.WACC.AST.Ident (Ident) where

import Data.Kind (Constraint, Type)
import Language.WACC.AST.WType (WType)

{- |
Identifier types for each AST type class @cls@ and each instance type @ins@.
-}
type family Ident (cls :: k -> Constraint) (ins :: k) :: WType erasure -> Type
