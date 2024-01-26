{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Annotation functor family.
-}
module Language.WACC.AST.Annotation (Ann) where

import Data.Kind (Constraint, Type)

{- |
Annotation functors for each AST type class @cls@ and each instance type @ins@.
-}
type family Ann (cls :: k -> Constraint) (ins :: k) :: Type -> Type
