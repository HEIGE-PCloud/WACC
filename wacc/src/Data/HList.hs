{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

{- |
Heterogeneous lists.
-}
module Data.HList (HList (..)) where

import Data.Kind (Type)

{- |
A heterogeneous list.
-}
data HList :: [Type] -> Type where
  -- | Empty list.
  HNil :: HList '[]
  -- | Cons cell.
  HCons :: t -> HList ts -> HList (t : ts)
