{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WACC.X86.Size
  ( Size (..)
  , SizeToNat
  , LT
  , EQ
  , GT
  , GTE
  , LTE
  )
where

import Data.Type.Bool (Not, type (||))
import GHC.TypeLits (Nat)

data Size
  = Q
  | D
  | W
  | B

type family SizeToNat (a :: Size) :: Nat where
  SizeToNat Q = 64
  SizeToNat D = 32
  SizeToNat W = 16
  SizeToNat B = 8

type family LT (a :: Size) (b :: Size) :: Bool where
  LT B W = 'True
  LT B D = 'True
  LT B Q = 'True
  LT W D = 'True
  LT W Q = 'True
  LT D Q = 'True
  LT _ _ = 'False

type family EQ (a :: Size) (b :: Size) :: Bool where
  EQ Q Q = 'True
  EQ D D = 'True
  EQ W W = 'True
  EQ B B = 'True
  EQ _ _ = 'False

type family LTE (a :: Size) (b :: Size) :: Bool where
  LTE a b = a `LT` b || a `EQ` b

type family GT (a :: Size) (b :: Size) :: Bool where
  GT a b = Not (a `LTE` b)

type family GTE (a :: Size) (b :: Size) :: Bool where
  GTE a b = Not (a `LT` b)
