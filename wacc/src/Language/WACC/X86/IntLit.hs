{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.WACC.X86.IntLit where

import Data.Int (Int16, Int32, Int64, Int8)
import Language.WACC.X86.Size (Size (..))

type IntLitQ = IntLit Q

type IntLitD = IntLit D

type IntLitW = IntLit W

type IntLitB = IntLit B

data IntLit (size :: Size) where
  IntLitQ :: Int64 -> IntLitQ
  IntLitD :: Int32 -> IntLitD
  IntLitW :: Int16 -> IntLitW
  IntLitB :: Int8 -> IntLitB

deriving instance Show (IntLit size)

deriving instance Eq (IntLit size)

deriving instance Ord (IntLit size)
