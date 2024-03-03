{-# LANGUAGE DeriveDataTypeable #-}

module Language.WACC.X86.Label where

import Data.Data (Data)
import Language.WACC.X86.Runtime (Runtime)

data Label = I Integer | R Runtime | S String
  deriving (Eq, Ord, Data, Show)
