{-# LANGUAGE DeriveDataTypeable #-}

module Language.WACC.X86.Runtime where

import Data.Array (Ix)
import Data.Data (Data, Typeable)

data Runtime
  = PrintI
  | PrintB
  | PrintC
  | PrintS
  | PrintP
  | PrintLn
  | Free
  | Malloc
  | ReadI
  | ReadC
  | ErrOutOfMemory
  | ErrOutOfBounds
  | ErrOverflow
  | ErrDivByZero
  | ErrBadChar
  | ErrNull
  | Exit
  deriving (Ix, Eq, Ord, Typeable, Data, Show)
