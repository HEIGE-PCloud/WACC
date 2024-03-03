{-# LANGUAGE DeriveDataTypeable #-}

module Language.WACC.X86.Label where

import Data.Data (Data)
import Language.WACC.X86.Runtime (Runtime)
import qualified Language.WACC.X86.Runtime as X86

data Label = I Integer | R Runtime | S String
  deriving (Eq, Ord, Data, Show)

printi :: Label
printi = R X86.PrintI

printc :: Label
printc = R X86.PrintC

printb :: Label
printb = R X86.PrintB

printp :: Label
printp = R X86.PrintP

prints :: Label
prints = R X86.PrintS

printLn :: Label
printLn = R X86.PrintLn

errNull :: Label
errNull = R X86.ErrNull

errOverflow :: Label
errOverflow = R X86.ErrOverflow

errDivByZero :: Label
errDivByZero = R X86.ErrDivByZero

exit :: Label
exit = R X86.Exit

malloc :: Label
malloc = R X86.Malloc

free :: Label
free = R X86.Free

errBadChar :: Label
errBadChar = R X86.ErrBadChar

errOutOfBounds :: Label
errOutOfBounds = R X86.ErrOutOfBounds

readI :: Label
readI = R X86.ReadI

readC :: Label
readC = R X86.ReadC
