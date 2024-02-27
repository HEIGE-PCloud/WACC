{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Test
  ( module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.QuickCheck
  , module Text.Gigaparsec.Position
  , (?==)
  )
where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Gigaparsec.Position (Pos)

(?==) :: (Eq a, Show a) => a -> a -> Bool
x ?== y = unsafePerformIO $ (x == y) <$ print x <* print y
