{- AUTOCOLLECT.TEST -}

module Test.Parser.QuickCheck
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test.Tasty

import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord

test = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  ]
