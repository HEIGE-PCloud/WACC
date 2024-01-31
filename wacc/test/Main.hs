module Main
  ( main,
  )
where

import System.Exit
import Test.Program
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup "Compilation with GHC" $
      [ testProgram "Foo" "make" [] (Just "..") (ExitSuccess)
      ]

validTests :: TestTree
validTests =
  testGroup
    "Valid tests"
    [ testProgram "Foo" "make" [] Nothing (ExitFailure 2)
    ]