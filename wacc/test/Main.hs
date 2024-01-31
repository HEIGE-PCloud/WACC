module Main
  ( main
  )
where

import System.Exit
import Test.Program
import Test.Tasty
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Runners.AntXML (antXMLRunner)

basePath :: FilePath
basePath = "wacc/test/wacc_examples"

-- TODO: Collect all test cases from the wacc_examples folder

main :: IO ()
main =
  defaultMainWithIngredients [composeReporters consoleTestReporter antXMLRunner] $
    testGroup
      "integrationTests"
      [ validTests
      -- syntaxErrorTests
      -- semanticErrorTests
      ]

validTests :: TestTree
validTests =
  testGroup
    "valid"
    [ generateTestGroup
        ["valid", "advanced"]
        [ "binarySortTree"
        , "ticTacToe"
        , "hashTable"
        ]
        ExitSuccess
    , generateTestGroup
        ["valid", "array"]
        [ "arrayLength"
        , "lenArrayIndex"
        , "arrayIndexMayBeArrayIndex"
        , "emptyArrayReplace"
        , "stringFromArray"
        , "arrayEmpty"
        , "array"
        , "arrayLookup"
        , "emptyArrayPrint"
        , "free"
        , "arrayOnHeap"
        , "modifyString"
        , "printRef"
        , "emptyArrayNextLine"
        , "arraySimple"
        , "charArrayInStringArray"
        , "arrayNested"
        , "arrayPrint"
        , "emptyArrayScope"
        , "arrayBasic"
        , "emptyArrayAloneIsFine"
        ]
        ExitSuccess
    ]

syntaxErrorTests :: TestTree
syntaxErrorTests =
  testGroup
    "syntax"
    [ testProgram "Foo2" "pwd" [] (Just "..") (ExitFailure 100)
    ]

semanticErrorTests :: TestTree
semanticErrorTests =
  testGroup
    "semantic"
    [ testProgram "Foo3" "make" [] (Just "..") (ExitFailure 200)
    ]

generateTestCase :: String -> String -> ExitCode -> TestTree
generateTestCase name path = testProgram name "./compile" [path] (Just "..")

generateTestGroup :: [String] -> [String] -> ExitCode -> TestTree
generateTestGroup testnames names exitCode =
  testGroup
    (last testnames)
    ( map
        ( \name ->
            generateTestCase
              name
              (basePath ++ concatMap ('/' :) testnames ++ "/" ++ name ++ ".wacc")
              exitCode
        )
        names
    )
