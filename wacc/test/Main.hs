module Main
  ( main
  )
where

import System.Exit
import Test.Program
import Test.Tasty
import Test.Tasty.Runners.AntXML (antXMLRunner)

basePath :: FilePath
basePath = "wacc/test/wacc_examples"

-- TODO: Collect all test cases

main :: IO ()
main =
  defaultMainWithIngredients [antXMLRunner] $
    testGroup
      "Integration Tests"
      [ validTests
      -- syntaxErrorTests,
      -- semanticErrorTests
      ]

validTests :: TestTree
validTests =
  testGroup
    "Valid"
    [ validArrayTests
    ]

validArrayTests :: TestTree
validArrayTests = generateTestGroup "valid" "array" validArrays ExitSuccess

syntaxErrorTests :: TestTree
syntaxErrorTests =
  testGroup
    "Syntax Error"
    [ testProgram "Foo2" "make" [] Nothing (ExitFailure 100)
    ]

semanticErrorTests :: TestTree
semanticErrorTests =
  testGroup
    "Semantic Error"
    [ testProgram "Foo3" "make" [] Nothing (ExitFailure 200)
    ]

validArrays :: [String]
validArrays =
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

generateTestCase :: String -> String -> ExitCode -> TestTree
generateTestCase name path = testProgram name "compile" [path] Nothing

generateTestGroup :: String -> String -> [String] -> ExitCode -> TestTree
generateTestGroup t st names exitCode =
  testGroup
    (t ++ "." ++ st)
    ( map
        ( \name ->
            generateTestCase
              name
              (basePath ++ "/" ++ t ++ "/" ++ st ++ "/" ++ name ++ ".wacc")
              exitCode
        )
        names
    )
