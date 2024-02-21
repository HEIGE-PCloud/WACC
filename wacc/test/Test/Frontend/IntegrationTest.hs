module Test.Frontend.IntegrationTest
  ( integrationTestGroup
  )
where

import Data.List (isPrefixOf)
import System.Exit
import Test.Common (allTests, takeBaseName)
import Test.Lib.Program
import Test.Tasty

integrationTestGroup :: TestTree
integrationTestGroup = testGroup "integrationTest" allintegrationTest

data IntegrationTestKind = Valid | SyntaxError | SemanticError

data IntegrationTest = IntegrationTest
  { testName :: String
  , testPath :: FilePath
  , testKind :: IntegrationTestKind
  }

mkIntegrationTest :: FilePath -> IntegrationTest
mkIntegrationTest rawPath =
  IntegrationTest
    { testName = name
    , testPath = path
    , testKind = kind
    }
  where
    basePath = "wacc/test/wacc_examples"
    path = basePath ++ "/" ++ rawPath
    isValid = "valid" `isPrefixOf` rawPath
    isSemanticError = "invalid/semanticErr" `isPrefixOf` rawPath
    isSyntaxError = "invalid/syntaxErr" `isPrefixOf` rawPath
    kind
      | isValid = Valid
      | isSemanticError = SemanticError
      | isSyntaxError = SyntaxError
      | otherwise = error $ "Unknown test kind for " ++ rawPath
    name = takeBaseName rawPath

expectedExitCode :: IntegrationTestKind -> ExitCode
expectedExitCode Valid = ExitSuccess
expectedExitCode SyntaxError = ExitFailure 100
expectedExitCode SemanticError = ExitFailure 200

mkIntegrationTestCase :: IntegrationTest -> TestTree
mkIntegrationTestCase IntegrationTest {testName = name, testPath = path, testKind = kind} =
  testProgram
    name
    "./compile"
    [path, "--parseOnly"]
    (Just "..")
    (expectedExitCode kind)

allintegrationTest :: [TestTree]
allintegrationTest = map (mkIntegrationTestCase . mkIntegrationTest) allTests
