{- AUTOCOLLECT.TEST -}

module Test.Integration
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.List (isPrefixOf)
import System.Exit
import Test.Common (allTests, takeBaseName)
import Test.Program
import Test.Tasty

-- test = testGroup "integrationTests" allIntegrationTests
test_ignoreTestBecause "Issue #123" = testGroup "integrationTests" allIntegrationTests

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
  testProgram name "./compile" [path] (Just "..") (expectedExitCode kind)

allIntegrationTests :: [TestTree]
allIntegrationTests = map (mkIntegrationTestCase . mkIntegrationTest) allTests
