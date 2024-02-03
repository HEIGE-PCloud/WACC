{- AUTOCOLLECT.TEST -}
module Test.Golden
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.ByteString.Lazy.UTF8 (fromString)
import Language.WACC.Parser.Prog (prog)
import Test.Common (syntaxErrTests, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Gigaparsec (Result (Failure, Success), parse)


goldenBasePath :: FilePath
goldenBasePath = "test/golden"

inputBasePath :: FilePath
inputBasePath = "test/wacc_examples/"

testNamePrefix :: String
testNamePrefix = "test.wacc_examples.invalid."

runSyntaxCheck :: FilePath -> TestTree
runSyntaxCheck path = goldenVsString testname goldenPath testAction
  where
    testname = drop (length testNamePrefix) (takeBaseName path)
    goldenPath = goldenBasePath ++ "/" ++ testname
    testAction = do
      input <- readFile path
      return (fromString (syntaxCheck input))

syntaxCheck :: String -> String
syntaxCheck source = case parse prog source of
  Success _ -> error "syntax check should fail but succeeded"
  Failure err -> err

test = testGroup "goldenTests" [runSyntaxCheck (inputBasePath ++ test) | test <- syntaxErrTests]