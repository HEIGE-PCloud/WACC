{- AUTOCOLLECT.TEST -}
module Test.Golden
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.ByteString.Lazy.UTF8 (fromString)
import Language.WACC.Error (Error (Error), parseFromFileWithError)
import Language.WACC.Parser.Stmt (program)
import Language.WACC.Parser.Token (fully)
import Test.Common (syntaxErrTests, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Text.Gigaparsec (Result (Failure, Success))

goldenBasePath :: FilePath
goldenBasePath = "test/golden"

inputBasePath :: FilePath
inputBasePath = "test/wacc_examples/"

testNamePrefix :: String
testNamePrefix = "test.wacc_examples.invalid."

runSyntaxCheck :: FilePath -> TestTree
runSyntaxCheck path = goldenVsStringDiff testname diff goldenPath testAction
  where
    testname = drop (length testNamePrefix) (takeBaseName path)
    diff ref new = ["diff", "-u", ref, new]
    goldenPath = goldenBasePath ++ "/" ++ testname
    testAction = do
      input <- readFile path
      res <- parseFromFileWithError (fully program) path
      return (fromString (input ++ "\n\n" ++ syntaxCheck res))

syntaxCheck :: Result Error b -> String
syntaxCheck res = case res of
  Success _ -> error "syntax check should fail but succeeded"
  Failure (Error err) -> err

test =
  testGroup
    "goldenTests"
    [runSyntaxCheck (inputBasePath ++ test) | test <- syntaxErrTests]
