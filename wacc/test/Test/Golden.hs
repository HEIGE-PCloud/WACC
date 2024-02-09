{- AUTOCOLLECT.TEST -}
module Test.Golden
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.ByteString.Lazy.UTF8 (fromString)
import Language.WACC.Error (Error (..), printError, semanticError, syntaxError)
import Language.WACC.Parser.Stmt (parseWithError, program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope (scopeAnalysis)
import Language.WACC.TypeChecking (checkTypes)
import Test.Common (semanticErrTests, syntaxErrTests, takeBaseName)
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
      let
        res = parseWithError (fully program) input
      return (fromString (input ++ "\n\n" ++ syntaxCheck res path (lines input)))

runSemanticCheck :: FilePath -> TestTree
runSemanticCheck path = goldenVsStringDiff testname diff goldenPath testAction
  where
    testname = drop (length testNamePrefix) (takeBaseName path)
    diff ref new = ["diff", "-u", ref, new]
    goldenPath = goldenBasePath ++ "/" ++ testname
    testAction = do
      input <- readFile path
      let
        res1 = parseWithError (fully program) input
      case res1 of
        Failure _ -> error "syntax check should succeed but failed"
        Success ast ->
          case scopeAnalysis ast of
            Failure errs ->
              return (fromString (input ++ "\n\n" ++ semanticCheck errs path (lines input)))
            Success res2 -> case uncurry checkTypes res2 of
              [] -> error "semantic check should fail but succeeded"
              errs ->
                return (fromString (input ++ "\n\n" ++ semanticCheck errs path (lines input)))

syntaxCheck :: Result Error b -> FilePath -> [String] -> String
syntaxCheck res path ls = case res of
  Success _ -> error "syntax check should fail but succeeded"
  Failure err -> printError path ls syntaxError err

semanticCheck :: [Error] -> FilePath -> [String] -> String
semanticCheck errs path ls = concat [printError path ls semanticError err | err <- errs]

test =
  testGroup
    "goldenTests"
    ( [runSyntaxCheck (inputBasePath ++ test) | test <- syntaxErrTests]
        ++ [runSemanticCheck (inputBasePath ++ test) | test <- semanticErrTests]
    )
