module Main (main) where

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.AST.Prog (Prog)
import Language.WACC.Error (Error, printError)
import Language.WACC.Parser.Stmt (parseWithError, program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident, scopeAnalysis)
import Language.WACC.TypeChecking (checkTypes)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec (Result (..))

syntaxErrorCode :: Int
syntaxErrorCode = 100

exitWithSyntaxError :: IO a
exitWithSyntaxError = exitWith (ExitFailure syntaxErrorCode)

semanticErrorCode :: Int
semanticErrorCode = 200

exitWithSemanticError :: IO a
exitWithSemanticError = exitWith (ExitFailure semanticErrorCode)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runParse filename
    _ -> usageAndExit

runParse :: String -> IO ()
runParse filename = do
  sourceCode <- readFile filename
  let
    res = parseWithError (fully program) sourceCode
  let
    printError' = printError filename (lines sourceCode)
  case res of
    Failure err -> putStrLn (printError' err) >> exitWithSyntaxError
    Success ast -> runScopeAnalysis printError' ast

runScopeAnalysis :: (Error -> String) -> Prog String String -> IO ()
runScopeAnalysis printError' ast = case scopeAnalysis ast of
  Failure errs -> do
    mapM_ (putStrLn . printError') errs
    exitWithSemanticError
  Success res -> runTypeCheck printError' res

runTypeCheck :: (Error -> String) -> (Prog Fnident Vident, VarST) -> IO ()
runTypeCheck printError' ast = case uncurry checkTypes ast of
  [] -> exitSuccess
  errs -> do
    mapM_ (putStrLn . printError') errs
    exitWithSemanticError

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
