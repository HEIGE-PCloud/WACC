module Main (main) where

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.Error (parseWithError, printError)
import Language.WACC.Parser.Stmt (program)
import Language.WACC.Parser.Token (fully)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec

syntaxErrorCode :: Int
syntaxErrorCode = 100

exitWithSyntaxError :: IO a
exitWithSyntaxError = exitWith (ExitFailure syntaxErrorCode)

-- semanticErrorCode :: Int
-- semanticErrorCode = 200

-- exitWithSemanticError :: IO a
-- exitWithSemanticError = exitWith (ExitFailure semanticErrorCode)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> compile filename
    _ -> usageAndExit

compile :: String -> IO ()
compile filename = do
  sourceCode <- readFile filename
  let
    res = parseWithError (fully program) sourceCode
  case res of
    Failure err -> putStrLn (printError filename (lines sourceCode) err) >> exitWithSyntaxError
    Success _ -> exitSuccess

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
