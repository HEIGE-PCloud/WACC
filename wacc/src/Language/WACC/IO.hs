{- |
The entrypoint for the compiler executable.
-}
module Language.WACC.IO (main, readProgramFile) where

import Data.List.Extra (replace)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.AST (Prog, WType)
import Language.WACC.Error (Error, printError, semanticError, syntaxError)
import Language.WACC.Parser.Stmt (parseWithError, program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident, scopeAnalysis)
import Language.WACC.TypeChecking (checkTypes)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec (Result (..))
import Text.Gigaparsec.Position (Pos)

{- |
Read a WACC source file, replacing each tab character with two spaces.
-}
readProgramFile :: FilePath -> IO String
readProgramFile = fmap (replace "\t" "  ") . readFile

syntaxErrorCode :: Int
syntaxErrorCode = 100

exitWithSyntaxError :: IO a
exitWithSyntaxError = exitWith (ExitFailure syntaxErrorCode)

semanticErrorCode :: Int
semanticErrorCode = 200

exitWithSemanticError :: IO a
exitWithSemanticError = exitWith (ExitFailure semanticErrorCode)

{- |
The entrypoint.
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runParse filename
    _ -> usageAndExit

runParse :: String -> IO ()
runParse filename = do
  sourceCode <- readProgramFile filename
  let
    res = parseWithError (fully program) sourceCode
  let
    printError' = printError filename (lines sourceCode)
  case res of
    Failure err -> putStrLn (printError' syntaxError err) >> exitWithSyntaxError
    Success ast -> runScopeAnalysis printError' ast

runScopeAnalysis
  :: (String -> Error -> String) -> Prog WType String String Pos -> IO ()
runScopeAnalysis printError' ast = case scopeAnalysis ast of
  Failure errs -> do
    mapM_ (putStrLn . printError' semanticError) errs
    exitWithSemanticError
  Success res -> runTypeCheck printError' res

runTypeCheck
  :: (String -> Error -> String)
  -> (Prog WType Fnident Vident Pos, VarST)
  -> IO ()
runTypeCheck printError' ast = case uncurry checkTypes ast of
  [] -> exitSuccess
  errs -> do
    mapM_ (putStrLn . printError' semanticError) errs
    exitWithSemanticError

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
