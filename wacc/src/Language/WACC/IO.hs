{- |
The entrypoint for the compiler executable.
-}
module Language.WACC.IO (main, readProgramFile) where

import Control.Exception (handle)
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
import System.FilePath.Posix (takeBaseName)
import System.IO.Error
import Text.Gigaparsec (Result (..))
import Text.Gigaparsec.Position (Pos)

ioErrorCode :: Int
ioErrorCode = 255

exitWithIOErrorCode :: IO a
exitWithIOErrorCode = exitWith (ExitFailure ioErrorCode)

syntaxErrorCode :: Int
syntaxErrorCode = 100

exitWithSyntaxError :: IO a
exitWithSyntaxError = exitWith (ExitFailure syntaxErrorCode)

semanticErrorCode :: Int
semanticErrorCode = 200

exitWithSemanticError :: IO a
exitWithSemanticError = exitWith (ExitFailure semanticErrorCode)

handleIOExceptions :: IO a -> IO a
handleIOExceptions =
  handle
    ( (*> exitWithIOErrorCode)
        . hPutStrLn stderr
        . ("File I/O error: " ++)
        . getReason
    )
  where
    getReason err
      | isDoesNotExistError err = "input file does not exist"
      | isFullError err = "disk is full"
      | isPermissionError err = "input file cannot be read (permission error)"
      | otherwise = show err

{- |
Read a WACC source file, replacing each tab character with two spaces.
-}
readProgramFile :: FilePath -> IO String
readProgramFile = fmap (replace "\t" "  ") . readFile

{- |
The entrypoint.
-}
main :: IO ()
main = handleIOExceptions $ do
  args <- getArgs
  case args of
    [filename] -> runParse filename
    _ -> usageAndExit

runParse :: FilePath -> IO ()
runParse filename = do
  sourceCode <- readProgramFile filename
  let
    name = takeBaseName filename
  let
    res = parseWithError (fully program) sourceCode
  let
    printError' = printError filename (lines sourceCode)
  case res of
    Failure err -> putStrLn (printError' syntaxError err) >> exitWithSyntaxError
    Success ast -> runScopeAnalysis name printError' ast

runScopeAnalysis
  :: String -> (String -> Error -> String) -> Prog WType String String Pos -> IO ()
runScopeAnalysis filename printError' ast = case scopeAnalysis ast of
  Failure errs -> do
    mapM_ (putStrLn . printError' semanticError) errs
    exitWithSemanticError
  Success res -> runTypeCheck filename printError' res

runTypeCheck
  :: String
  -> (String -> Error -> String)
  -> (Prog WType Fnident Vident Pos, VarST)
  -> IO ()
runTypeCheck filename printError' ast = case uncurry checkTypes ast of
  [] -> runCodeGen filename ast
  errs -> do
    mapM_ (putStrLn . printError' semanticError) errs
    exitWithSemanticError

runCodeGen :: String -> (Prog WType Fnident Vident Pos, VarST) -> IO ()
runCodeGen name _ = writeFile (name ++ ".s") "TODO"

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
