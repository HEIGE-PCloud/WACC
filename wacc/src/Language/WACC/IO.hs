{-# LANGUAGE DeriveDataTypeable #-}

{- |
The entrypoint for the compiler executable.
-}
module Language.WACC.IO (main, readProgramFile) where

import Control.Exception (handle)
import Data.List.Extra (replace)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.AST (Prog, WType)
import Language.WACC.Error (Error, printError, semanticError)
import Language.WACC.Parser.Stmt (parseWithError, program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident, scopeAnalysis)
import Language.WACC.TypeChecking (checkTypes)
import System.Environment (getArgs)
import System.Exit
  ( ExitCode (ExitFailure)
  , exitFailure
  , exitSuccess
  , exitWith
  )
import System.FilePath.Posix (takeBaseName)
import System.IO.Error
import Text.Gigaparsec (Result (..))
import Text.Gigaparsec.Position (Pos)

ioErrorCode :: ExitCode
ioErrorCode = ExitFailure 255

syntaxErrorCode :: ExitCode
syntaxErrorCode = ExitFailure 100

semanticErrorCode :: ExitCode
semanticErrorCode = ExitFailure 200

handleIOExceptions :: IO a -> IO a
handleIOExceptions =
  handle
    ( (*> exitWith ioErrorCode)
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

type Result' = Result ([Error], ExitCode) (Prog WType Fnident Vident Pos, VarST)

{- |
The entrypoint.
-}
main :: IO ()
main = handleIOExceptions $ do
  args <- getArgs
  case args of
    [filename] -> do
      sourceCode <- readProgramFile filename
      let
        printError' = printError filename (lines sourceCode)
      case runParse sourceCode of
        Success ast -> runCodeGen filename ast
        Failure (errs, exitCode) ->
          mapM_ (putStrLn . printError' semanticError) errs >> exitWith exitCode
    _ -> usageAndExit

runParse
  :: String -> Result'
runParse sourceCode = case parseWithError (fully program) sourceCode of
  Failure err -> Failure ([err], syntaxErrorCode)
  Success ast -> runScopeAnalysis ast

runScopeAnalysis
  :: Prog WType String String Pos -> Result'
runScopeAnalysis ast = case scopeAnalysis ast of
  Failure errs -> Failure (errs, semanticErrorCode)
  Success res -> runTypeCheck res

runTypeCheck
  :: (Prog WType Fnident Vident Pos, VarST) -> Result'
runTypeCheck ast = case uncurry checkTypes ast of
  [] -> Success ast
  errs -> Failure (errs, semanticErrorCode)

runCodeGen :: String -> (Prog WType Fnident Vident Pos, VarST) -> IO ()
-- runCodeGen path _ = writeFile filename "TODO" >>= const exitSuccess
runCodeGen path _ = exitSuccess
  where
    filename = takeBaseName path ++ ".s"

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
