{-# LANGUAGE DeriveDataTypeable #-}

{- |
The entrypoint for the compiler executable.
-}
module Language.WACC.IO (main, readProgramFile, runParse, runScopeAnalysis, runTypeCheck) where

import Control.Exception (handle)
import Data.List.Extra (replace)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.AST.Prog (Prog (..))
import Language.WACC.AST.WType (WType)
import Language.WACC.Error (Error, printError, semanticError)
import Language.WACC.Parser.Stmt (parseWithError, program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope
  ( Fnident
  , VarST
  , Vident
  , scopeAnalysis
  )
import Language.WACC.TAC (TACProgram, generateTAC)
import Language.WACC.TypeChecking (BType, checkTypes)
import Language.WACC.X86.ATNT (ATNT (..))
import Language.WACC.X86.Translate (translateProg)
import System.Console.CmdArgs
  ( Data
  , Typeable
  , argPos
  , cmdArgs
  , def
  , help
  , name
  , summary
  , typ
  , (&=)
  )
import System.Exit
  ( ExitCode (ExitFailure)
  , exitSuccess
  , exitWith
  )
import System.FilePath.Posix (takeBaseName)
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Error
  ( isDoesNotExistError
  , isFullError
  , isPermissionError
  )
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

type Result' =
  Result ([Error], ExitCode) (Prog WType Fnident Vident BType, VarST)

data Compile = Compile {file :: FilePath, parseOnly :: Bool}
  deriving (Show, Data, Typeable)

compileArgs :: Compile
compileArgs =
  Compile
    { file = def &= argPos 0 &= typ "FILE"
    , parseOnly = def &= help "Run in parse-only mode" &= name "parseOnly"
    }
    &= summary "WACC Compiler - Group 19"

{- |
The entrypoint.
-}
main :: IO ()
main = handleIOExceptions $ do
  args <- cmdArgs compileArgs
  let
    filename = file args
  let
    codeGen = not $ parseOnly args
  sourceCode <- readProgramFile filename
  let
    printError' = printError filename (lines sourceCode)
  case runParse sourceCode of
    Success ast -> if codeGen then runCodeGen filename ast else exitSuccess
    Failure (errs, exitCode) ->
      mapM_ (putStrLn . printError' semanticError) errs >> exitWith exitCode

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
runTypeCheck (ast, vars) = case checkTypes ast vars of
  (Just ast', []) -> Success (ast', vars)
  (_, errs) -> Failure (errs, semanticErrorCode)

runCodeGen :: String -> (Prog WType Fnident Vident BType, VarST) -> IO ()
runCodeGen path (ast, _) =
  withFile filename WriteMode (flip streamA $ translateProg tacProgram)
    *> exitSuccess
  where
    filename = takeBaseName path ++ ".s"
    tacProgram :: TACProgram Fnident Vident
    tacProgram = generateTAC ast
