module Main (main) where

import Data.DList (empty)
import Data.Functor.Foldable
import Data.Map ((!))
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.AST.Prog (Prog)
import Language.WACC.AST.WType (WType (..))
import Language.WACC.Error (parseWithError, printError)
import Language.WACC.Parser.Stmt (program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident, scopeAnalysis)
import Language.WACC.TypeChecking.BType (BType (..), fix)
import Language.WACC.TypeChecking.Prog (checkProg)
import Language.WACC.TypeChecking.State (runTypingM)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec

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
  case res of
    Failure err -> putStrLn (printError filename (lines sourceCode) err) >> exitWithSyntaxError
    Success ast -> runScopeAnalysis filename sourceCode ast

runScopeAnalysis :: String -> String -> Prog String String -> IO ()
runScopeAnalysis filename sourceCode ast = do
  case scopeAnalysis ast of
    Left errs -> do
      mapM (\err -> putStrLn (printError filename (lines sourceCode) err)) errs
      exitWithSemanticError
    Right res -> runTypeCheck res

runTypeCheck :: (Prog Fnident Vident, VarST) -> IO ()
runTypeCheck (ast, sb) = do
  let
    (_, _, err) = runTypingM (checkProg ast) (\ident -> ((fix (fst (sb ! ident))))) undefined
  if null err then exitSuccess else exitWithSemanticError

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
