module Main (main) where

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.Error (parseWithError, printError)
import Language.WACC.Parser.Stmt (program)
import Language.WACC.Parser.Token (fully)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec
import Language.WACC.Semantic.Scope (scopeAnalysis)
import Language.WACC.AST.Prog (Prog)
import Language.WACC.AST.WType (WType (..))
import Language.WACC.Semantic.Scope (Fnident, Vident, VarST)
import Language.WACC.TypeChecking.Prog (checkProg)
import Language.WACC.TypeChecking.State (runTypingM)
import Data.DList (empty)
import Data.Map ((!))
import Data.Functor.Foldable
import Language.WACC.TypeChecking.BType (BType (..), fix)

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
    Success ast -> runScopeAnalysis ast

runScopeAnalysis :: Prog String String -> IO ()
runScopeAnalysis ast = do
  case scopeAnalysis ast of
    Left err -> exitWithSemanticError
    Right res -> runTypeCheck res

runTypeCheck :: (Prog Fnident Vident, VarST) -> IO ()
runTypeCheck (ast, sb) = do 
  let (_, _, err) = runTypingM (checkProg ast) (\ident -> ((fix (fst (sb ! ident))))) undefined 
  if null err then exitSuccess else exitWithSemanticError


usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
