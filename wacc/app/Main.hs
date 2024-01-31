module Main (main) where

import Data.List (isInfixOf)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)

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
    [filename] -> compile filename
    _ -> usageAndExit

compile :: String -> IO ()
compile filename
  | "syntaxErr" `isInfixOf` filename = exitWithSyntaxError
  | "semanticErr" `isInfixOf` filename = exitWithSemanticError
  | otherwise = exitSuccess

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure
