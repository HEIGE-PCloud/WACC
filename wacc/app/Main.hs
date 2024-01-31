module Main (main) where

import GHC.IO.Handle.FD (stderr)
import System.Environment (getArgs)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Exit (exitFailure, exitWith, ExitCode (ExitFailure), exitSuccess)
import Data.List (isInfixOf)

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

