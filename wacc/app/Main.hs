module Main (main) where

-- import Lib

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith, exitFailure)
import Text.Gigaparsec
import Text.Gigaparsec.Char

syntaxErrorCode :: Int
syntaxErrorCode = 100
semanticErrorCode :: Int
semanticErrorCode = 200

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> compile filename
    _          -> usageAndExit


compile :: String -> IO ()
compile filename = do
  source <- readFile filename
  case parse parser source of
    Failure err -> putStrLn err >> exitWith (ExitFailure syntaxErrorCode)
    Success res -> print res >> exitSuccess


usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure

parser :: Parsec Char
parser = item
