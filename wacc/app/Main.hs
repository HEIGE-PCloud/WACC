module Main (main) where

-- import Lib

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Language.WACC.Parser.Token
import Text.Gigaparsec
import Text.Gigaparsec.Char
import qualified Data.Set as Set

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
compile filename = do
  source <- readFile filename
  case parse parser source of
    Failure err -> putStrLn err >> exitWithSyntaxError
    Success res -> print res >> exitSuccess

usageAndExit :: IO ()
usageAndExit = hPutStrLn stderr "Usage: compile <filename>" >> exitFailure

parser :: Parsec Integer
parser = fully intliter

charliter :: Parsec Char
charliter = charLiteral
stringliter :: Parsec String
stringliter = stringLiteral

intliter :: Parsec Integer
intliter = decimal

pairliter :: Parsec String
pairliter = string "null"

intsign :: Parsec Char
intsign = char '+' <|> char '-'

digit :: Parsec Char
digit = oneOf (Set.fromList ['0'..'9'])

boolliter :: Parsec Bool
boolliter = (string "true" $> True) <|> (string "false" $> False)
