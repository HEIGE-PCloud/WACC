module Main (main) where

-- import Lib

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Token.Descriptions (LexicalDesc (LexicalDesc, nameDesc, numericDesc, spaceDesc, symbolDesc, textDesc), plainName, plainNumeric, plainSpace, plainSymbol, plainText)
import Text.Gigaparsec.Token.Lexer (Lexer, fully, mkLexer)

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

lexer :: Lexer
lexer =
  mkLexer $
    LexicalDesc
      { nameDesc = plainName,
        symbolDesc = plainSymbol,
        numericDesc = plainNumeric,
        textDesc = plainText,
        spaceDesc = plainSpace
      }

parser = fully lexer item
