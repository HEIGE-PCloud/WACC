module Main (main) where

-- import Lib

import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Token.Descriptions (BreakCharDesc (NoBreakChar), LexicalDesc (LexicalDesc, nameDesc, numericDesc, spaceDesc, symbolDesc, textDesc), NumericDesc (integerNumbersCanBeBinary, integerNumbersCanBeHexadecimal, integerNumbersCanBeOctal, leadingDotAllowed, leadingZerosAllowed, literalBreakChar, realNumbersCanBeBinary, realNumbersCanBeHexadecimal, realNumbersCanBeOctal, trailingDotAllowed), PlusSignPresence (PlusOptional), plainName, plainNumeric, plainSpace, plainSymbol, plainText, positiveSign)
import Text.Gigaparsec.Token.Lexer (Lexeme (names, integer), Lexer (lexeme), fully, mkLexer, IntegerParsers (decimal), TextParsers (ascii))
import qualified Data.Set as Set
import Text.Gigaparsec.Internal.Token.Lexer

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

lexer :: Text.Gigaparsec.Token.Lexer.Lexer
lexer =
  Text.Gigaparsec.Token.Lexer.mkLexer $
    LexicalDesc
      { nameDesc = plainName,
        symbolDesc = plainSymbol,
        numericDesc =
          plainNumeric
            { literalBreakChar = NoBreakChar,
              leadingDotAllowed = True,
              trailingDotAllowed = True,
              leadingZerosAllowed = True,
              positiveSign = PlusOptional,
              integerNumbersCanBeHexadecimal = False,
              integerNumbersCanBeOctal = False,
              integerNumbersCanBeBinary = False,
              realNumbersCanBeHexadecimal = False,
              realNumbersCanBeOctal = False,
              realNumbersCanBeBinary = False
            },
        textDesc = plainText,
        spaceDesc = plainSpace
      }

parser :: Parsec Integer
parser = fully lexer intliter


mylexeme :: Text.Gigaparsec.Token.Lexer.Lexeme
mylexeme = lexeme lexer
myinteger = integer mylexeme

myStringLiteral = stringLiteral mylexeme
myCharLiteral = charLiteral mylexeme

charliter :: Parsec Char
charliter = ascii myCharLiteral
stringliter :: Parsec String
stringliter = ascii myStringLiteral

intliter :: Parsec Integer
intliter = decimal myinteger

pairliter :: Parsec String
pairliter = string "null"

intsign :: Parsec Char
intsign = char '+' <|> char '-'

digit :: Parsec Char
digit = oneOf (Set.fromList ['0'..'9'])


boolliter :: Parsec Bool
boolliter = (string "true" $> True) <|> (string "false" $> False)

