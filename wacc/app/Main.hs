module Main (main) where

-- import Lib

import qualified Data.Set as Set
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.Parser.Token
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Expr (Fixity (InfixL, InfixN, InfixR, Prefix), Prec (Atom), ops, precedence, (>+))

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

parser :: Parsec String
parser = fully intLiter

intLiter :: Parsec String
intLiter = show <$> decimal

boolLiter :: Parsec String
boolLiter = string "true" <|> string "false"

charLiter :: Parsec String
charLiter = show <$> charLiteral

stringLiter :: Parsec String
stringLiter = stringLiteral

pairLiter :: Parsec String
pairLiter = string "null"

ident :: Parsec String
ident = identifier

arrayElem :: Parsec String
arrayElem = concat <$> (ident <:> some (string "[" *> expr <* string "]"))

atom = intLiter <|> pairLiter <|> boolLiter <|> charLiter <|> stringLiteral <|> pairLiter <|> ident <|> arrayElem

showBinOp :: String -> String -> String -> String
showBinOp op x y = "(" ++ x ++ op ++ y ++ ")"

binSig :: String -> Parsec (String -> String -> String)
binSig op = string op $> showBinOp op

unarySig :: String -> Parsec (String -> String)
unarySig op = string op $> \x -> "(" ++ op ++ x ++ ")"

expr :: Parsec String
expr =
  precedence $
    Atom atom
      >+ ops Prefix [unarySig "!", unarySig "-", unarySig "len", unarySig "ord", unarySig "chr"]
      >+ ops InfixL [binSig "*", binSig "%", binSig "/"]
      >+ ops InfixL [binSig "+", binSig "-"]
      >+ ops InfixN [binSig "<", binSig "<=", binSig ">=", binSig ">"]
      >+ ops InfixN [binSig "==", binSig "!="]
      >+ ops InfixR [binSig "&&"]
      >+ ops InfixR [binSig "||"]
