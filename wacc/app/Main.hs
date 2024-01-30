module Main (main) where

-- import Lib

import qualified Data.Set as Set
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Expr (Fixity (InfixL, InfixN, InfixR, Prefix), Prec (Atom), ops, precedence, (>+))
import Text.Gigaparsec.Combinator
import Language.WACC.Parser.Token


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
parser = fully program

intLiter :: Parsec String
intLiter = show <$> decimal

boolLiter :: Parsec String
boolLiter = sym "true" <|> sym "false"

charLiter :: Parsec String
charLiter = show <$> charLiteral

stringLiter :: Parsec String
stringLiter = stringLiteral

pairLiter :: Parsec String
pairLiter = sym "null"

ident :: Parsec String
ident = identifier

arrayElem :: Parsec String
arrayElem = concat <$> (ident <:> some (sym "[" *> expr <* sym "]"))

atom :: Parsec String
atom = intLiter <|> pairLiter <|> boolLiter <|> charLiter <|> stringLiteral <|> pairLiter <|> ident <|> arrayElem

showBinOp :: String -> String -> String -> String
showBinOp op x y = "(" ++ x ++ op ++ y ++ ")"

binSig :: String -> Parsec (String -> String -> String)
binSig op = sym op $> showBinOp op

unarySig :: String -> Parsec (String -> String)
unarySig op = sym op $> \x -> "(" ++ op ++ x ++ ")"

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

typeParser :: Parsec String
typeParser = baseOrArrayType <|> pairType

baseType :: Parsec String
baseType = sym "int" <|> sym "bool" <|> sym "char" <|> sym "string"

-- arrayType :: Parsec String
-- arrayType = typeParser <* sym "[]"

baseOrArrayType :: Parsec String
baseOrArrayType = liftA2 (++) baseType (showMaybe <$> option (sym "[]"))

pairType :: Parsec String
pairType = sym "pair" *> sym "(" *> pairElemType <* sym "," *> pairElemType <* sym ")"

pairElemType :: Parsec String
pairElemType = baseOrArrayType <|> sym "pair"

program :: Parsec String
-- program = sym "begin" *> liftA2 (++) (concat <$> many func) stmt <* sym "end"
program = sym "begin" *>  stmt <* sym "end"

showMaybe :: Maybe String -> String
showMaybe Nothing = ""
showMaybe (Just x) = x

func :: Parsec String
func = param <* sym "(" *> (showMaybe <$> option paramList) <* sym ")" <* sym "is" *> stmt <* sym "end"

paramList :: Parsec String
paramList = concat <$> (param <:> many (sym "," *> param))

param :: Parsec String
param = liftA2 (++) typeParser ident

stmt :: Parsec String
stmt = liftA2 (++) (
      atomic (sym "skip")
  <|> atomic (param <* sym "=" *> rvalue)
  <|> lvalue <* sym "=" <* rvalue
  <|> sym "read" *> lvalue
  <|> sym "free" *> expr
  <|> sym "return" *> expr
  <|> sym "exit" *> expr
  <|> sym "print" *> expr
  <|> sym "println" *> expr
  <|> sym "if" *> expr <* sym "then" *> stmt <* sym "else" *> stmt <* sym "fi"
  <|> sym "while" *> expr <* sym "do" *> stmt <* sym "done"
  <|> sym "begin" *> stmt <* sym "end"
  ) stmtTail

stmtTail :: Parsec String
stmtTail = showMaybe <$> option (sym ";" *> stmt)

lvalue :: Parsec String
lvalue = ident <|> arrayElem <|> pairElem

rvalue :: Parsec String
rvalue = expr 
  <|> arrayLiter 
  <|> (sym "newpair" *> sym "(" *> expr <* sym "," *> expr <* sym ")")
  <|> pairElem
  <|> sym "call" *> ident <* sym "(" *> (showMaybe <$> option argList) <* sym ")"

argList :: Parsec String
argList = concat <$> (expr <:> many (sym "," *> expr))

pairElem :: Parsec String
pairElem = sym "fst" *> lvalue <|> sym "snd" *> lvalue

arrayLiter :: Parsec String
arrayLiter = sym "[" *> (showMaybe <$> option argList) <* sym "]"

