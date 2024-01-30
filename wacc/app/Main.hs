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
import Text.Gigaparsec.Combinator
import Debug.Trace

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
parser = trace "parser" $ fully program

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

atom :: Parsec String
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

typeParser :: Parsec String
typeParser = trace "typeParser" $ baseOrArrayType <|> pairType

baseType :: Parsec String
baseType = trace "baseType" $ string "int" <|> string "bool" <|> string "char" <|> string "string"

-- arrayType :: Parsec String
-- arrayType = trace "arrayType" $ typeParser <* string "[]"

baseOrArrayType :: Parsec String
baseOrArrayType = trace "baseOrArrayType" $ liftA2 (++) baseType (showMaybe <$> option (string "[]"))

pairType :: Parsec String
pairType = trace "pairType" $ string "pair" *> string "(" *> pairElemType <* string "," *> pairElemType <* string ")"

pairElemType :: Parsec String
pairElemType = trace "pairElemType" $ baseOrArrayType <|> string "pair"

program :: Parsec String
-- program = trace "program" $ string "begin" *> liftA2 (++) (concat <$> many func) stmt <* string "end"
program = trace "program" $ string "begin" *>  stmt <* string "end"

showMaybe :: Maybe String -> String
showMaybe Nothing = ""
showMaybe (Just x) = x

func :: Parsec String
func = trace "func" $ param <* string "(" *> (showMaybe <$> option paramList) <* string ")" <* string "is" *> stmt <* string "end"

paramList :: Parsec String
paramList = trace "paramList" $ concat <$> (param <:> many (string "," *> param))

param :: Parsec String
param = trace "param" $ liftA2 (++) typeParser ident

stmt :: Parsec String
stmt = trace "stmt" $ 
      string "skip"
  <|> param <* string "=" *> rvalue
  <|> lvalue <* string "=" <* rvalue
  <|> string "read" *> lvalue
  <|> string "free" *> expr
  <|> string "return" *> expr
  <|> string "exit" *> expr
  <|> string "print" *> expr
  <|> string "println" *> expr
  <|> string "if" *> expr <* string "then" *> stmt <* string "else" *> stmt <* string "fi"
  <|> string "while" *> expr <* string "do" *> stmt <* string "done"
  <|> string "begin" *> stmt <* string "end"
  -- <|> stmt <* string ";" *> stmt

lvalue :: Parsec String
lvalue = trace "lvalue" $ ident <|> arrayElem <|> pairElem

rvalue :: Parsec String
rvalue = trace "rvalue" $ expr 
  <|> arrayLiter 
  <|> (string "newpair" *> string "(" *> expr <* string "," *> expr <* string ")")
  <|> pairElem
  <|> string "call" *> ident <* string "(" *> (showMaybe <$> option argList) <* string ")"

argList :: Parsec String
argList = trace "argList" $ concat <$> (expr <:> many (string "," *> expr))

pairElem :: Parsec String
pairElem = trace "pairElem" $ string "fst" *> lvalue <|> string "snd" *> lvalue

arrayLiter :: Parsec String
arrayLiter = trace "arrayLiter" $ string "[" *> (showMaybe <$> option argList) <* string "]"

