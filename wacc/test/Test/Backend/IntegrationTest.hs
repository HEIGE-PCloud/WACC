module Test.Backend.IntegrationTest where

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Test.Common (validTests)
import Text.Gigaparsec
  ( Parsec
  , Result (Failure, Success)
  , atomic
  , many
  , parse
  , some
  , ($>)
  , (<|>)
  , (<~>)
  )
import Text.Gigaparsec.Char (char, item, satisfy, string)
import Text.Gigaparsec.Combinator (option)
import Text.Gigaparsec.Token.Descriptions (plain)
import Text.Gigaparsec.Token.Lexer
  ( IntegerParsers (decimal)
  , Lexeme (integer)
  , Lexer (nonlexeme)
  , mkLexer
  , sym
  )

lexeme' :: Lexeme
lexeme' = nonlexeme $ mkLexer plain

decimal' :: Parsec Integer
decimal' = decimal (integer lexeme')

s :: String -> Parsec ()
s = sym lexeme'

item' :: Parsec Char
item' = satisfy (`notElem` "\n\r")

newline :: Parsec ()
newline = (atomic (string "\n") <|> string "\r\n") $> ()

hash :: Parsec ()
hash = s "#" *> many (char ' ') $> ()

parseLine :: Parsec [Char]
parseLine = atomic (hash *> many item') <* newline

parseDescription :: Parsec [Char]
parseDescription = (unlines <$> some parseLine) <* newline

parseInput :: Parsec String
parseInput = atomic (hash *> s "Input:") *> many item' <* many newline

parseOutput :: Parsec [Char]
parseOutput =
  atomic (hash *> s "Output:")
    *> newline
    *> (unlines <$> many parseLine)
    <* newline

parseExit :: Parsec Integer
parseExit = atomic (hash *> s "Exit:") *> newline *> hash *> decimal' <* many newline

parseProgram :: Parsec ()
parseProgram = atomic (option (hash *> string "Program:") $> ()) *> many item $> ()

parseTestProgram :: FilePath -> Parsec TestProgram
parseTestProgram path =
  mkTestProgram path
    <$> ( parseDescription
            <~> option parseInput
            <~> option parseOutput
            <~> option parseExit
            <* parseProgram
        )

mkTestProgram
  :: FilePath
  -> ((([Char], Maybe String), Maybe [Char]), Maybe Integer)
  -> TestProgram
mkTestProgram path (((d, i), o), e) =
  TestProgram
    { filePath = path
    , description = d
    , input = fromMaybe "" i
    , output = fromMaybe "" o
    , exit = fromMaybe 0 e
    }

test :: IO ()
test = go [t | t <- validTests, not $ "advanced" `isInfixOf` t]

go :: [FilePath] -> IO ()
go [] = return ()
go (p : ps) = do
  let
    path = "/Users/pcloud/Code/WACC_19/wacc/test/wacc_examples/" ++ p
  code <- readFile path
  let
    res = parse (parseTestProgram path) code
  case res of
    Failure e -> putStrLn p >> putStrLn e
    Success s' -> putStrLn p >> print s' >> putStrLn "" >> go ps

data TestProgram = TestProgram
  { filePath :: FilePath
  , description :: String
  , input :: String
  , output :: String
  , exit :: Integer
  }
  deriving (Show)
