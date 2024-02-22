module Test.Backend.IntegrationTest where

import Data.List (isPrefixOf, isInfixOf)
import Text.Gigaparsec (Parsec, many, parseRepl, some, ($>), (<~>), parse, Result (Failure, Success), atomic, (<|>))
import Text.Gigaparsec.Char (item, string, satisfy, whitespaces)
import Text.Gigaparsec.Combinator (option)
import Text.Gigaparsec.Token.Descriptions (plain)
import Text.Gigaparsec.Token.Lexer
  ( IntegerParsers (decimal)
  , Lexeme (integer)
  , Lexer (lexeme, nonlexeme)
  , mkLexer
  , sym
  )
import Test.Common (validTests)


groupStrings :: [String] -> [[String]]
groupStrings ss = reverse (snd (foldl go ([], []) ss))
  where
    go :: ([String], [[String]]) -> String -> ([String], [[String]])
    go p@(cur, acc) str
      | null cur && not ("#" `isPrefixOf` str) = p
      | null cur = ([str], acc)
      | not ("#" `isPrefixOf` str) = ([], reverse cur : acc)
      | otherwise = (str : cur, acc)

raw :: String
raw =
  "# attempt dereference of a null pair by setting an element of it\n\n# Output:\n# #runtime_error#\n\n# Exit:\n# 255\n\n# Program:\n\nbegin\npair(int, int) p = null ;\nsnd p = 1\nend\n"

ls :: [String]
ls = lines raw

g :: [[String]]
g = groupStrings ls

-- parseDescription :: [[String]] -> String
-- parseDescription ss = unlines (map stripHash (head ss))

-- parseInput :: [[String]] -> Maybe String
-- parseInput ss =
--   (listToMaybe . catMaybes)
--     [stripPrefix "# Input: " s | [s] <- ss, "# Input: " `isPrefixOf` s]

-- parseOutput :: [[String]] -> Maybe String
-- parseOutput ss =
--   listToMaybe
--     [unlines (stripHash <$> s') | (s : s') <- ss, "# Output:" `isPrefixOf` s]

-- parseExit :: [[String]] -> Maybe Int
-- parseExit ss =
--   read . stripHash <$> listToMaybe [n | [s, n] <- ss, "# Exit:" `isPrefixOf` s]

-- stripHash :: String -> String
-- stripHash = drop 2

-- parseTestProgram :: [[String]] -> TestProgram
-- parseTestProgram ss =
--   TestProgram
--     { description = parseDescription ss,
--       input = fromMaybe "" (parseInput ss),
--       output = fromMaybe "" (parseOutput ss),
--       exit = fromMaybe 0 (parseExit ss)
--     }

lexer' :: Lexer
lexer' = mkLexer plain

lexeme' :: Lexeme
lexeme' = nonlexeme lexer'

integer' = integer lexeme'

decimal' :: Parsec Integer
decimal' = decimal integer'

s :: String -> Parsec ()
s = sym lexeme'

item' = satisfy (\x -> not (x `elem` "\n\r"))

newline = (atomic (string "\n") <|> string "\r\n") $> ()

hash :: Parsec ()
hash = s "#" *> (many $ string " ") $> ()

parseLine :: Parsec [Char]
parseLine = atomic (hash *> many item') <* newline

parseDescription :: Parsec [Char]
parseDescription = (concat <$> some parseLine) <* newline

parseInput :: Parsec String
parseInput = atomic (hash *> s "Input:") *> many item' <* many newline

parseOutput :: Parsec [Char]
parseOutput = atomic (hash *> s "Output:") *> newline *> (concat <$> many parseLine) <* newline

parseExit :: Parsec Integer
parseExit = atomic (hash *> s "Exit:") *> newline *> hash *> decimal' <* many newline

parseProgram :: Parsec ()
parseProgram = atomic ((option (hash *> (string "Program:")) $> ())) *> many item $> ()

parseTestProgram =
   parseDescription
    <~> ( (option parseInput))
    <~> ( (option parseOutput))
    <~> ( (option parseExit))
    <~> ( parseProgram)

p :: (Show a) => Parsec a -> String -> IO ()
p = parseRepl

_ = validTests
test = go [t | t <- validTests, not $ "advanced" `isInfixOf` t]
go :: [FilePath] -> IO ()
go [] = return ()
go (p : ps) = do
  code <- readFile ("/Users/pcloud/Code/WACC_19/wacc/test/wacc_examples/" ++ p)
  let res = parse parseTestProgram code
  case res of
    Failure e -> putStrLn p >> putStrLn e
    Success s -> go ps

data TestProgram = TestProgram
  { description :: String
  , input :: String
  , output :: String
  , exit :: Int
  }
  deriving (Show)
