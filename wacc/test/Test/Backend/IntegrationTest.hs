module Test.Backend.IntegrationTest where

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test (testGroup)
import Test.Common (validTests)
import Test.Lib.Program (TestProgram (..), ignoreOutput, testCompiler)
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

parseTestProgramMeta :: FilePath -> Parsec TestProgramMeta
parseTestProgramMeta path =
  mkTestProgramMeta path
    <$> ( parseDescription
            <~> option parseInput
            <~> option parseOutput
            <~> option parseExit
            <* parseProgram
        )

mkTestProgramMeta
  :: FilePath
  -> ((([Char], Maybe String), Maybe [Char]), Maybe Integer)
  -> TestProgramMeta
mkTestProgramMeta path (((d, i), o), e) =
  TestProgramMeta
    { filePath = path
    , description = d
    , input = fromMaybe "" i
    , output = fromMaybe "" o
    , exit = fromMaybe 0 e
    }

-- test :: IO ()
-- test = go [t | t <- validTests, not $ "advanced" `isInfixOf` t]

go :: [FilePath] -> IO ()
go [] = return ()
go (p : ps) = do
  let
    path = "/Users/pcloud/Code/WACC_19/wacc/test/wacc_examples/" ++ p
  code <- readFile path
  let
    res = parse (parseTestProgramMeta path) code
  case res of
    Failure e -> putStrLn p >> putStrLn e
    Success s' -> putStrLn p >> print s' >> putStrLn "" >> go ps

data TestProgramMeta = TestProgramMeta
  { filePath :: FilePath
  , description :: String
  , input :: String
  , output :: String
  , exit :: Integer
  }
  deriving (Show)

-- 1. Read source file and collect metadata
-- 2. Run the compiler on the source file and check its exit code is zero
-- 3. Run GCC on the generated assembly and check its exit code is zero
-- 4. Run the generate executable and check its exit code and output

compile :: FilePath -> TestProgram
compile path =
  TestProgram
    "./compile"
    [path]
    (Just "..")
    ExitSuccess
    ignoreOutput
    ignoreOutput

-- gcc -o test -z noexecstack out.s
assemble :: FilePath -> TestProgram
assemble filename =
  TestProgram
    "gcc"
    ["-o", "test", "-z", "noexecstack", filename]
    (Just "..")
    ExitSuccess
    ignoreOutput
    ignoreOutput

executable :: FilePath -> TestProgram
executable path =
  TestProgram
    path
    []
    (Just "..")
    (ExitFailure 1)
    ignoreOutput
    ignoreOutput

test1 =
  testCompiler
    "test name"
    ( compile
        "/Users/pcloud/Code/WACC_19/wacc/test/wacc_examples/valid/basic/exit/exit-1.wacc"
    )
    (assemble "exit-1.s")
    (executable "./test")

integrationTestGroup = testGroup "integrationTest" [test1]
