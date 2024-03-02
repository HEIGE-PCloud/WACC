{-# LANGUAGE TypeApplications #-}

module Test.Backend.IntegrationTest where

import Data.List (intercalate, isInfixOf, (\\))
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeBaseName)
import Test (TestName, TestTree, testGroup)
import Test.Common (takeTestName, validTests)
import Test.Lib.Program (TestProgram (..), ignoreOutput, runTestProgram)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.Providers (IsTest (run, testOptions), singleTest)
import Test.Tasty.Runners (resultSuccessful)
import Text.Gigaparsec
  ( Parsec
  , Result (Failure, Success)
  , atomic
  , many
  , parse
  , parseFromFile
  , some
  , ($>)
  , (<|>)
  , (<~>)
  )
import Text.Gigaparsec.Char (char, item, satisfy, string)
import Text.Gigaparsec.Combinator (manyN, option)
import Text.Gigaparsec.Token.Descriptions (plain)
import Text.Gigaparsec.Token.Lexer
  ( IntegerParsers (decimal)
  , Lexeme (integer)
  , Lexer (fully, nonlexeme)
  , mkLexer
  , sym
  )

enabledTests :: [FilePath]
enabledTests = allTests -- [t | t <- allTests, any (`isInfixOf` t) enabledPaths]
  where
    enabledPaths = ["basic", "variables", "sequence", "epxressions", "valid/if"]

allTests :: [FilePath]
allTests = [t | t <- validTests, not $ "advanced" `isInfixOf` t]

ignoredTests :: [FilePath]
ignoredTests = allTests \\ enabledTests

lexer :: Lexer
lexer = mkLexer plain

lexeme' :: Lexeme
lexeme' = nonlexeme lexer

decimal' :: Parsec Integer
decimal' = decimal (integer lexeme')

s :: String -> Parsec ()
s = sym lexeme'

fully' :: Parsec a -> Parsec a
fully' = fully lexer

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
    *> (intercalate "\n" <$> many parseLine)
    <* newline

parseExit :: Parsec Integer
parseExit = atomic (hash *> s "Exit:") *> newline *> hash *> decimal' <* many newline

parseProgram :: Parsec ()
parseProgram = atomic (option (hash *> string "Program:") $> ()) *> many item $> ()

parseTestProgramMetadata :: FilePath -> Parsec TestProgramMetadata
parseTestProgramMetadata path =
  mkTestProgramMetadata path
    <$> ( parseDescription
            <~> option parseInput
            <~> option parseOutput
            <~> option parseExit
            <* parseProgram
        )

fromInt :: Integer -> ExitCode
fromInt 0 = ExitSuccess
fromInt n = ExitFailure (fromIntegral n)

mkTestProgramMetadata
  :: FilePath
  -> ((([Char], Maybe String), Maybe [Char]), Maybe Integer)
  -> TestProgramMetadata
mkTestProgramMetadata path (((d, i), o), e) =
  TestProgramMetadata
    { sourceCodeFilePath = path
    , assemblyFilePath = takeBaseName path ++ ".s"
    , executableFilePath = takeBaseName path ++ ".out"
    , description = d
    , input = fromMaybe "" i
    , output = fromMaybe "" o
    , exit = maybe ExitSuccess fromInt e
    }

data TestProgramMetadata = TestProgramMetadata
  { sourceCodeFilePath :: FilePath
  , assemblyFilePath :: FilePath
  , executableFilePath :: FilePath
  , description :: String
  , input :: String
  , output :: String
  , exit :: ExitCode
  }
  deriving (Show)

timeout :: Int
timeout = 5000000

workingDirectory :: Maybe String
workingDirectory = Just ".."

compile :: TestProgramMetadata -> TestProgram
compile (TestProgramMetadata path _ _ _ _ _ _) =
  TestProgram
    "./compile"
    [path]
    workingDirectory
    Nothing
    ExitSuccess
    ignoreOutput
    ignoreOutput
    timeout

assemble :: TestProgramMetadata -> TestProgram
assemble (TestProgramMetadata _ path exe _ _ _ _) =
  TestProgram
    "gcc"
    ["-o", exe, "-z", "noexecstack", path]
    workingDirectory
    Nothing
    ExitSuccess
    ignoreOutput
    ignoreOutput
    timeout

executable :: TestProgramMetadata -> TestProgram
executable (TestProgramMetadata _ _ exe _ i o ecode) =
  TestProgram
    exe
    []
    workingDirectory
    (Just i)
    ecode
    ignoreOutput
    (outputChecker o)
    timeout

outputChecker :: String -> String -> (Bool, String)
outputChecker rawOutput realOutput = case res of
  Success p' -> case parse @String p' realOutput of
    Success _ -> (True, "")
    Failure err -> (False, "incorrect output from the compiled binary\n" ++ err)
  Failure err ->
    error
      ( "failed to parse expected output, check the backend integration test suite or the test WACC file\n"
          ++ err
      )
  where
    res = parse @String genOutputParser rawOutput

mkIntegrationTestCase :: FilePath -> TestTree
mkIntegrationTestCase rawPath =
  testCompiler
    name
    TestCompiler
      { metadata = iometa
      , testCompile = compile
      , testAssemble = assemble
      , testExecutable = executable
      }
  where
    basePath = "wacc/test/wacc_examples"
    path = basePath ++ "/" ++ rawPath
    name = takeTestName rawPath
    iometa = do
      res <- parseFromFile (parseTestProgramMetadata path) ("../" ++ path)
      return $ case res of
        Success meta -> meta
        Failure err -> error ("Failed to parse test program metadata\n" ++ err)

integrationTestGroup :: TestTree
integrationTestGroup =
  testGroup
    "integrationTest"
    ( (mkIntegrationTestCase <$> enabledTests)
        ++ (ignoreTest . mkIntegrationTestCase <$> ignoredTests)
    )

data TestCompiler = TestCompiler
  { metadata :: IO TestProgramMetadata
  , testCompile :: TestProgramMetadata -> TestProgram
  , testAssemble :: TestProgramMetadata -> TestProgram
  , testExecutable :: TestProgramMetadata -> TestProgram
  }

testCompiler
  :: TestName -> TestCompiler -> TestTree
testCompiler = singleTest

instance IsTest TestCompiler where
  run _ (TestCompiler iometa compiler assembler exe) _ = do
    meta <- iometa
    resCompile <- runTestProgram (compiler meta)
    ( if resultSuccessful resCompile
        then
          ( do
              resAssemble <- runTestProgram (assembler meta)
              if resultSuccessful resAssemble
                then runTestProgram (exe meta)
                else return resAssemble
          )
        else return resCompile
      )

  testOptions = return []

hexDigit :: Parsec Char
hexDigit = satisfy (`elem` (['0' .. '9'] ++ ['a' .. 'f']))

address :: Parsec ()
address = string "0x" *> (manyN 12 hexDigit $> ())

parseAddress :: Parsec (Parsec ())
parseAddress = string "#addrs#" $> address

runtimeError :: Parsec (Parsec ())
runtimeError = string "#runtime_error#" $> (string "fatal error: " *> many item $> ())

item'' :: Parsec (Parsec ())
item'' = (\c -> char c $> ()) <$> item

genOutputParser :: Parsec (Parsec [()])
genOutputParser = sequence <$> many (atomic parseAddress <|> atomic runtimeError <|> item'')
