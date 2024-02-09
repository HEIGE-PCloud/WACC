{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WACC.Error (printError, parseWithError, Error (..)) where

import Data.List.Extra ((!?))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Set (Set, toList)
import Data.String (IsString (fromString))
import Language.WACC.Parser.Token (keywords, nonlexeme, operators)
import Text.Gigaparsec (Parsec, Result, parse, ($>))
import Text.Gigaparsec.Char (whitespace)
import Text.Gigaparsec.Errors.DefaultErrorBuilder
  ( StringBuilder (..)
  , disjunct
  , endOfInputDefault
  , expectedDefault
  , namedDefault
  , rawDefault
  , unexpectedDefault
  )
import Text.Gigaparsec.Errors.ErrorBuilder
  ( ErrorBuilder
      ( ErrorInfoLines
      , ExpectedItems
      , ExpectedLine
      , Item
      , LineInfo
      , Message
      , Messages
      , Position
      , Source
      , UnexpectedLine
      )
  , combineExpectedItems
  , combineMessages
  , endOfInput
  , expected
  , format
  , lineInfo
  , message
  , named
  , numLinesAfter
  , numLinesBefore
  , pos
  , raw
  , reason
  , source
  , specialisedError
  , unexpected
  , unexpectedToken
  , vanillaError
  )
import Text.Gigaparsec.Errors.TokenExtractors
import Text.Gigaparsec.Position (Pos)
import qualified Text.Gigaparsec.Token.Lexer as T

parseWithError :: Parsec a -> String -> Result Error a
parseWithError = parse

data Error = Error {errorMessage :: String, position :: Pos, width :: Word}

printError :: FilePath -> [String] -> Error -> String
printError filePath sourceCodeLines (Error errMsg p@(row, col) w) =
  unlines [printHeader filePath p, errMsg, lineStr]
  where
    prevLine = printLine (row - 2, col) sourceCodeLines
    currline = printLine (row - 1, col) sourceCodeLines
    caretLine = replicate (fromIntegral col - 1) ' ' ++ replicate (fromIntegral w) '^'
    nextLine = printLine (row, col) sourceCodeLines
    lineStr =
      unlines $
        map ("> " ++) $
          catMaybes [prevLine, currline, Just caretLine, nextLine]

printPos :: Pos -> String
printPos (x, y) = "(line " ++ show x ++ ", column " ++ show y ++ ")"

printLine :: Pos -> [String] -> Maybe String
printLine (row, _) ls = ls !? fromIntegral row

printHeader :: FilePath -> Pos -> String
printHeader filePath p = "In " ++ filePath ++ " " ++ printPos p ++ ":"

instance ErrorBuilder Error where
  format :: Position Error -> Source Error -> ErrorInfoLines Error -> Error
  format p _ (ls, w) =
    Error
      { errorMessage = unlines ls
      , position = p
      , width = w
      }

  type Position Error = Pos
  type Source Error = Maybe StringBuilder

  pos :: Word -> Word -> Position Error
  pos x y = (x, y)
  source :: Maybe FilePath -> Source Error
  source = fmap fromString

  type ErrorInfoLines Error = ([String], Word)
  vanillaError
    :: UnexpectedLine Error
    -> ExpectedLine Error
    -> Messages Error
    -> LineInfo Error
    -> ErrorInfoLines Error
  vanillaError unexpectedLine expectedLine msgs w = (catMaybes [unexpectedLine, expectedLine] ++ msgs, w)

  specialisedError :: Messages Error -> LineInfo Error -> ErrorInfoLines Error
  specialisedError msgs w = (msgs, w)

  type ExpectedItems Error = Maybe StringBuilder
  type Messages Error = [String]

  combineExpectedItems :: Set (Item Error) -> ExpectedItems Error
  combineExpectedItems = disjunct True . toList
  combineMessages :: [Message Error] -> Messages Error
  combineMessages = id

  type UnexpectedLine Error = Maybe String
  type ExpectedLine Error = Maybe String
  type Message Error = String
  type LineInfo Error = Word

  unexpected :: Maybe (Item Error) -> UnexpectedLine Error
  unexpected x = (\(StringBuilder y) -> y "") <$> unexpectedDefault x
  expected :: ExpectedItems Error -> ExpectedLine Error
  expected x = (\(StringBuilder y) -> y "") <$> expectedDefault x
  reason :: String -> Message Error
  message :: String -> Message Error
  reason = id
  lineInfo :: String -> [String] -> [String] -> Word -> Word -> LineInfo Error
  lineInfo _ _ _ _ w = w
  message = id

  numLinesBefore :: Int
  numLinesBefore = 1
  numLinesAfter :: Int
  numLinesAfter = 1

  type Item Error = String

  raw :: String -> Item Error
  raw = rawDefault
  named :: String -> Item Error
  named = namedDefault
  endOfInput :: Item Error
  endOfInput = endOfInputDefault

  unexpectedToken :: NonEmpty Char -> Word -> Bool -> Token
  unexpectedToken = lexToken ps singleChar
    where
      ps =
        map (\x -> T.sym nonlexeme x $> ("keyword " ++ x)) keywords
          ++ map (\x -> T.sym nonlexeme x $> ("operator " ++ x)) operators
          ++ [ ("integer " ++) . show <$> T.decimal (T.integer nonlexeme)
             , ("character " ++) . show <$> T.ascii (T.charLiteral nonlexeme)
             , ("string " ++) . show <$> T.ascii (T.stringLiteral nonlexeme)
             , showWhitespace <$> whitespace
             ]

showWhitespace :: Char -> String
showWhitespace ' ' = "space"
showWhitespace '\t' = "tab"
showWhitespace '\n' = "newline"
showWhitespace '\r' = "carriage return"
showWhitespace '\f' = "form feed"
showWhitespace '\v' = "vertical tab"
showWhitespace x = "whitespace " ++ show x
