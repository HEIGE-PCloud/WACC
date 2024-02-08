{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WACC.Error where

import Data.List.Extra ((!?))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Set (Set, toList)
import Data.String (IsString (fromString))
import Language.WACC.Parser.Token (keywords, nonlexeme, operators)
import Text.Gigaparsec (Parsec, Result, parse, parseFromFile, ($>))
import Text.Gigaparsec.Char (whitespace)
import Text.Gigaparsec.Errors.DefaultErrorBuilder
  ( StringBuilder (..)
  , combineMessagesDefault
  , disjunct
  , endOfInputDefault
  , expectedDefault
  , formatDefault
  , formatPosDefault
  , intercalate
  , lineInfoDefault
  , namedDefault
  , rawDefault
  , specialisedErrorDefault
  , unexpectedDefault
  , vanillaErrorDefault
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
printError filePath sourceCodeLines (Error errorMessage position@(row, col) width) = printHeader filePath position ++ "\n" ++ errorMessage ++ "\n" ++ lineStr
  where
    prevLine = printLine (row - 2, col) sourceCodeLines
    currline = printLine (row - 1, col) sourceCodeLines
    caretLine = replicate (fromIntegral col - 1) ' ' ++ replicate (fromIntegral width) '^'
    nextLine = printLine (row, col) sourceCodeLines
    lines = map ("> " ++) $ catMaybes [prevLine, currline, Just caretLine, nextLine]
    lineStr = unlines lines

printPos :: Pos -> String
printPos (x, y) = "(line " ++ show x ++ ", column " ++ show y ++ ")"

printLine :: Pos -> [String] -> Maybe String
printLine (row, _) lines = lines !? fromIntegral row

printHeader :: FilePath -> Pos -> String
printHeader filePath pos = "In " ++ filePath ++ " " ++ printPos pos ++ ":"

instance ErrorBuilder Error where
  format :: Position Error -> Source Error -> ErrorInfoLines Error -> Error
  format pos src (lines, width') =
    Error
      { errorMessage = intercalate "\n" lines
      , position = pos
      , width = width'
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
  vanillaError unexpectedLine expectedLine msgs width = (catMaybes [unexpectedLine, expectedLine] ++ msgs, width)

  specialisedError :: Messages Error -> LineInfo Error -> ErrorInfoLines Error
  specialisedError msgs width = (msgs, width)

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
  lineInfo _ _ _ _ width = width
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
