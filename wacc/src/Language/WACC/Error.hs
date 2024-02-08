{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WACC.Error where

import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set, toList)
import Data.String (IsString (fromString))
import Language.WACC.Parser.Token (keywords, nonlexeme, space)
import Text.Gigaparsec (Parsec, Result, parseFromFile, ($>))
import Text.Gigaparsec.Errors.DefaultErrorBuilder
  ( StringBuilder (..)
  , combineMessagesDefault
  , disjunct
  , endOfInputDefault
  , expectedDefault
  , formatDefault
  , formatPosDefault
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
import qualified Text.Gigaparsec.Token.Lexer as T

parseFromFileWithError :: Parsec a -> FilePath -> IO (Result Error a)
parseFromFileWithError = parseFromFile

newtype Error = Error String

instance Show Error where
  show (Error x) = x

instance ErrorBuilder Error where
  format :: Position Error -> Source Error -> ErrorInfoLines Error -> Error
  format x y z = Error (formatDefault x y z)

  type Position Error = StringBuilder
  type Source Error = Maybe StringBuilder

  pos :: Word -> Word -> Position Error
  pos = formatPosDefault
  source :: Maybe FilePath -> Source Error
  source = fmap fromString

  type ErrorInfoLines Error = [StringBuilder]
  vanillaError
    :: UnexpectedLine Error
    -> ExpectedLine Error
    -> Messages Error
    -> LineInfo Error
    -> ErrorInfoLines Error
  vanillaError = vanillaErrorDefault
  specialisedError :: Messages Error -> LineInfo Error -> ErrorInfoLines Error
  specialisedError = specialisedErrorDefault

  type ExpectedItems Error = Maybe StringBuilder
  type Messages Error = [StringBuilder]

  combineExpectedItems :: Set (Item Error) -> ExpectedItems Error
  combineExpectedItems = disjunct True . toList
  combineMessages :: [Message Error] -> Messages Error
  combineMessages = combineMessagesDefault

  type UnexpectedLine Error = Maybe StringBuilder
  type ExpectedLine Error = Maybe StringBuilder
  type Message Error = String
  type LineInfo Error = [StringBuilder]

  unexpected :: Maybe (Item Error) -> UnexpectedLine Error
  unexpected = unexpectedDefault
  expected :: ExpectedItems Error -> ExpectedLine Error
  expected = expectedDefault
  reason :: String -> Message Error
  message :: String -> Message Error
  reason = id
  lineInfo :: String -> [String] -> [String] -> Word -> Word -> LineInfo Error
  message = id

  lineInfo = lineInfoDefault

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
  unexpectedToken = lexToken xs (tillNextWhitespace True isSpace)
    where
      xs = map (\x -> T.sym nonlexeme x $> ("keyword " ++ x)) keywords
        ++ 
        [ ("integer " ++) . show <$> T.decimal (T.integer nonlexeme)
        -- , ("identifier " ++) <$> T.identifier (T.names nonlexeme)
        , ("character " ++) . show <$> T.ascii (T.charLiteral nonlexeme)
        , ("string " ++) . show <$> T.ascii (T.stringLiteral nonlexeme)
        , T.whiteSpace space $> "whitespace"
        ]
          