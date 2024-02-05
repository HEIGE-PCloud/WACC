module Language.WACC.Parser.Token where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Gigaparsec (Parsec, atomic, notFollowedBy, void)
import Text.Gigaparsec.Char (char, digit)
import Text.Gigaparsec.Token.Descriptions
  ( BreakCharDesc (NoBreakChar)
  , EscapeDesc
    ( EscapeDesc
    , binaryEscape
    , decimalEscape
    , emptyEscape
    , escBegin
    , gapsSupported
    , hexadecimalEscape
    , literals
    , mapping
    , octalEscape
    )
  , ExponentDesc (NoExponents)
  , LexicalDesc
    ( LexicalDesc
    , nameDesc
    , numericDesc
    , spaceDesc
    , symbolDesc
    , textDesc
    )
  , NameDesc
    ( NameDesc
    , identifierLetter
    , identifierStart
    , operatorLetter
    , operatorStart
    )
  , NumericDesc
    ( NumericDesc
    , binaryExponentDesc
    , binaryLeads
    , decimalExponentDesc
    , hexadecimalExponentDesc
    , hexadecimalLeads
    , integerNumbersCanBeBinary
    , integerNumbersCanBeHexadecimal
    , integerNumbersCanBeOctal
    , leadingDotAllowed
    , leadingZerosAllowed
    , literalBreakChar
    , octalExponentDesc
    , octalLeads
    , positiveSign
    , realNumbersCanBeBinary
    , realNumbersCanBeHexadecimal
    , realNumbersCanBeOctal
    , trailingDotAllowed
    )
  , NumericEscape (NumericIllegal)
  , PlusSignPresence (PlusOptional)
  , SpaceDesc
    ( SpaceDesc
    , lineCommentAllowsEOF
    , lineCommentStart
    , multiLineCommentEnd
    , multiLineCommentStart
    , multiLineNestedComments
    , space
    , whitespaceIsContextDependent
    )
  , SymbolDesc (SymbolDesc, caseSensitive, hardKeywords, hardOperators)
  , TextDesc
    ( TextDesc
    , characterLiteralEnd
    , escapeSequences
    , graphicCharacter
    , multiStringEnds
    , stringEnds
    )
  )
import Text.Gigaparsec.Token.Errors
  ( ErrorConfig
      ( labelSymbol, labelEscapeEnd
      )
  , LabelConfigurable (label)
  , LabelWithExplainConfigurable (labelAndReason)
  , defaultErrorConfig
  )
import Text.Gigaparsec.Token.Lexer
  ( CanHoldSigned
  , IntegerParsers
  , Lexeme
  , Lexer
  , Names
  , TextParsers (ascii)
  , mkLexerWithErrorConfig
  )
import qualified Text.Gigaparsec.Token.Lexer as T

waccNameDesc :: NameDesc
waccNameDesc =
  NameDesc
    { identifierStart = Just (\x -> isAlpha x || x == '_')
    , identifierLetter = Just (\x -> isAlphaNum x || x == '_')
    , operatorStart = Nothing
    , operatorLetter = Nothing
    }

keywords :: [String]
keywords =
  [ "begin"
  , "end"
  , "is"
  , "skip"
  , "read"
  , "free"
  , "return"
  , "exit"
  , "println"
  , "print"
  , "if"
  , "else"
  , "fi"
  , "while"
  , "done"
  , "do"
  , "newpair"
  , "call"
  , "fst"
  , "snd"
  , "len"
  , "ord"
  , "chr"
  , "int"
  , "bool"
  , "char"
  , "string"
  , "pair"
  , "null"
  , "true"
  , "false"
  ]

operators :: [String]
operators =
  [ "!"
  , "-"
  , "*"
  , "/"
  , "%"
  , "+"
  , ">"
  , ">="
  , "<"
  , "<="
  , "=="
  , "!="
  , "&&"
  , "||"
  ]

escapeChars :: [Char]
escapeChars = ['0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\']

waccSymbolDesc :: SymbolDesc
waccSymbolDesc =
  SymbolDesc
    { hardKeywords =
        S.fromList keywords
    , hardOperators =
        S.fromList operators
    , caseSensitive = True
    }

waccNumericDesc :: NumericDesc
waccNumericDesc =
  NumericDesc
    { literalBreakChar = NoBreakChar
    , leadingDotAllowed = False
    , trailingDotAllowed = False
    , leadingZerosAllowed = True
    , positiveSign = PlusOptional
    , integerNumbersCanBeHexadecimal = False
    , integerNumbersCanBeBinary = False
    , integerNumbersCanBeOctal = False
    , realNumbersCanBeBinary = False
    , realNumbersCanBeHexadecimal = False
    , realNumbersCanBeOctal = False
    , hexadecimalLeads = S.empty
    , octalLeads = S.empty
    , binaryLeads = S.empty
    , decimalExponentDesc = NoExponents
    , hexadecimalExponentDesc = NoExponents
    , octalExponentDesc = NoExponents
    , binaryExponentDesc = NoExponents
    }

waccTextDesc :: TextDesc
waccTextDesc =
  TextDesc
    { escapeSequences =
        EscapeDesc
          { escBegin = '\\'
          , literals = S.fromList escapeChars
          , mapping = M.empty
          , decimalEscape = NumericIllegal
          , octalEscape = NumericIllegal
          , hexadecimalEscape = NumericIllegal
          , binaryEscape = NumericIllegal
          , emptyEscape = Nothing
          , gapsSupported = False
          }
    , characterLiteralEnd = '\''
    , stringEnds = S.fromList [("\"", "\"")]
    , multiStringEnds = S.empty
    , graphicCharacter = Just (`elem` graphicChars)
    }

waccSpaceDesc :: SpaceDesc
waccSpaceDesc =
  SpaceDesc
    { lineCommentStart = "#"
    , lineCommentAllowsEOF = True
    , multiLineCommentStart = ""
    , multiLineCommentEnd = ""
    , multiLineNestedComments = False
    , space = Just isSpace
    , whitespaceIsContextDependent = False
    }

errorConfig :: Text.Gigaparsec.Token.Errors.ErrorConfig
errorConfig =
  Text.Gigaparsec.Token.Errors.defaultErrorConfig
    { labelSymbol =
        M.fromList
          [
            ( "}"
            , Text.Gigaparsec.Token.Errors.labelAndReason
                (S.fromList ["closing brace"])
                "unclosed brace"
            )
          ,
            ( ")"
            , Text.Gigaparsec.Token.Errors.labelAndReason
                (S.fromList ["closing bracket"])
                "unclosed bracket"
            )
          , ("=", Text.Gigaparsec.Token.Errors.label (S.fromList ["assignment"]))
          , ("+", Text.Gigaparsec.Token.Errors.label (S.fromList ["arithmetic operator"]))
          , ("-", Text.Gigaparsec.Token.Errors.label (S.fromList ["arithmetic operator"]))
          , ("*", Text.Gigaparsec.Token.Errors.label (S.fromList ["arithmetic operator"]))
          , ("/", Text.Gigaparsec.Token.Errors.label (S.fromList ["arithmetic operator"]))
          , ("%", Text.Gigaparsec.Token.Errors.label (S.fromList ["arithmetic operator"]))
          , (">", Text.Gigaparsec.Token.Errors.label (S.fromList ["comparison operator"]))
          , ("<", Text.Gigaparsec.Token.Errors.label (S.fromList ["comparison operator"]))
          , (">=", Text.Gigaparsec.Token.Errors.label (S.fromList ["comparison operator"]))
          , ("<=", Text.Gigaparsec.Token.Errors.label (S.fromList ["comparison operator"]))
          , ("==", Text.Gigaparsec.Token.Errors.label (S.fromList ["comparison operator"]))
          , ("!=", Text.Gigaparsec.Token.Errors.label (S.fromList ["comparison operator"]))
          , ("&&", Text.Gigaparsec.Token.Errors.label (S.fromList ["logical operator"]))
          , ("||", Text.Gigaparsec.Token.Errors.label (S.fromList ["logical operator"]))
          ]
          , labelEscapeEnd =
              Text.Gigaparsec.Token.Errors.labelAndReason
                (S.fromList ["escape sequence"])
                "valid escape sequences are \\0, \\n, \\t, \\b, \\f, \\r, \\\", \\\' or \\\\"
    }

lexer :: Lexer
lexer =
  mkLexerWithErrorConfig
    LexicalDesc
      { nameDesc = waccNameDesc
      , symbolDesc = waccSymbolDesc
      , numericDesc = waccNumericDesc
      , textDesc = waccTextDesc
      , spaceDesc = waccSpaceDesc
      }
    errorConfig

lexeme :: Lexeme
lexeme = T.lexeme lexer

names :: Names
names = T.names lexeme

identifier :: Parsec String
identifier = T.identifier names

integer :: IntegerParsers CanHoldSigned
integer = T.integer lexeme

decimal :: Parsec Integer
decimal = T.decimal32 integer

stringLiteral :: Parsec String
stringLiteral = ascii $ T.stringLiteral lexeme

charLiteral :: Parsec Char
charLiteral = ascii $ T.charLiteral lexeme

fully :: Parsec a -> Parsec a
fully = T.fully lexer

sym :: String -> Parsec ()
sym = T.sym lexeme

negateOp :: Parsec ()
negateOp = T.apply lexeme (atomic (void (char '-' <* notFollowedBy digit)))

graphicChars :: [Char]
graphicChars = ['\32' ..] \\ ['\\', '\"', '\'']
