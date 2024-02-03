module Language.WACC.Parser.Token where

import Data.Char (isAlpha, isAlphaNum, isAscii, isPrint, isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Gigaparsec (Parsec, ($>))
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
import Text.Gigaparsec.Token.Lexer
  ( CanHoldSigned
  , IntegerParsers
  , Lexeme
  , Lexer
  , Names
  , TextParsers (ascii)
  , mkLexer
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
  ]

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
        Set.fromList keywords
    , hardOperators =
        Set.fromList operators
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
    , hexadecimalLeads = Set.empty
    , octalLeads = Set.empty
    , binaryLeads = Set.empty
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
          , literals = Set.fromList escapeChars
          , mapping = Map.empty
          , decimalEscape = NumericIllegal
          , octalEscape = NumericIllegal
          , hexadecimalEscape = NumericIllegal
          , binaryEscape = NumericIllegal
          , emptyEscape = Nothing
          , gapsSupported = False
          }
    , characterLiteralEnd = '\''
    , stringEnds = Set.fromList [("\"", "\"")]
    , multiStringEnds = Set.empty
    , graphicCharacter = Just (\x -> isAscii x && isPrint x)
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

lexer :: Lexer
lexer =
  mkLexer $
    LexicalDesc
      { nameDesc = waccNameDesc
      , symbolDesc = waccSymbolDesc
      , numericDesc = waccNumericDesc
      , textDesc = waccTextDesc
      , spaceDesc = waccSpaceDesc
      }

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

sym :: String -> Parsec String
sym s = T.sym lexeme s $> s
