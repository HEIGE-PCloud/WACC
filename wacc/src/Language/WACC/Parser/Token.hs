{-# LANGUAGE ApplicativeDo #-}

module Language.WACC.Parser.Token
  ( lexTok
  )
where

import Data.Char (isAlpha, isAlphaNum, isAscii, isPrint, isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import Text.Gigaparsec.Token.Lexer (Lexer, mkLexer)

-- * Lexical Tokens of WACC not directly in structure

lexTok :: Lexer
lexTok =
  mkLexer $
    LexicalDesc
      { nameDesc =
          NameDesc
            { identifierStart = Just (\x -> isAlpha x || x == '_')
            , identifierLetter = Just (\x -> isAlphaNum x || x == '_')
            , operatorStart = Nothing
            , operatorLetter = Nothing
            }
      , symbolDesc =
          SymbolDesc
            { hardKeywords =
                Set.fromList
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
                  ]
            , hardOperators =
                Set.fromList
                  [ "!"
                  , "-"
                  , "len"
                  , "ord"
                  , "chr"
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
            , caseSensitive = True
            }
      , numericDesc =
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
      , textDesc =
          TextDesc
            { escapeSequences =
                EscapeDesc
                  { escBegin = '\\'
                  , literals = Set.fromList ['0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\']
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
      , spaceDesc =
          SpaceDesc
            { lineCommentStart = "#"
            , lineCommentAllowsEOF = True
            , multiLineCommentStart = ""
            , multiLineCommentEnd = ""
            , multiLineNestedComments = False
            , Text.Gigaparsec.Token.Descriptions.space = Just isSpace
            , whitespaceIsContextDependent = False
            }
      }
