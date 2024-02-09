{- |
Defines the token types and the lexer for the WACC language.
-}
module Language.WACC.Parser.Token where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Gigaparsec (Parsec, atomic, notFollowedBy, void)
import Text.Gigaparsec.Char (char, digit)
import qualified Text.Gigaparsec.Errors.Combinator as E (label)
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
      ( labelCharAsciiEnd
      , labelEscapeEnd
      , labelSymbol
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

{- |
Defines the lexer for the WACC language.
-}
waccNameDesc :: NameDesc
waccNameDesc =
  NameDesc
    { identifierStart = Just (\x -> isAlpha x || x == '_')
    , identifierLetter = Just (\x -> isAlphaNum x || x == '_')
    , operatorStart = Nothing
    , operatorLetter = Nothing
    }

{- |
Defines the keywords and operators for the WACC language.
-}
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

{- |
Defines the operators for the WACC language.
-}
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

{- |
Defines the escape characters for the WACC language.
-}
escapeChars :: [Char]
escapeChars = ['0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\']

{- |
Defines the symbol description for the WACC language lexer.
-}
waccSymbolDesc :: SymbolDesc
waccSymbolDesc =
  SymbolDesc
    { hardKeywords =
        S.fromList keywords
    , hardOperators =
        S.fromList operators
    , caseSensitive = True
    }

{- |
Defines the numeric description for the WACC language lexer.
-}
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

{- |
Defines the text description for the WACC language lexer.
-}
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

{- |
Defines the space description for the WACC language lexer.
-}
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

{- |
Defines the error configuration for the WACC language lexer.
-}
errorConfig :: ErrorConfig
errorConfig =
  defaultErrorConfig
    { labelSymbol =
        M.fromList
          [
            ( "}"
            , labelAndReason
                (S.singleton "closing brace")
                "unclosed brace"
            )
          , ("(", label (S.singleton "opening parenthesis"))
          ,
            ( ")"
            , label
                (S.singleton "closing bracket")
            )
          , ("[", label (S.singleton "array index"))
          , ("!", label (S.singleton "not"))
          , ("=", label (S.singleton "assignment"))
          , ("+", label (S.singleton "binary operator"))
          , ("-", label (S.singleton "binary operator"))
          , ("*", label (S.singleton "binary operator"))
          , ("/", label (S.singleton "binary operator"))
          , ("%", label (S.singleton "binary operator"))
          , (">", label (S.singleton "binary operator"))
          , ("<", label (S.singleton "binary operator"))
          , (">=", label (S.singleton "binary operator"))
          , ("<=", label (S.singleton "binary operator"))
          , ("==", label (S.singleton "binary operator"))
          , ("!=", label (S.singleton "binary operator"))
          , ("&&", label (S.singleton "binary operator"))
          , ("||", label (S.singleton "binary operator"))
          , ("bool", label (S.singleton "type"))
          , ("char", label (S.singleton "type"))
          , ("int", label (S.singleton "type"))
          , ("pair", label (S.singleton "type"))
          , ("string", label (S.singleton "type"))
          ]
    , labelEscapeEnd =
        labelAndReason
          (S.singleton "escape sequence")
          "valid escape sequences are \\0, \\n, \\t, \\b, \\f, \\r, \\\", \\\' or \\\\"
    , labelCharAsciiEnd = label (S.singleton "end of character literal")
    }

{- |
Defines the lexer for the WACC language.
-}
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

{- |
Extracts the lexeme from the lexer. This is used to extract the token with whitespace consumed.
-}
lexeme :: Lexeme
lexeme = T.lexeme lexer

{- |
Extracts the non-lexeme from the lexer. This is used to extract the token without whitespace consumed.
-}
nonlexeme :: Lexeme
nonlexeme = T.nonlexeme lexer

{- |
Gets name Parsers from the lexer.
-}
names :: Names
names = T.names lexeme

{- |
Parser for an identifier.
-}
identifier :: Parsec String
identifier = T.identifier names

{- |
Parser for an integer.
-}
integer :: IntegerParsers CanHoldSigned
integer = T.integer lexeme

{- |
Parser for a 32-bit decimal.
-}
decimal :: Parsec Integer
decimal = T.decimal32 integer

{- |
Parser for a string literal.
-}
stringLiteral :: Parsec String
stringLiteral = ascii $ T.stringLiteral lexeme

{- |
Parser for a character literal.
-}
charLiteral :: Parsec Char
charLiteral = ascii $ T.charLiteral lexeme

{- |
Parser that consumes leading whitespace expects end of file at the end.
-}
fully :: Parsec a -> Parsec a
fully = T.fully lexer

{- |
Parser for a symbol in programs.
-}
sym :: String -> Parsec ()
sym = T.sym lexeme

{- |
Parser for disambiguating between a negative integer literal and a negation operator.
-}
negateOp :: Parsec ()
negateOp =
  E.label
    (S.singleton "negation")
    (T.apply lexeme (atomic (void (char '-' <* notFollowedBy digit))))

{- |
Parser for a graphic character.
-}
graphicChars :: [Char]
graphicChars = ['\32' ..] \\ ['\\', '\"', '\'']
