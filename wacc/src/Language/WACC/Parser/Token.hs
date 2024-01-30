module Language.WACC.Parser.Token
  ( identifier,
    decimal,
    stringLiteral,
    charLiteral,
    fully,
    sym,
  )
where

import Data.Char (isAlpha, isAlphaNum, isAscii, isPrint, isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Gigaparsec (Parsec, ($>))
import Text.Gigaparsec.Token.Descriptions
  ( BreakCharDesc (NoBreakChar),
    EscapeDesc
      ( EscapeDesc,
        binaryEscape,
        decimalEscape,
        emptyEscape,
        escBegin,
        gapsSupported,
        hexadecimalEscape,
        literals,
        mapping,
        octalEscape
      ),
    ExponentDesc (NoExponents),
    LexicalDesc
      ( LexicalDesc,
        nameDesc,
        numericDesc,
        spaceDesc,
        symbolDesc,
        textDesc
      ),
    NameDesc
      ( NameDesc,
        identifierLetter,
        identifierStart,
        operatorLetter,
        operatorStart
      ),
    NumericDesc
      ( NumericDesc,
        binaryExponentDesc,
        binaryLeads,
        decimalExponentDesc,
        hexadecimalExponentDesc,
        hexadecimalLeads,
        integerNumbersCanBeBinary,
        integerNumbersCanBeHexadecimal,
        integerNumbersCanBeOctal,
        leadingDotAllowed,
        leadingZerosAllowed,
        literalBreakChar,
        octalExponentDesc,
        octalLeads,
        positiveSign,
        realNumbersCanBeBinary,
        realNumbersCanBeHexadecimal,
        realNumbersCanBeOctal,
        trailingDotAllowed
      ),
    NumericEscape (NumericIllegal),
    PlusSignPresence (PlusOptional),
    SpaceDesc
      ( SpaceDesc,
        lineCommentAllowsEOF,
        lineCommentStart,
        multiLineCommentEnd,
        multiLineCommentStart,
        multiLineNestedComments,
        space,
        whitespaceIsContextDependent
      ),
    SymbolDesc (SymbolDesc, caseSensitive, hardKeywords, hardOperators),
    TextDesc
      ( TextDesc,
        characterLiteralEnd,
        escapeSequences,
        graphicCharacter,
        multiStringEnds,
        stringEnds
      ),
  )
import Text.Gigaparsec.Token.Lexer (CanHoldSigned, IntegerParsers, Lexeme, Lexer, Names, TextParsers (ascii), mkLexer)
import qualified Text.Gigaparsec.Token.Lexer (IntegerParsers (decimal), Lexeme (charLiteral, integer, names, stringLiteral, sym), Lexer (fully, lexeme), Names (identifier))

waccNameDesc :: NameDesc
waccNameDesc =
  NameDesc
    { identifierStart = Just (\x -> isAlpha x || x == '_'),
      identifierLetter = Just (\x -> isAlphaNum x || x == '_'),
      operatorStart = Nothing,
      operatorLetter = Nothing
    }

waccSymbolDesc :: SymbolDesc
waccSymbolDesc =
  SymbolDesc
    { hardKeywords =
        Set.fromList
          [ "begin",
            "end",
            "is",
            "skip",
            "read",
            "free",
            "return",
            "exit",
            "println",
            "print",
            "if",
            "else",
            "fi",
            "while",
            "done",
            "do",
            "newpair",
            "call",
            "fst",
            "snd"
          ],
      hardOperators =
        Set.fromList
          [ "!",
            "-",
            "len",
            "ord",
            "chr",
            "*",
            "/",
            "%",
            "+",
            ">",
            ">=",
            "<",
            "<=",
            "==",
            "!=",
            "&&",
            "||"
          ],
      caseSensitive = True
    }

waccNumericDesc :: NumericDesc
waccNumericDesc =
  NumericDesc
    { literalBreakChar = NoBreakChar,
      leadingDotAllowed = False,
      trailingDotAllowed = False,
      leadingZerosAllowed = True,
      positiveSign = PlusOptional,
      integerNumbersCanBeHexadecimal = False,
      integerNumbersCanBeBinary = False,
      integerNumbersCanBeOctal = False,
      realNumbersCanBeBinary = False,
      realNumbersCanBeHexadecimal = False,
      realNumbersCanBeOctal = False,
      hexadecimalLeads = Set.empty,
      octalLeads = Set.empty,
      binaryLeads = Set.empty,
      decimalExponentDesc = NoExponents,
      hexadecimalExponentDesc = NoExponents,
      octalExponentDesc = NoExponents,
      binaryExponentDesc = NoExponents
    }

waccTextDesc :: TextDesc
waccTextDesc =
  TextDesc
    { escapeSequences =
        EscapeDesc
          { escBegin = '\\',
            literals = Set.fromList ['0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\'],
            mapping = Map.empty,
            decimalEscape = NumericIllegal,
            octalEscape = NumericIllegal,
            hexadecimalEscape = NumericIllegal,
            binaryEscape = NumericIllegal,
            emptyEscape = Nothing,
            gapsSupported = False
          },
      characterLiteralEnd = '\'',
      stringEnds = Set.fromList [("\"", "\"")],
      multiStringEnds = Set.empty,
      graphicCharacter = Just (\x -> isAscii x && isPrint x)
    }

waccSpaceDesc :: SpaceDesc
waccSpaceDesc =
  SpaceDesc
    { lineCommentStart = "#",
      lineCommentAllowsEOF = True,
      multiLineCommentStart = "",
      multiLineCommentEnd = "",
      multiLineNestedComments = False,
      space = Just isSpace,
      whitespaceIsContextDependent = False
    }

lexer :: Lexer
lexer =
  mkLexer $
    LexicalDesc
      { nameDesc = waccNameDesc,
        symbolDesc = waccSymbolDesc,
        numericDesc = waccNumericDesc,
        textDesc = waccTextDesc,
        spaceDesc = waccSpaceDesc
      }

lexeme :: Lexeme
lexeme = Text.Gigaparsec.Token.Lexer.lexeme lexer

names :: Names
names = Text.Gigaparsec.Token.Lexer.names lexeme

identifier :: Parsec String
identifier = Text.Gigaparsec.Token.Lexer.identifier names

integer :: IntegerParsers CanHoldSigned
integer = Text.Gigaparsec.Token.Lexer.integer lexeme

decimal :: Parsec Integer
decimal = Text.Gigaparsec.Token.Lexer.decimal integer

stringLiteral :: Parsec String
stringLiteral = ascii $ Text.Gigaparsec.Token.Lexer.stringLiteral lexeme

charLiteral :: Parsec Char
charLiteral = ascii $ Text.Gigaparsec.Token.Lexer.charLiteral lexeme

fully :: Parsec a -> Parsec a
fully = Text.Gigaparsec.Token.Lexer.fully lexer

sym :: String -> Parsec String
sym s = Text.Gigaparsec.Token.Lexer.sym lexeme s $> s 
