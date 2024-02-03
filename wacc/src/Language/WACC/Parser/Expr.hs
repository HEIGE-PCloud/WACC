{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Expr where

import Language.WACC.AST.Expr (ArrayIndex (..), Expr (..), WAtom (..))
import Language.WACC.Parser.Token
  ( decimal
  , identifier
  , lexer
  , negateOp
  , validChars
  )
import Text.Gigaparsec (Parsec, atomic, many, ($>), (<|>))
import Text.Gigaparsec.Char (char, string)
import Text.Gigaparsec.Combinator (choice, option)
import Text.Gigaparsec.Expr
  ( Fixity (InfixL, InfixN, InfixR, Prefix)
  , Prec (..)
  , ops
  , precedence
  , (>+)
  )
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )
import Text.Gigaparsec.Token.Patterns (overloadedStrings)
import Prelude hiding (GT, LT)

$( deriveLiftedConstructors
    "mk"
    [ 'IntLit
    , 'BoolLit
    , 'CharLit
    , 'StringLit
    , 'Null
    , 'Ident
    , 'ArrayIndex
    , 'ArrayElem
    , 'WAtom
    ]
 )

$( deriveDeferredConstructors
    "mk"
    [ 'Not
    , 'Negate
    , 'Len
    , 'Ord
    , 'Chr
    , 'Mul
    , 'Div
    , 'Mod
    , 'Add
    , 'Sub
    , 'GT
    , 'GTE
    , 'LT
    , 'LTE
    , 'Eq
    , 'Ineq
    , 'And
    , 'Or
    ]
 )

$(overloadedStrings [|lexer|])

-- | > <int-liter> ::= <int-sign>? <digit>+
intLiter :: Parsec (WAtom i)
intLiter = mkIntLit decimal

-- | > <bool-liter> ::= "true" | "false"
boolLiter :: Parsec (WAtom i)
boolLiter = mkBoolLit (("true" $> True) <|> ("false" $> False))

{- | > <character> ::= any-graphic-ASCII-character-except-'\'-'''-'"' (graphic 𝑔 ≥' ')
 >              |  '\' ⟨escaped-char⟩
-}
character :: Parsec Char
character = last <$> choice [atomic (string c) | c <- validChars]

-- | > <char-liter> ::= ''' <character> '''
charLiter :: Parsec (WAtom i)
charLiter = char '\'' *> mkCharLit character <* "'"

stringLiter :: Parsec (WAtom i)
stringLiter = mkStringLit (char '\"' *> many character <* "\"")

pairLiter :: Parsec (WAtom i)
pairLiter = "null" >> mkNull

ident :: Parsec (WAtom String)
ident = mkIdent identifier

arrayElem :: Parsec (WAtom String)
arrayElem = mkArrayElem $ mkArrayIndex identifier (many ("[" *> expr <* "]"))

atom :: Parsec (Expr String)
atom =
  mkWAtom intLiter
    <|> mkWAtom boolLiter
    <|> mkWAtom charLiter
    <|> mkWAtom stringLiter
    <|> mkWAtom pairLiter
    <|> mkWAtom (mkIdentOrArrayElem identifier (option (many ("[" *> expr <* "]"))))
    <|> ("(" *> expr <* ")")

mkIdentOrArrayElem
  :: Parsec String -> Parsec (Maybe [Expr String]) -> Parsec (WAtom String)
mkIdentOrArrayElem = liftA2 mkIdentOrArrayElem'
  where
    mkIdentOrArrayElem' :: String -> Maybe [Expr String] -> WAtom String
    mkIdentOrArrayElem' str (Just e) = ArrayElem $ ArrayIndex str e
    mkIdentOrArrayElem' str Nothing = Ident str

expr :: Parsec (Expr String)
expr =
  precedence
    ( Atom atom
        >+ ops
          Prefix
          [ mkNot <* "!"
          , mkNegate <* negateOp
          , mkLen <* "len"
          , mkOrd <* "ord"
          , mkChr <* "chr"
          ]
        >+ ops InfixL [mkMul <* "*", mkMod <* "%", mkDiv <* "/"]
        >+ ops InfixL [mkAdd <* "+", mkSub <* "-"]
        >+ ops
          InfixN
          [mkGTE <* ">=", mkGT <* ">", mkLTE <* "<=", mkLT <* "<"]
        >+ ops InfixN [mkEq <* "==", mkIneq <* "!="]
        >+ ops InfixR ["&&" *> mkAnd]
        >+ ops InfixR ["||" *> mkOr]
    )
