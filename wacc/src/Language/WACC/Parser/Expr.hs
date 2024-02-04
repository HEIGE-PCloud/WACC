{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Expr
  ( intLiter
  , boolLiter
  , charLiter
  , stringLiter
  , pairLiter
  , ident
  , arrayElem
  , atom
  , expr
  )
where

import Data.Set (fromList)
import Language.WACC.AST.Expr (ArrayIndex (..), Expr (..), WAtom (..))
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Token
  ( decimal
  , escapedChars
  , identifier
  , negateOp
  , normalChars
  )
import Text.Gigaparsec (Parsec, many, ($>), (<|>))
import Text.Gigaparsec.Char (char)
import Text.Gigaparsec.Combinator (choice, option)
import Text.Gigaparsec.Errors.Combinator (explain, label)
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

-- | > <int-liter> ::= <int-sign>? <digit>+
intLiter :: Parsec (WAtom i)
intLiter = mkIntLit' decimal

-- | > <bool-liter> ::= "true" | "false"
boolLiter :: Parsec (WAtom i)
boolLiter = mkBoolLit' (("true" $> True) <|> ("false" $> False))

{- | > <character> ::= any-graphic-ASCII-character-except-'\'-'''-'"'
 > (graphic 𝑔 ≥' ')
 >                   | '\' ⟨escaped-char⟩
-}
character :: Parsec Char
character = mkNormalChars' normalChars <|> mkEscapedChars' (char '\\' *> escapedChars)

-- | > <char-liter> ::= ''' <character> '''
charLiter :: Parsec (WAtom i)
charLiter = char '\'' *> mkCharLit' character <* "'"

-- | > <string-liter> ::= '"' <character>* '"'
stringLiter :: Parsec (WAtom i)
stringLiter = mkStringLit' (char '\"' *> many character <* "\"")

-- | > <null-liter> ::= "null"
pairLiter :: Parsec (WAtom i)
pairLiter = "null" >> mkNull'

-- | > <ident> ::= ('_'|'a'-'z'|'A'-'Z')('_'|'a'-'z'|'A'-'Z'|'0'-'9')*
ident :: Parsec (WAtom String)
ident = mkIdent' identifier

-- | > <array-elem> ::= <ident> | <ident> ('['⟨expr⟩']')+
arrayElem :: Parsec (WAtom String)
arrayElem = mkArrayElem (mkArrayIndex identifier (many ("[" *> expr <* "]")))

{- | > <atom> ::= <int-liter> | <bool-liter> | <char-liter> | <string-liter>
 >              | <pair-liter> | <ident> | <array-elem> | '(' <expr> ')'
-}
atom :: Parsec (Expr String)
atom =
  choice
    [ mkWAtom intLiter
    , mkWAtom boolLiter
    , mkWAtom charLiter
    , mkWAtom stringLiter
    , mkWAtom pairLiter
    , mkWAtom (mkIdentOrArrayElem identifier (option (many ("[" *> expr <* "]"))))
    , "(" *> expr <* ")"
    ]

mkIdentOrArrayElem
  :: Parsec String -> Parsec (Maybe [Expr String]) -> Parsec (WAtom String)
mkIdentOrArrayElem = liftA2 mkIdentOrArrayElem'
  where
    mkIdentOrArrayElem' :: String -> Maybe [Expr String] -> WAtom String
    mkIdentOrArrayElem' str (Just e) = ArrayElem (ArrayIndex str e)
    mkIdentOrArrayElem' str Nothing = Ident str

{- | > <expr> ::= <unary-oper> <expr>
 >              | <expr> <binary-oper> <expr>
 >              | <atom>
-}
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
        >+ ops InfixR [mkAnd <* "&&"]
        >+ ops InfixR [mkOr <* "||"]
    )

mkIntLit' :: Parsec Integer -> Parsec (WAtom ident)
mkIntLit' = label (fromList ["integer"]) . mkIntLit

mkBoolLit' :: Parsec Bool -> Parsec (WAtom ident)
mkBoolLit' = label (fromList ["boolean"]) . mkBoolLit

mkCharLit' :: Parsec Char -> Parsec (WAtom ident)
mkCharLit' = label (fromList ["character"]) . mkCharLit

mkStringLit' :: Parsec String -> Parsec (WAtom ident)
mkStringLit' = label (fromList ["strings"]) . mkStringLit

mkNull' :: Parsec (WAtom ident)
mkNull' = label (fromList ["null"]) mkNull

mkIdent' :: Parsec String -> Parsec (WAtom String)
mkIdent' = label (fromList ["identifier"]) . mkIdent

mkNormalChars' :: Parsec Char -> Parsec Char
mkNormalChars' = label (fromList ["character"])

mkEscapedChars' :: Parsec Char -> Parsec Char
mkEscapedChars' =
  explain "This is a special character, it needs to be escaped by '\\'."
    . label (fromList ["escaped character"])
