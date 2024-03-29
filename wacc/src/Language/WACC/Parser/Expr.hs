{-# LANGUAGE OverloadedStrings #-}

{- |
Defines the parser for WACC expressions.
-}
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

import Control.Applicative (liftA3)
import Data.Set (singleton)
import Language.WACC.AST.Expr (ArrayIndex (..), Expr (..), WAtom (..))
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Constructors.Expr
import Language.WACC.Parser.Token
  ( charLiteral
  , decimal
  , identifier
  , negateOp
  , stringLiteral
  )
import Text.Gigaparsec (Parsec, some, ($>), (<|>))
import Text.Gigaparsec.Combinator (choice, option, sepBy)
import Text.Gigaparsec.Errors.Combinator (explain, label)
import Text.Gigaparsec.Errors.Patterns (preventativeExplain)
import Text.Gigaparsec.Expr
  ( Fixity (InfixL, InfixN, InfixR, Prefix)
  , Prec (..)
  , ops
  , precedence
  , (>+)
  )
import Text.Gigaparsec.Position (Pos, pos)
import Prelude hiding (GT, LT)

-- | > <int-liter> ::= <int-sign>? <digit>+
intLiter :: Parsec (WAtom i Pos)
intLiter = mkIntLit' decimal

-- | > <bool-liter> ::= "true" | "false"
boolLiter :: Parsec (WAtom i Pos)
boolLiter = mkBoolLit' (("true" $> True) <|> ("false" $> False))

-- | > <char-liter> ::= ''' <character> '''
charLiter :: Parsec (WAtom i Pos)
charLiter = mkCharLit' charLiteral

-- | > <string-liter> ::= '"' <character>* '"'
stringLiter :: Parsec (WAtom i Pos)
stringLiter = mkStringLit' stringLiteral

-- | > <null-liter> ::= "null"
pairLiter :: Parsec (WAtom i Pos)
pairLiter = "null" >> mkNull'

-- | > <ident> ::= ('_'|'a'-'z'|'A'-'Z')('_'|'a'-'z'|'A'-'Z'|'0'-'9')*
ident :: Parsec (WAtom String Pos)
ident = mkIdent' identifier

-- | > <array-elem> ::= <ident> | <ident> ('['⟨expr⟩']')+
arrayElem :: Parsec (WAtom String Pos)
arrayElem = mkArrayElem' (mkArrayIndex identifier (some ("[" *> expr <* "]")))

{- | > <atom> ::= <int-liter> | <bool-liter> | <char-liter> | <string-liter>
 >              | <pair-liter> | <ident> | <array-elem> | '(' <expr> ')'
-}
atom :: Parsec (Expr String Pos)
atom =
  choice
    [ mkWAtom intLiter
    , mkWAtom boolLiter
    , mkWAtom charLiter
    , mkWAtom stringLiter
    , mkWAtom pairLiter
    , mkWAtom
        (mkIdentOrArrayElem pos identifier (option (some ("[" *> expr <* "]"))))
    , "(" *> expr <* ")"
    ]

{- |
Left-factoring the identifier and array element parsers.
-}
mkIdentOrArrayElem
  :: Parsec Pos
  -> Parsec String
  -> Parsec (Maybe [Expr String Pos])
  -> Parsec (WAtom String Pos)
mkIdentOrArrayElem = liftA3 mkIdentOrArrayElem'
  where
    mkIdentOrArrayElem'
      :: Pos -> String -> Maybe [Expr String Pos] -> WAtom String Pos
    mkIdentOrArrayElem' p str (Just e) = ArrayElem (ArrayIndex str e p) p
    mkIdentOrArrayElem' p str Nothing = Ident str p

{- | > <expr> ::= <unary-oper> <expr>
 >              | <expr> <binary-oper> <expr>
 >              | <atom>
-}
expr :: Parsec (Expr String Pos)
expr =
  mkExpr'
    ( precedence
        ( Atom (_funcExpr *> atom)
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
    )

{- |
Lifted Constructor for the 'WAtom' 'int' literal.
-}
mkIntLit' :: Parsec Integer -> Parsec (WAtom ident Pos)
mkIntLit' = label (singleton "integer") . mkIntLit

{- |
Lifted Constructor for the 'WAtom' 'bool' literal.
-}
mkBoolLit' :: Parsec Bool -> Parsec (WAtom ident Pos)
mkBoolLit' = label (singleton "boolean") . mkBoolLit

{- |
Lifted constructor for the 'WAtom' 'char' literal.
-}
mkCharLit' :: Parsec Char -> Parsec (WAtom ident Pos)
mkCharLit' = label (singleton "character literal") . mkCharLit

{- |
Lifted constructor for the 'WAtom' 'string' literal.
-}
mkStringLit' :: Parsec String -> Parsec (WAtom ident Pos)
mkStringLit' = label (singleton "strings") . mkStringLit

{- |
Lifted constructor for the 'WAtom' 'null' literal.
-}
mkNull' :: Parsec (WAtom ident Pos)
mkNull' = label (singleton "null") mkNull

{- |
Lifted constructor for the 'WAtom' identifier literal.
-}
mkIdent' :: Parsec String -> Parsec (WAtom String Pos)
mkIdent' = label (singleton "identifier") . mkIdent

{- |
Constructs a parser for the 'Expr' with error messages for expected values.
-}
mkExpr' :: Parsec a -> Parsec a
mkExpr' =
  explain
    "expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses"
    . label (singleton "expression")

{- |
Lifted constructor for the 'WAtom' 'array' element.
-}
mkArrayElem' :: Parsec (ArrayIndex String Pos) -> Parsec (WAtom String Pos)
mkArrayElem' = label (singleton "array element") . mkArrayElem

{- |
Unit Parser to annnotate error messages for function calls in expressions.
-}
_funcExpr :: Parsec ()
_funcExpr =
  preventativeExplain
    (const "function calls may not appear in expressions and must use `call`")
    (identifier <* "(" *> (identifier `sepBy` ",") <* ")")
