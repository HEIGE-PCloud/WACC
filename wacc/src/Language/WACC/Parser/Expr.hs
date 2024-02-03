{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Expr where

import Language.WACC.AST.Expr (ArrayIndex (ArrayIndex), Expr (..), WAtom (..))
import Language.WACC.Parser.Token
  ( decimal
  , identifier
  , lexer
  , negateOp
  , validChars
  )
import Text.Gigaparsec (Parsec, atomic, many, ($>), (<|>))
import Text.Gigaparsec.Char (char, string)
import Text.Gigaparsec.Combinator (choice)
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
    ['IntLit, 'BoolLit, 'CharLit, 'StringLit, 'Null]
 )

$( deriveDeferredConstructors
    "mkD"
    ['WAtom]
 )

$( deriveDeferredConstructors
    "mk"
    ['Ident, 'ArrayElem]
 )

$( deriveDeferredConstructors
    "mk"
    ['Not, 'Negate, 'Len, 'Ord, 'Chr]
 )

$( deriveDeferredConstructors
    "mk"
    ['Mul, 'Div, 'Mod]
 )

$( deriveDeferredConstructors
    "mk"
    ['Add, 'Sub]
 )

$( deriveDeferredConstructors
    "mk"
    ['GT, 'GTE, 'LT, 'LTE]
 )

$( deriveDeferredConstructors
    "mk"
    ['Eq, 'Ineq]
 )

$( deriveDeferredConstructors
    "mk"
    ['And]
 )

$( deriveDeferredConstructors
    "mk"
    ['Or]
 )

$( deriveLiftedConstructors
    "mk"
    ['WAtom]
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

arrOrIdent :: Parsec (WAtom String)
arrOrIdent = do
  s <- identifier
  exprs <- many ("[" *> expr <* "]")
  f <- mkIdent
  g <- mkArrayElem
  case exprs of
    [] -> pure (f s)
    _ -> pure (g (ArrayIndex s exprs))

atom :: Parsec (Expr String)
atom =
  choice
    [ WAtom <$> arrOrIdent
    , WAtom <$> intLiter
    , WAtom <$> pairLiter
    , WAtom <$> boolLiter
    , WAtom <$> charLiter
    , WAtom <$> stringLiter
    , WAtom <$> pairLiter
    , "(" *> expr <* ")"
    ]

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
