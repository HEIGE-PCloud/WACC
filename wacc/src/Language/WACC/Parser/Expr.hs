{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Expr where

import qualified Data.Set as Set
import Language.WACC.AST.Expr (ArrayIndex (ArrayIndex), Expr (..), WAtom (..))
import Language.WACC.Parser.Token
import Language.WACC.Parser.Token
  ( decimal
  , escapeChars
  , identifier
  , lexer
  , stringLiteral
  )
import Text.Gigaparsec (Parsec, atomic, many, notFollowedBy, ($>), (<|>))
import Text.Gigaparsec.Char (char, digit, noneOf, oneOf, whitespaces)
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
import Text.Gigaparsec.Token.Lexer (Lexeme (apply))
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

intLiter :: Parsec (WAtom i)
intLiter = mkIntLit decimal

boolLiter :: Parsec (WAtom i)
boolLiter = mkBoolLit (("true" $> True) <|> ("false" $> False))

charLiter :: Parsec (WAtom i)
charLiter =
  char '\''
    *> mkCharLit
      ( noneOf (Set.fromList ['\\', '\'', '"'])
          <|> (char '\\' *> oneOf (Set.fromList escapeChars))
      )
    <* "'"

stringLiter :: Parsec (WAtom i)
stringLiter = mkStringLit stringLiteral

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
