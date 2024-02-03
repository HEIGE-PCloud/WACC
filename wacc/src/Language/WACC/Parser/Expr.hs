{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Expr where

import qualified Data.Set as Set
import Language.WACC.AST.Expr (ArrayIndex (ArrayIndex), Expr (..), WAtom (..))
import Language.WACC.Parser.Token
  ( charLiteral
  , decimal
  , escapeChars
  , identifier
  , stringLiteral
  , sym
  )
import Text.Gigaparsec (Parsec, atomic, many, notFollowedBy, (<|>))
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

intLiter :: Parsec (WAtom i)
intLiter = mkIntLit decimal

mkNegLit :: Parsec (Expr String -> Expr String)
mkNegLit = do
  g <- mkNegate
  pure (\x -> case x of WAtom (IntLit i p) -> WAtom (IntLit (-i) p); _ -> g x)

boolLiter :: Parsec (WAtom i)
boolLiter = mkBoolLit $ (== "true") <$> (sym "true" <|> sym "false")

charLiter :: Parsec (WAtom i)
charLiter =
  char '\''
    *> mkCharLit
      ( noneOf (Set.fromList ['\\', '\'', '"'])
          <|> (char '\\' *> oneOf (Set.fromList escapeChars))
      )
    <* sym "'"

stringLiter :: Parsec (WAtom i)
stringLiter = mkStringLit stringLiteral

pairLiter :: Parsec (WAtom i)
pairLiter = do
  sym "null"
  mkNull

arrOrIdent :: Parsec (WAtom String)
arrOrIdent = do
  s <- identifier
  exprs <- many (sym "[" *> expr <* sym "]")
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
    , sym "(" *> expr <* sym ")"
    ]

expr :: Parsec (Expr String)
expr =
  precedence
    ( Atom atom
        >+ ops
          Prefix
          [ mkNot <* sym "!"
          , mkNegate <* atomic (char '-' <* notFollowedBy digit <* whitespaces)
          , mkLen <* sym "len"
          , mkOrd <* sym "ord"
          , mkChr <* sym "chr"
          ]
        >+ ops InfixL [mkMul <* sym "*", mkMod <* sym "%", mkDiv <* sym "/"]
        >+ ops InfixL [mkAdd <* sym "+", mkSub <* sym "-"]
        >+ ops
          InfixN
          [mkGTE <* sym ">=", mkGT <* sym ">", mkLTE <* sym "<=", mkLT <* sym "<"]
        >+ ops InfixN [mkEq <* sym "==", mkIneq <* sym "!="]
        >+ ops InfixR [sym "&&" *> mkAnd]
        >+ ops InfixR [sym "||" *> mkOr]
    )
