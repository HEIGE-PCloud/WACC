{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Expr where

import Data.Foldable (Foldable (toList))
import Language.WACC.AST.Expr (ArrayIndex (ArrayIndex), Expr (..), WAtom (..))
import Language.WACC.Parser.Token
  ( charLiteral
  , decimal
  , identifier
  , stringLiteral
  , sym
  )
import Text.Gigaparsec (Parsec, (<|>), atomic, many)
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Combinator.NonEmpty (some)
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
import Control.Arrow (Arrow(arr))

$( deriveLiftedConstructors
    "mk"
    ['IntLit, 'BoolLit, 'CharLit, 'StringLit, 'Null]
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

boolLiter :: Parsec (WAtom i)
boolLiter = mkBoolLit $ (== "true") <$> (sym "true" <|> sym "false")

charLiter :: Parsec (WAtom i)
charLiter = mkCharLit charLiteral

stringLiter :: Parsec (WAtom i)
stringLiter = mkStringLit stringLiteral

pairLiter :: Parsec (WAtom i)
pairLiter = do
  sym "null"
  mkNull

arrayElem :: Parsec (ArrayIndex String)
arrayElem = do
  s <- identifier
  exprs <- some (sym "[" *> expr <* sym "]")
  pure (ArrayIndex s (toList exprs)) 

arrOrIdent :: Parsec (WAtom String)
arrOrIdent = do
  s <- identifier 
  exprs <- many (sym "[" *> expr <* sym "]")
  f <- mkIdent
  g <- mkArrayElem
  case exprs of
    [] -> pure (f s)
    _ -> pure (g (ArrayIndex s (toList exprs)))

atom :: Parsec (Expr String)
atom =
  choice
    [ WAtom <$> intLiter
    , WAtom <$> pairLiter
    , WAtom <$> boolLiter
    , WAtom <$> charLiter
    , WAtom <$> stringLiter
    , WAtom <$> pairLiter
    , WAtom <$> arrOrIdent
    , sym "(" *> expr <* sym ")"
    ]

expr :: Parsec (Expr String)
expr =
  precedence
    ( Atom atom
        >+ ops
          Prefix
          [ mkNot <* sym "!"
          , mkNegate <* sym "-"
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
