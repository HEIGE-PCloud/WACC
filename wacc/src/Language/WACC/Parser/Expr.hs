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
import Text.Gigaparsec (Parsec, (<|>), atomic)
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

$( deriveLiftedConstructors
    "mk"
    ['IntLit, 'BoolLit, 'CharLit, 'StringLit, 'Null, 'Ident, 'ArrayElem]
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

ident :: Parsec (WAtom String)
ident = mkIdent identifier

arrayElemExpr :: Parsec (WAtom String)
arrayElemExpr = mkArrayElem $ do arrayElem

arrayElem :: Parsec (ArrayIndex String)
arrayElem = do
  s <- identifier
  exprs <- some (sym "[" *> expr <* sym "]")
  pure (ArrayIndex s (toList exprs))

atom :: Parsec (WAtom String)
atom =
  choice
    [ atomic(intLiter)
    , atomic(pairLiter)
    , atomic(boolLiter)
    , atomic(charLiter)
    , atomic(stringLiter)
    , atomic(pairLiter)
    , atomic(arrayElemExpr)
    , ident
    ]

expr :: Parsec (Expr String)
expr =
  precedence $
    Atom (mkWAtom atom)
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
        [mkGT <* sym ">", mkGTE <* sym ">=", mkLT <* sym "<", mkLTE <* sym "<="]
      >+ ops InfixN [mkEq <* sym "==", mkIneq <* sym "!="]
      >+ ops InfixR [sym "&&" *> mkAnd]
      >+ ops InfixR [sym "||" *> mkOr]
