{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Type where

import Control.Applicative (many)
import Data.Functor (void)
import Language.WACC.AST (WType)
import Language.WACC.AST.WType (WType (..))
import Language.WACC.Parser.Token (sym)
import Text.Gigaparsec (Parsec, some, (<~>))
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$( deriveDeferredConstructors
    "mk"
    ['WBool, 'WInt, 'WChar, 'WString, 'WErasedPair, 'WKnownPair, 'WArray]
 )

wTypeWithArray :: Parsec WType -> Parsec WType
wTypeWithArray p = do
  (t, ln) <- p <~> many (void (sym "[]"))
  pure $ foldr (const WArray) t ln

wType :: Parsec WType
wType = wTypeWithArray (choice [wBaseType, wPairType])

wBaseType :: Parsec WType
wBaseType =
  choice
    [ sym "int" *> mkWInt
    , sym "bool" *> mkWBool
    , sym "char" *> mkWChar
    , sym "string" *> mkWString
    ]

wPairType :: Parsec WType
wPairType = do
  sym "pair"
  sym "("
  pet1 <- pairElemType
  sym ","
  pet2 <- pairElemType
  sym ")"

  constructor <- mkWKnownPair
  pure $ constructor pet1 pet2

pairElemType :: Parsec WType
pairElemType = choice [wTypeWithArray wBaseType, sym "pair" *> mkWErasedPair]
