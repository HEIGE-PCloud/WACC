{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Type where

import Control.Applicative (many)
import Data.Functor (void)
import Data.Maybe (isNothing)
import Language.WACC.AST.WType (WType (..))
import Language.WACC.Parser.Token (sym)
import Text.Gigaparsec (Parsec, atomic, lookAhead, some, (<~>))
import Text.Gigaparsec.Combinator (choice, option)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  )

$( deriveDeferredConstructors
    "mk"
    ['WBool, 'WInt, 'WChar, 'WString, 'WErasedPair, 'WKnownPair, 'WArray]
 )

wTypeWithArray :: Parsec WType -> Parsec WType
wTypeWithArray p = do
  (t, ln) <- p <~> many (void (sym "[]"))
  pure $ foldr (const WArray) t ln

wTypeWithArray1 :: Parsec WType -> Parsec WType
wTypeWithArray1 p = do
  (t, ln) <- p <~> some (void (sym "[]"))
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

pairTypeOrErased :: Parsec WType
pairTypeOrErased = do
  m <- option (atomic $ lookAhead (sym "pair" *> sym "("))
  if isNothing m
    then sym "pair" *> mkWErasedPair
    else wTypeWithArray1 wPairType

pairElemType :: Parsec WType
pairElemType =
  choice
    [ lookAhead (sym "pair") *> pairTypeOrErased
    , wTypeWithArray wBaseType
    ]
