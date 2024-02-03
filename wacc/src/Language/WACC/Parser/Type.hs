{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Type where

import Language.WACC.AST.WType (WType (..))
import Language.WACC.Parser.Token (lexer)
import Text.Gigaparsec (Parsec, many, some, (<|>))
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Expr.Chain (postfix1)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )
import Text.Gigaparsec.Token.Patterns (overloadedStrings)

$( deriveDeferredConstructors
    "mk"
    ['WBool, 'WInt, 'WChar, 'WString, 'WErasedPair, 'WArray]
 )

$( deriveLiftedConstructors
    "mk"
    ['WKnownPair]
 )

$(overloadedStrings [|lexer|])

wType :: Parsec WType
wType = mkWType (baseType <|> pairType) (many "[]")

mkWType :: Parsec WType -> Parsec [()] -> Parsec WType
mkWType = liftA2 mkWType'
  where
    mkWType' :: WType -> [()] -> WType
    mkWType' t [] = t
    mkWType' t ts = foldr (const WArray) t ts

baseType :: Parsec WType
baseType =
  choice
    [ "int" *> mkWInt
    , "bool" *> mkWBool
    , "char" *> mkWChar
    , "string" *> mkWString
    ]

arrayType :: Parsec WType
arrayType = postfix1 id (baseType <|> pairType) ("[]" >> pure WArray)

pairBrackets :: Parsec WType
pairBrackets = "(" *> mkWKnownPair (pairElemType <* ",") pairElemType <* ")"

pairType :: Parsec WType
pairType = "pair" *> pairBrackets

pairElemType :: Parsec WType
pairElemType =
  mkWType baseType (many "[]")
    <|> ("pair" *> (mkWType pairBrackets (some "[]") <|> mkWErasedPair))
