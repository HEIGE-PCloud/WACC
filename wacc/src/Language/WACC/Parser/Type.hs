{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Type where

import Language.WACC.AST.WType (WType (..))
import Language.WACC.Parser.Token (lexer)
import Text.Gigaparsec (Parsec, atomic, (<|>))
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Expr.Chain (postfix1)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors,
    deriveLiftedConstructors,
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
wType = atomic arrayType <|> baseType <|> pairType

baseType :: Parsec WType
baseType =
  choice
    [ "int" *> mkWInt,
      "bool" *> mkWBool,
      "char" *> mkWChar,
      "string" *> mkWString
    ]

arrayType :: Parsec WType
arrayType = postfix1 id (baseType <|> pairType) ("[]" >> pure WArray)

pairType :: Parsec WType
pairType = "pair" *> "(" *> mkWKnownPair (pairElemType <* ",") pairElemType <* ")"

pairElemType :: Parsec WType
pairElemType = atomic arrayType <|> baseType <|> ("pair" >> mkWErasedPair)
