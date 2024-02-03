{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Type
  ( wType
  , baseType
  , arrayType
  , pairElemType
  , pairType
  )
where

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
    ['WBool, 'WInt, 'WChar, 'WString, 'WErasedPair]
 )

$( deriveLiftedConstructors
    "mk"
    ['WKnownPair]
 )

$(overloadedStrings [|lexer|])

-- | > <type> ::= <base-type> | <array-type> | <pair-type>
wType :: Parsec WType
wType = mkWType (baseType <|> pairType) (many "[]")

mkWType :: Parsec WType -> Parsec [()] -> Parsec WType
mkWType = liftA2 (foldr (const WArray))

-- | > <base-type> ::= "int" | "bool" | "char" | "string"
baseType :: Parsec WType
baseType =
  choice
    [ "int" *> mkWInt
    , "bool" *> mkWBool
    , "char" *> mkWChar
    , "string" *> mkWString
    ]

squareBrackets :: Parsec ()
squareBrackets = "[" *> "]"

-- | > <array-type> ::= <type> '[' ']'
arrayType :: Parsec WType
arrayType = postfix1 id (baseType <|> pairType) (squareBrackets >> pure WArray)

pairBrackets :: Parsec WType
pairBrackets = "(" *> mkWKnownPair (pairElemType <* ",") pairElemType <* ")"

-- | > <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
pairType :: Parsec WType
pairType = "pair" *> pairBrackets

-- | > <pair-elem-type> ::= <base-type> | <array-type> | 'pair'
pairElemType :: Parsec WType
pairElemType =
  mkWType baseType (many squareBrackets)
    <|> ("pair" *> (mkWType pairBrackets (some squareBrackets) <|> mkWErasedPair))
