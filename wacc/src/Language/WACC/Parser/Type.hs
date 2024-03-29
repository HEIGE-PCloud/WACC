{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Defines the parser for WACC types.
-}
module Language.WACC.Parser.Type
  ( wType
  , baseType
  , arrayType
  , pairElemType
  , pairType
  )
where

import qualified Data.Set as Set
import Language.WACC.AST.WType (WType (..))
import Language.WACC.Parser.Common ()
import Text.Gigaparsec (Parsec, many, notFollowedBy, some, (<|>))
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Errors.Combinator (label)
import Text.Gigaparsec.Errors.Patterns (preventativeExplain)
import Text.Gigaparsec.Expr.Chain (postfix1)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$( deriveDeferredConstructors
    "mk"
    ['WBool, 'WInt, 'WChar, 'WString, 'WErasedPair]
 )

$( deriveLiftedConstructors
    "mk"
    ['WKnownPair]
 )

-- | > <type> ::= <base-type> | <array-type> | <pair-type>
wType :: Parsec WType
wType = mkWType (baseType <|> pairType) (_arrayType (many "[]"))

{- |
Lifted Constructor for WType
-}
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

{- |
Parser for square brackets.
-}
squareBrackets :: Parsec ()
squareBrackets = "[" *> "]"

-- | > <array-type> ::= <type> '[' ']'
arrayType :: Parsec WType
arrayType = postfix1 id (baseType <|> pairType) (squareBrackets >> pure WArray)

{- |
Parser for 'WType' within a pair.
-}
pairBrackets :: Parsec WType
pairBrackets = "(" *> mkWKnownPair (pairElemType <* ",") pairElemType <* ")"

-- | > <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
pairType :: Parsec WType
pairType = "pair" *> pairBrackets

-- | > <pair-elem-type> ::= <base-type> | <array-type> | 'pair'
pairElemType :: Parsec WType
pairElemType =
  _nestedPair
    *> mkWType baseType (many squareBrackets)
      <|> ("pair" *> (mkWType pairBrackets (some squareBrackets) <|> mkWErasedPair))

{- |
Labels the array type in error messages.
-}
_arrayType :: Parsec a -> Parsec a
_arrayType = label (Set.singleton "array type")

{- |
Labels the nested pair in error messages.
-}
_nestedPair :: Parsec ()
_nestedPair =
  preventativeExplain
    (const "pair types may not be nested")
    (pairType <* notFollowedBy "[]")
