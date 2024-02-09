{-# LANGUAGE TemplateHaskell #-}

{- |
Overload Strings to use the lexer.
-}
module Language.WACC.Parser.Common where

import Language.WACC.Parser.Token (lexer)
import Text.Gigaparsec.Token.Patterns (overloadedStrings)

$(overloadedStrings [|lexer|])
