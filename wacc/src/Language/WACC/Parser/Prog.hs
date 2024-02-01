{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Prog where

import Language.WACC.AST.Prog (Prog (..))
import Language.WACC.AST.WType (WType)
import Language.WACC.Parser.Stmt (stmts)
import Language.WACC.Parser.Token (identifier, sym)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, many, (<|>))
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$(deriveDeferredConstructors "mk" ['Func])
$(deriveLiftedConstructors "mk" ['Main])

prog :: Parsec (Prog String String)
prog = sym "begin" *> progInner <* sym "end"

progInner :: Parsec (Prog String String)
progInner = funcs <|> mkMain stmts

funcs :: Parsec (Prog String String)
funcs = do
  wt <- wType
  i <- identifier
  sym "("
  p <- param
  ps <- many (sym "," *> param)
  sym ")"
  sym "is"
  ss <- stmts
  sym "end"
  f <- mkFunc

  f wt i (p : ps) ss <$> progInner

param :: Parsec (WType, String)
param = do
  wt <- wType
  i <- identifier
  pure (wt, i)
