{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Prog
  ( prog
  )
where

import Data.Maybe (fromMaybe)
import Language.WACC.AST.Prog (Func (..), Prog (..))
import Language.WACC.AST.WType (WType)
import Language.WACC.Parser.Stmt (stmts)
import Language.WACC.Parser.Token (identifier, sym)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, atomic, many)
import Text.Gigaparsec.Combinator (option)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$(deriveDeferredConstructors "mk" ['Func])
$(deriveLiftedConstructors "mk" ['Main])

prog :: Parsec (Prog String String)
prog = sym "begin" *> mkMain (many func) stmts <* sym "end"

func :: Parsec (Func String String)
func = do
  wt <- wType
  i <- identifier
  sym "("
  mps <- option (atomic params)
  sym ")"
  sym "is"
  ss <- stmts
  sym "end"

  f <- mkFunc

  pure $ f wt i (fromMaybe [] mps) ss

param :: Parsec (WType, String)
param = do
  wt <- wType
  i <- identifier
  pure (wt, i)

params :: Parsec [(WType, String)]
params = do
  p <- param
  ps <- many (sym "," *> param)
  pure (p : ps)
