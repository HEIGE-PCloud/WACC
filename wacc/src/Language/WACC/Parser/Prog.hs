{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Prog where

import Data.Maybe (fromMaybe)
import Language.WACC.AST.Prog (Func (..), Prog (..))
import Language.WACC.AST.WType (WType)
import Language.WACC.Parser.Stmt (stmts)
import Language.WACC.Parser.Token (identifier, sym, fully)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, atomic, many, (<|>), lookAhead)
import Text.Gigaparsec.Combinator (option, optional)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$(deriveDeferredConstructors "mk" ['Func, 'Main])

prog :: Parsec (Prog String String)
prog = fully $ sym "begin" *> progInner <* sym "end"

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

funcStmtPre = do
  wt <- wType
  i <- identifier
  c <- sym "(" <|> sym "="
  pure (wt, i, c)

progInner = do
  m <- option $ lookAhead funcStmtPre
  case m of 
    Nothing -> Main [] <$> stmts
    Just (wt , ic, c) -> case c of
                            "(" -> (do f <- func; (Main fs ss) <- progInner; pure (Main (f:fs) ss))
                            "=" -> (do Main [] <$> stmts;)