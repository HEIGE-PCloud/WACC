{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.WACC.Parser.Prog where

import Data.List.NonEmpty (NonEmpty ((:|)), last)
import Data.Maybe (fromMaybe)
import Language.WACC.AST.Prog (Func (..), Prog (..))
import Language.WACC.AST.Stmt (Stmt (..))
import Language.WACC.AST.WType (WType)
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Stmt (stmts)
import Language.WACC.Parser.Token (fully, identifier)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, lookAhead, many, ($>), (<|>))
import Text.Gigaparsec.Combinator (option)
import Text.Gigaparsec.Errors.Combinator (fail)
import Text.Gigaparsec.Patterns (deriveDeferredConstructors)

$(deriveDeferredConstructors "mk" ['Func, 'Main])

prog :: Parsec (Prog String String)
prog = fully $ "begin" *> progInner <* "end"

func :: Parsec (Func String String)
func = do
  wt <- wType
  i <- identifier
  "("
  mps <- option params
  ")"
  "is"
  ss <- stmts
  "end"

  f <- mkFunc

  if statementWithReturnOrExit (Data.List.NonEmpty.last ss)
    then pure $ f wt i (fromMaybe [] mps) ss
    else
      Text.Gigaparsec.Errors.Combinator.fail
        ("Function must end with a return or exit statement" :| [])

statementWithReturnOrExit :: Stmt fnident ident -> Bool
statementWithReturnOrExit (Return _) = True
statementWithReturnOrExit (Exit _) = True
statementWithReturnOrExit (IfElse _ s1 s2) =
  statementWithReturnOrExit (Data.List.NonEmpty.last s1)
    && statementWithReturnOrExit (Data.List.NonEmpty.last s2)
statementWithReturnOrExit (While _ s) = statementWithReturnOrExit (Data.List.NonEmpty.last s)
statementWithReturnOrExit (BeginEnd s) = statementWithReturnOrExit (Data.List.NonEmpty.last s)
statementWithReturnOrExit _ = False

param :: Parsec (WType, String)
param = do
  wt <- wType
  i <- identifier
  pure (wt, i)

params :: Parsec [(WType, String)]
params = do
  p <- param
  ps <- many ("," *> param)
  pure (p : ps)

funcStmtPre :: Parsec (WType, String, String)
funcStmtPre = do
  wt <- wType
  i <- identifier
  c <- ("(" $> "(") <|> ("=" $> "=")
  pure (wt, i, c)

progInner :: Parsec (Prog String String)
progInner = do
  m <- option $ lookAhead funcStmtPre
  case m of
    Nothing -> Main [] <$> stmts
    Just (_, _, c) -> case c of
      "(" -> (do f <- func;       (Main fs ss) <- progInner; pure (Main (f : fs) ss))
      "=" -> (do Main [] <$> stmts)
