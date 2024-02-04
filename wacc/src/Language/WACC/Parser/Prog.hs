{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.WACC.Parser.Prog where

import Data.List.NonEmpty as D (NonEmpty ((:|)), last)
import Language.WACC.AST.Prog (Func (..), Prog (..))
import Language.WACC.AST.Stmt (Stmt (..), Stmts)
import Language.WACC.AST.WType (WType)
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Stmt (stmts)
import Language.WACC.Parser.Token (fully, identifier)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, lookAhead, ($>), (<|>), (<~>))
import Text.Gigaparsec.Combinator (option, sepBy1)
import Text.Gigaparsec.Errors.Combinator as E (fail)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$(deriveDeferredConstructors "mk" ['Main])

$(deriveLiftedConstructors "mk" ['Func])

prog :: Parsec (Prog String String)
prog = fully $ "begin" *> progInner <* "end"

func :: Parsec (Func String String)
func =
  mkFunc
    wType
    identifier
    ("(" *> (concat <$> option paramList) <* ")")
    ("is" *> (stmts >>= check) <* "end")

check :: Stmts fnident ident -> Parsec (Stmts fnident ident)
check ss
  | funcExit (D.last ss) = pure ss
  | otherwise =
      E.fail $
        "Function can only be exited via a 'return' or 'exit' statement.\n \
        \ There must not be any code following the last 'return' or 'exit' of any execution path."
          :| []

funcExit :: Stmt fnident ident -> Bool
funcExit (Return _) = True
funcExit (Exit _) = True
funcExit (IfElse _ s1 s2) =
  funcExit (D.last s1)
    && funcExit (D.last s2)
funcExit (While _ s) = funcExit (D.last s)
funcExit (BeginEnd s) = funcExit (D.last s)
funcExit _ = False

param :: Parsec (WType, String)
param = wType <~> identifier

paramList :: Parsec [(WType, String)]
paramList = sepBy1 param ","

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
      "(" -> (do f <- func; (Main fs ss) <- progInner; pure (Main (f : fs) ss))
      "=" -> (do Main [] <$> stmts)
