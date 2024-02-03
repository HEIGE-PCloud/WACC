{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Stmt where

import Control.Applicative (Alternative (many))
import Data.List.NonEmpty (fromList)
import qualified Data.Maybe
import Language.WACC.AST.Expr (ArrayIndex (..), Expr)
import Language.WACC.AST.Stmt
  ( LValue (..)
  , PairElem (..)
  , RValue (..)
  , Stmt (..)
  , Stmts
  )
import Language.WACC.Parser.Expr (expr)
import Language.WACC.Parser.Token (identifier, sym)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Combinator (choice, option)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$( deriveLiftedConstructors
    "mk"
    ['LVPairElem]
 )

$( deriveDeferredConstructors
    "mk"
    ['LVIdent, 'LVArrayElem]
 )

$( deriveLiftedConstructors
    "mk"
    ['FstElem, 'SndElem]
 )

$( deriveLiftedConstructors
    "mk"
    ['RVExpr, 'RVArrayLit, 'RVPairElem]
 )

$( deriveDeferredConstructors
    "mk"
    ['RVNewPair, 'RVCall]
 )

$( deriveLiftedConstructors
    "mk"
    ['Skip, 'Read, 'Free, 'Return, 'Exit, 'Print, 'PrintLn]
 )

$( deriveDeferredConstructors
    "mk"
    ['Decl, 'IfElse, 'While, 'BeginEnd, 'Asgn]
 )

pairElem :: Parsec (PairElem String)
pairElem = sym "fst" *> mkFstElem lValue <|> sym "snd" *> mkSndElem lValue

lValue :: Parsec (LValue String)
lValue =
  choice
    [lVArrOrIdent, mkLVPairElem pairElem]

lVArrOrIdent :: Parsec (LValue String)
lVArrOrIdent = do
  s <- identifier
  exprs <- many (sym "[" *> expr <* sym "]")
  f <- mkLVIdent
  g <- mkLVArrayElem
  case exprs of
    [] -> pure (f s)
    _ -> pure (g (ArrayIndex s exprs))

rValue :: Parsec (RValue String String)
rValue =
  choice
    [ mkRVExpr expr
    , mkRVArrayLit arrayLit
    , newPair
    , mkRVPairElem pairElem
    , fnCall
    ]

arrayLit :: Parsec [Expr String]
arrayLit = do
  sym "["
  mexps <- option arrexps
  sym "]"

  pure $ Data.Maybe.fromMaybe [] mexps
  where
    arrexps = do
      e <- expr
      es <- many (sym "," *> expr)
      pure (e : es)

newPair :: Parsec (RValue fnident String)
newPair = do
  sym "newpair"
  sym "("
  e1 <- expr
  sym ","
  e2 <- expr
  sym ")"
  f <- mkRVNewPair
  pure $ f e1 e2

fnCall :: Parsec (RValue String String)
fnCall = do
  sym "call"
  i <- identifier
  sym "("
  mexps <- option arrexps
  sym ")"

  let
    exps = Data.Maybe.fromMaybe [] mexps
  f <- mkRVCall
  pure $ f i exps
  where
    arrexps = do
      e <- expr
      es <- many (sym "," *> expr)
      pure (e : es)

stmt :: Parsec (Stmt String String)
stmt =
  choice
    [ sym "skip" *> mkSkip
    , decl
    , asgn
    , sym "read" *> mkRead lValue
    , sym "free" *> mkFree expr
    , sym "return" *> mkReturn expr
    , sym "exit" *> mkExit expr
    , sym "print" *> mkPrint expr
    , sym "println" *> mkPrintLn expr
    , ifElse
    , while
    , beginEnd
    ]

decl :: Parsec (Stmt String String)
decl = do
  wt <- wType
  i <- identifier
  sym "="
  r <- rValue
  f <- mkDecl
  pure $ f wt i r

asgn :: Parsec (Stmt String String)
asgn = do
  lv <- lValue
  sym "="
  rv <- rValue
  f <- mkAsgn
  pure $ f lv rv

stmts :: Parsec (Stmts String String)
stmts = do
  s <- stmt
  ss <- many (sym ";" *> stmt)
  pure $ fromList (s : ss)

ifElse :: Parsec (Stmt String String)
ifElse = do
  sym "if"
  e1 <- expr
  sym "then"
  s1 <- stmts
  sym "else"
  s2 <- stmts
  sym "fi"
  f <- mkIfElse
  pure $ f e1 s1 s2

while :: Parsec (Stmt String String)
while = do
  sym "while"
  e1 <- expr
  sym "do"
  s <- stmts
  sym "done"
  f <- mkWhile
  pure $ f e1 s

beginEnd :: Parsec (Stmt String String)
beginEnd = do
  sym "begin"
  s <- stmts
  sym "end"
  f <- mkBeginEnd
  pure $ f s
