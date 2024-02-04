{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Expr (expr)
import Language.WACC.Parser.Token (identifier)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Combinator (choice, option)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )

$( deriveLiftedConstructors
    "mk"
    [ 'LVPairElem
    , 'FstElem
    , 'SndElem
    , 'RVExpr
    , 'RVArrayLit
    , 'RVPairElem
    , 'Skip
    , 'Read
    , 'Free
    , 'Return
    , 'Exit
    , 'Print
    , 'PrintLn
    ]
 )

$( deriveDeferredConstructors
    "mk"
    [ 'RVNewPair
    , 'RVCall
    , 'Decl
    , 'IfElse
    , 'While
    , 'BeginEnd
    , 'Asgn
    , 'LVIdent
    , 'LVArrayElem
    ]
 )

pairElem :: Parsec (PairElem String)
pairElem = "fst" *> mkFstElem lValue <|> "snd" *> mkSndElem lValue

lValue :: Parsec (LValue String)
lValue =
  choice
    [lVArrOrIdent, mkLVPairElem pairElem]

lVArrOrIdent :: Parsec (LValue String)
lVArrOrIdent = do
  s <- identifier
  exprs <- many ("[" *> expr <* "]")
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
  "["
  mexps <- option arrexps
  "]"

  pure $ Data.Maybe.fromMaybe [] mexps
  where
    arrexps = do
      e <- expr
      es <- many ("," *> expr)
      pure (e : es)

newPair :: Parsec (RValue fnident String)
newPair = do
  "newpair"
  "("
  e1 <- expr
  ","
  e2 <- expr
  ")"
  f <- mkRVNewPair
  pure $ f e1 e2

fnCall :: Parsec (RValue String String)
fnCall = do
  "call"
  i <- identifier
  "("
  mexps <- option arrexps
  ")"

  let
    exps = Data.Maybe.fromMaybe [] mexps
  f <- mkRVCall
  pure $ f i exps
  where
    arrexps = do
      e <- expr
      es <- many ("," *> expr)
      pure (e : es)

stmt :: Parsec (Stmt String String)
stmt =
  choice
    [ "skip" *> mkSkip
    , decl
    , asgn
    , "read" *> mkRead lValue
    , "free" *> mkFree expr
    , "return" *> mkReturn expr
    , "exit" *> mkExit expr
    , "print" *> mkPrint expr
    , "println" *> mkPrintLn expr
    , ifElse
    , while
    , beginEnd
    ]

decl :: Parsec (Stmt String String)
decl = do
  wt <- wType
  i <- identifier
  "="
  r <- rValue
  f <- mkDecl
  pure $ f wt i r

asgn :: Parsec (Stmt String String)
asgn = do
  lv <- lValue
  "="
  rv <- rValue
  f <- mkAsgn
  pure $ f lv rv

stmts :: Parsec (Stmts String String)
stmts = do
  s <- stmt
  ss <- many (";" *> stmt)
  pure $ fromList (s : ss)

ifElse :: Parsec (Stmt String String)
ifElse = do
  "if"
  e1 <- expr
  "then"
  s1 <- stmts
  "else"
  s2 <- stmts
  "fi"
  f <- mkIfElse
  pure $ f e1 s1 s2

while :: Parsec (Stmt String String)
while = do
  "while"
  e1 <- expr
  "do"
  s <- stmts
  "done"
  f <- mkWhile
  pure $ f e1 s

beginEnd :: Parsec (Stmt String String)
beginEnd = do
  "begin"
  s <- stmts
  "end"
  f <- mkBeginEnd
  pure $ f s
