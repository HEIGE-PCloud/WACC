{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Stmt where

import Control.Applicative (Alternative (many))
import Data.List.NonEmpty (fromList)
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
import Text.Gigaparsec.Combinator (choice, option, sepBy1)
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )
import Data.Maybe (fromMaybe)

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
    , 'IfElse
    , 'While
    , 'BeginEnd
    , 'RVNewPair
    , 'Decl
    , 'Asgn
    , 'RVCall
    ]
 )

$( deriveDeferredConstructors
    "mk"
    [ 
    'LVIdent
    , 'LVArrayElem
    ]
 )

--------------LValue--------------

lValue :: Parsec (LValue String)
lValue =
  choice
    [lValueOrIdent, mkLVPairElem pairElem]

mkIdentOrArrayElem
  :: Parsec String -> Parsec (Maybe [Expr String]) -> Parsec (LValue String)
mkIdentOrArrayElem = liftA2 mkIdentOrArrayElem'
  where
    mkIdentOrArrayElem' str (Just e) = LVArrayElem (ArrayIndex str e)
    mkIdentOrArrayElem' str Nothing = LVIdent str

lValueOrIdent :: Parsec (LValue String)
lValueOrIdent = mkIdentOrArrayElem identifier (option (many ("[" *> expr <* "]")))

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
arrayLit = "[" *> optionalArgList argList <* "]"

newPair :: Parsec (RValue fnident String)
newPair = mkRVNewPair ("newpair" *> "(" *> expr) ("," *> expr <* ")")

pairElem :: Parsec (PairElem String)
pairElem = ("fst" *> mkFstElem lValue) <|> ("snd" *> mkSndElem lValue)

fnCall :: Parsec (RValue String String)
fnCall = mkRVCall ("call" *> identifier) ("(" *> optionalArgList argList <* ")")

mkArgList :: Parsec (Maybe [a]) -> Parsec [a]
mkArgList = fmap (fromMaybe [])

optionalArgList :: Parsec [a] -> Parsec [a]
optionalArgList = mkArgList . option

argList :: Parsec [Expr String]
argList = sepBy1 expr ","

mkStmts :: Parsec [Stmt String String] -> Parsec (Stmts String String)
mkStmts = fmap fromList

stmts :: Parsec (Stmts String String)
stmts = mkStmts (sepBy1 stmt ";")

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
decl = mkDecl wType (identifier <* "=") rValue

asgn :: Parsec (Stmt String String)
asgn = mkAsgn (lValue <* "=") rValue

ifElse :: Parsec (Stmt String String)
ifElse = mkIfElse ("if" *> expr <* "then") stmts ("else" *> stmts <* "fi")

while :: Parsec (Stmt String String)
while = mkWhile ("while" *> expr <* "do") (stmts <* "done")

beginEnd :: Parsec (Stmt String String)
beginEnd = mkBeginEnd ("begin" *> stmts <* "end")
