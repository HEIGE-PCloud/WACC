{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Stmt where

import Control.Applicative (Alternative (many))
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty as D (NonEmpty ((:|)), last)
import Language.WACC.AST.Expr (ArrayIndex (..), Expr)
import Language.WACC.AST.Prog (Func (..), Prog (Main))
import Language.WACC.AST.Stmt
  ( LValue (..)
  , PairElem (..)
  , RValue (..)
  , Stmt (..)
  , Stmts
  )
import Language.WACC.AST.WType (WType)
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Expr (expr)
import Language.WACC.Parser.Token (identifier)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec (Parsec, lookAhead, ($>), (<|>), (<~>))
import Text.Gigaparsec.Combinator (choice, option, sepBy1)
import Text.Gigaparsec.Errors.Combinator as E (fail)
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
    , 'IfElse
    , 'While
    , 'BeginEnd
    , 'RVNewPair
    , 'Decl
    , 'Asgn
    , 'RVCall
    , 'Func
    , 'Main
    ]
 )

$( deriveDeferredConstructors
    "mk"
    [ 'LVIdent
    , 'LVArrayElem
    ]
 )

program :: Parsec (Prog String String)
program = "begin" *> progInner <* "end"

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

func :: Parsec (Func String String)
func =
  mkFunc
    wType
    identifier
    ("(" *> (concat <$> option paramList) <* ")")
    ("is" *> (stmts >>= checkExit) <* "end")

checkExit :: Stmts fnident ident -> Parsec (Stmts fnident ident)
checkExit ss
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
arrayLit = "[" *> optionalArgList <* "]"

newPair :: Parsec (RValue fnident String)
newPair = mkRVNewPair ("newpair" *> "(" *> expr) ("," *> expr <* ")")

pairElem :: Parsec (PairElem String)
pairElem = ("fst" *> mkFstElem lValue) <|> ("snd" *> mkSndElem lValue)

fnCall :: Parsec (RValue String String)
fnCall = mkRVCall ("call" *> identifier) ("(" *> optionalArgList <* ")")

optionalArgList :: Parsec [Expr String]
optionalArgList = concat <$> option argList

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
