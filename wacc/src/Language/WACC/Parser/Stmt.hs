{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.WACC.Parser.Stmt
  ( program
  , func
  , paramList
  , param
  , stmts
  , lValue
  , rValue
  , argList
  , pairElem
  , arrayLiter
  )
where

import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty as D (NonEmpty ((:|)), last)
import qualified Data.Set as Set
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
import Text.Gigaparsec (Parsec, many, ($>), (<|>), (<~>))
import Text.Gigaparsec.Combinator (choice, option, sepBy1)
import Text.Gigaparsec.Errors.Combinator as E (fail, label)
import Text.Gigaparsec.Patterns (deriveLiftedConstructors)

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
    ]
 )

-- | > program ::= "begin" <func>* <stmt> "end"
program :: Parsec (Prog String String)
program = "begin" *> program' <* "end"

program' :: Parsec (Prog String String)
program' = parseProgPrefix >>= parseProgRest

func' :: Parsec ([(WType, String)], Stmts String String)
func' =
  ((concat <$> option paramList) <* ")")
    <~> ("is" *> (stmts >>= checkExit) <* "end")

stmts' :: Parsec [Stmt String String]
stmts' = many (";" *> stmt)

parseFuncPreix :: Parsec ((WType, String), Bool)
parseFuncPreix = wType <~> identifier <~> (("(" $> True) <|> ("=" $> False))

parseProgPrefix :: Parsec (Maybe ((WType, String), Bool))
parseProgPrefix = option parseFuncPreix

parseProgRest :: Maybe ((WType, String), Bool) -> Parsec (Prog String String)
parseProgRest Nothing = mkMain1 <$> stmts
parseProgRest (Just ((wtype, ident), True)) = do
  (params, ss) <- func'
  mkMain2 wtype ident (params, ss) <$> program'
parseProgRest (Just ((wtype, ident), False)) = do
  rvalue <- rValue
  mkMain3 wtype ident rvalue <$> stmts'

mkMain1 :: Stmts fnident ident -> Prog fnident ident
mkMain1 = Main []

mkMain2
  :: WType
  -> String
  -> ([(WType, String)], Stmts String String)
  -> Prog String String
  -> Prog String String
mkMain2 wtype ident (params, ss) (Main fs sts) = Main (Func wtype ident params ss : fs) sts

mkMain3
  :: WType
  -> String
  -> RValue String String
  -> [Stmt String String]
  -> Prog String String
mkMain3 wtype ident rvalue sts = Main [] (fromList (Decl wtype ident rvalue : sts))

-- | > func ::= <type> <identifier> '(' <paramList>? ')' 'is' <stmt> 'end'
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

-- | > <param> ::= <type> <identifier>
param :: Parsec (WType, String)
param = wType <~> identifier

paramList :: Parsec [(WType, String)]
paramList = sepBy1 param ","

-- | > <lvalue> ::= <ident> | <array-elem> | <pair-elem>
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
lValueOrIdent = mkIdentOrArrayElem identifier (option (many (arrayIndex "[" *> expr <* "]")))

arrayIndex :: Parsec a -> Parsec a
arrayIndex = label (Set.fromList ["array index"])

-- | > <rvalue> ::= <expr> | <array-liter> | <newpair> | <pair-elem> | <fn-call>
rValue :: Parsec (RValue String String)
rValue =
  choice
    [ mkRVExpr expr
    , mkRVArrayLit arrayLiter
    , newPair
    , mkRVPairElem pairElem
    , fnCall
    ]

arrayLiter :: Parsec [Expr String]
arrayLiter = "[" *> optionalArgList <* "]"

-- | > <newpair> ::= "newpair" '(' <expr> ',' <expr> ')'
newPair :: Parsec (RValue fnident String)
newPair = mkRVNewPair ("newpair" *> "(" *> expr) ("," *> expr <* ")")

-- | > <pair-elem> ::= "fst" <lvalue> | "snd" <lvalue>
pairElem :: Parsec (PairElem String)
pairElem = ("fst" *> mkFstElem lValue) <|> ("snd" *> mkSndElem lValue)

-- | > <fn-call> ::= <identifier> '(' <argList>? ')'
fnCall :: Parsec (RValue String String)
fnCall = mkRVCall ("call" *> identifier) ("(" *> optionalArgList <* ")")

optionalArgList :: Parsec [Expr String]
optionalArgList = concat <$> option argList

-- | > <argList> ::= <expr> (',' <expr>)*
argList :: Parsec [Expr String]
argList = sepBy1 expr ","

mkStmts :: Parsec [Stmt String String] -> Parsec (Stmts String String)
mkStmts = fmap fromList

-- | > <stmts> ::= <stmt> (';' <stmt>)*
stmts :: Parsec (Stmts String String)
stmts = mkStmts (sepBy1 stmt ";")

{- | > <stmt> ::= "skip"
 >              | <decl>
 >              | <asgn>
 >              | "read" <lvalue>
 >              | "free" <expr>
 >              | "return" <expr>
 >              | "exit" <expr>
 >              | "print" <expr>
 >              | "println" <expr>
 >              | <if-else>
 >              | <while>
 >              | <begin-end>
-}
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

-- | > <decl> ::= <type> <ident> '=' <rvalue>
decl :: Parsec (Stmt String String)
decl = mkDecl wType (identifier <* "=") rValue

-- | > <asgn> ::= <lvalue> '=' <rvalue>
asgn :: Parsec (Stmt String String)
asgn = mkAsgn (lValue <* "=") rValue

-- | > <if-else> ::= "if" <expr> "then" <stmts> "else" <stmts> "fi"
ifElse :: Parsec (Stmt String String)
ifElse = mkIfElse ("if" *> expr <* "then") stmts ("else" *> stmts <* "fi")

-- | > <while> ::= "while" <expr> "do" <stmts> "done"
while :: Parsec (Stmt String String)
while = mkWhile ("while" *> expr <* "do") (stmts <* "done")

-- | > <begin-end> ::= "begin" <stmts> "end"
beginEnd :: Parsec (Stmt String String)
beginEnd = mkBeginEnd ("begin" *> stmts <* "end")
