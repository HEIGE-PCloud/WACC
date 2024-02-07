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
import Text.Gigaparsec (Parsec, eof, many, ($>), (<|>), (<~>))
import Text.Gigaparsec.Combinator (choice, option, sepBy1)
import Text.Gigaparsec.Errors.Combinator as E (explain, fail, label)
import Text.Gigaparsec.Errors.ErrorGen
  ( ErrorGen (SpecializedGen, adjustWidth, messages)
  )
import Text.Gigaparsec.Errors.Patterns
  ( preventWith
  , preventativeExplain
  , verifiedExplain
  )
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
program =
  mkBegin "begin"
    *> (program' <|> _emptyProgram)
    <* ("end" <|> _unclosedEnd "main body")
    <* _semiColonAfterEnd

program' :: Parsec (Prog String String)
program' = parseProgPrefix >>= parseProgRest

func' :: Parsec ([(WType, String)], Stmts String String)
func' =
  ((concat <$> option paramList) <* ")")
    <~> ("is" *> (stmts >>= checkExit) <* ("end" <|> _unclosedEnd "function body"))

stmts' :: Parsec [Stmt String String]
stmts' = many (";" *> (stmt <|> _extraSemiColon))

parseFuncPreix :: Parsec ((WType, String), Bool)
parseFuncPreix =
  _checkFunc
    *> mkFunc' (wType <~> identifier <~> (("(" $> True) <|> ("=" $> False)))

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
paramList = param `sepBy1` ","

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
arrayIndex = label (Set.singleton "array index")

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
arrayLiter = arrayLiteral "[" *> optionalArgList <* "]"

-- | > <newpair> ::= "newpair" '(' <expr> ',' <expr> ')'
newPair :: Parsec (RValue fnident String)
newPair = mkRVNewPair ("newpair" *> "(" *> expr) ("," *> expr <* ")")

-- | > <pair-elem> ::= "fst" <lvalue> | "snd" <lvalue>
pairElem :: Parsec (PairElem String)
pairElem = ("fst" *> mkFstElem lValue) <|> ("snd" *> mkSndElem lValue)

-- | > <fn-call> ::= <identifier> '(' <argList>? ')'
fnCall :: Parsec (RValue String String)
fnCall = mkRVCall (mkCall "call" *> identifier) ("(" *> optionalArgList <* ")")

optionalArgList :: Parsec [Expr String]
optionalArgList = concat <$> option argList

-- | > <argList> ::= <expr> (',' <expr>)*
argList :: Parsec [Expr String]
argList = expr `sepBy1` ","

mkStmts :: Parsec [Stmt String String] -> Parsec (Stmts String String)
mkStmts = label (Set.fromList ["statement"]) . fmap fromList

-- | > <stmts> ::= <stmt> (';' <stmt>)*
stmts :: Parsec (Stmts String String)
stmts = mkStmts ((stmt <|> _extraSemiColon) `sepBy1` ";")

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
  _funcLateDefine
    *> choice
      [ "skip" *> mkSkip
      , decl
      , asgn
      , "read" *> mkRead lValue
      , "free" *> mkFree expr
      , "return" *> mkReturn expr
      , "exit" *> mkExit expr
      , "print" *> mkPrint expr
      , "println" *> mkPrintLn (expr <|> _arrayLiteral)
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
ifElse = mkIfElse ("if" *> expr <* _then) stmts (_else *> stmts <* _fi)

-- | > <while> ::= "while" <expr> "do" <stmts> "done"
while :: Parsec (Stmt String String)
while =
  mkWhile
    ("while" *> expr <* ("do" <|> _missingDo))
    (stmts <* _done)

-- | > <begin-end> ::= "begin" <stmts> "end"
beginEnd :: Parsec (Stmt String String)
beginEnd = mkBeginEnd ("begin" *> stmts <* ("end" <|> _unclosedEnd "block"))

mkBegin :: Parsec a -> Parsec a
mkBegin =
  explain
    "all program body and function declarations must be within `begin` and `end`"

mkCall :: Parsec a -> Parsec a
mkCall = label (Set.singleton "function call")

mkFunc' :: Parsec a -> Parsec a
mkFunc' = label (Set.fromList ["function declaration"])

arrayLiteral :: Parsec a -> Parsec a
arrayLiteral = label (Set.singleton "array literal")

_emptyProgram :: Parsec b
_emptyProgram = verifiedExplain (const "missing main program body") "end"

_unclosedEnd :: String -> Parsec b
_unclosedEnd place = verifiedExplain (const $ "unclosed " ++ place) eof

_missingDo :: Parsec b
_missingDo =
  verifiedExplain
    (const "the condition of a while loop must be closed with `do`")
    stmts

_done :: Parsec ()
_done = explain "unclosed while loop" "done"

_semiColonAfterEnd :: Parsec ()
_semiColonAfterEnd =
  preventativeExplain
    (const "semi-colons cannot follow the `end` of the program")
    ";"

_extraSemiColon :: Parsec b
_extraSemiColon =
  verifiedExplain
    ( const
        "extra semi-colons are not valid, there must be exactly one between each statement"
    )
    ";"

_arrayLiteral :: Parsec b
_arrayLiteral = verifiedExplain (const "array literals can only appear in assignments") "["

_else :: Parsec ()
_else = explain "all if statements must have an else clause" "else"

_fi :: Parsec ()
_fi = explain "unclosed if statement" "fi"

_then :: Parsec ()
_then = explain "the condition of an if statement must be closed with `then`" "then"

_checkFunc :: Parsec ()
_checkFunc =
  preventWith
    ( SpecializedGen
        { messages = \x -> ["function declaration for `" ++ x ++ "` missing type"]
        , adjustWidth = const id
        }
    )
    (identifier <* "()")

_funcLateDefine :: Parsec ()
_funcLateDefine =
  preventativeExplain
    (const "function declaration must be at the beginning of the program")
    func
