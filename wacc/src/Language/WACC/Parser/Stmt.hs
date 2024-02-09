{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- |
Defines the parser for WACC statements.
-}
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
  , checkFunc
  , parseWithError
  )
where

import Control.Applicative (asum, liftA3)
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty as D (last)
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
import Language.WACC.Error (Error (Error, errorMessage, position, width))
import Language.WACC.Parser.Common ()
import Language.WACC.Parser.Expr (expr)
import Language.WACC.Parser.Token (identifier)
import Language.WACC.Parser.Type (wType)
import Text.Gigaparsec
  ( Parsec
  , Result (Failure, Success)
  , eof
  , many
  , parse
  , some
  , ($>)
  , (<|>)
  , (<~>)
  )
import Text.Gigaparsec.Combinator (choice, option, sepBy1)
import Text.Gigaparsec.Errors.Combinator as E (explain, label)
import Text.Gigaparsec.Errors.ErrorGen
  ( ErrorGen (SpecializedGen, adjustWidth, messages)
  )
import Text.Gigaparsec.Errors.Patterns
  ( preventWith
  , preventativeExplain
  , verifiedExplain
  )
import Text.Gigaparsec.Patterns (deriveLiftedConstructors)
import Text.Gigaparsec.Position (Pos, pos)

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

-- | > program ::= "begin" <func>* <stmt> "end"
program :: Parsec (Prog String String)
program =
  mkBegin "begin"
    *> (program' <|> _emptyProgram)
    <* ("end" <|> _unclosedEnd "main body")
    <* _semiColonAfterEnd

{- |
> <prog> ::= <func> <prog>
-}
program' :: Parsec (Prog String String)
program' = parseProgPrefix >>= parseProgRest

{- |
Disambiguating statement declarations from function calls.
-}
func' :: Parsec ([(WType, String)], Stmts String String)
func' =
  ((concat <$> option paramList) <* ")")
    <~> ("is" *> stmts <* ("end" <|> _unclosedEnd "function body"))

{- |
Parser that handles sequential statements.
-}
stmts' :: Parsec [Stmt String String]
stmts' = many (";" *> (stmt <|> _extraSemiColon))

{- |
Parser that parses the function declaration prefix.
-}
parseFuncPreix :: Parsec (((WType, Pos), String), Bool)
parseFuncPreix =
  _checkFunc
    *> mkFunc' (wType <~> pos <~> identifier <~> (("(" $> True) <|> ("=" $> False)))

{- |
Parser that parses the program declaration prefix.
-}
parseProgPrefix :: Parsec (Maybe (((WType, Pos), String), Bool))
parseProgPrefix = option parseFuncPreix

{- |
Disambiguating Parser that parses the remainder of the program.
-}
parseProgRest
  :: Maybe (((WType, Pos), String), Bool) -> Parsec (Prog String String)
parseProgRest Nothing = mkMain1 stmts
parseProgRest (Just (((wtype, p), ident), True)) = do
  (params, ss) <- func'
  mkMain2 p wtype ident (params, ss) <$> program'
parseProgRest (Just (((wtype, p), ident), False)) = do
  rvalue <- rValue
  mkMain3 p wtype ident rvalue <$> stmts'

{- |
Parser that handles empty programs.
-}
mkMain1 :: Parsec (Stmts fnident ident) -> Parsec (Prog fnident ident)
mkMain1 = mkMain (pure [])

{- |
Parser that handles non-empty programs with function declaration.
-}
mkMain2
  :: Pos
  -> WType
  -> fnident
  -> ([(WType, ident)], Stmts fnident ident)
  -> Prog fnident ident
  -> Prog fnident ident
mkMain2 p' wtype ident (params, ss) (Main fs sts p) =
  Main (Func wtype ident params ss p' : fs) sts p

{- |
Parser that handles non-empty programs without function declaration and statement declaration.
-}
mkMain3
  :: Pos
  -> WType
  -> ident
  -> RValue fnident ident
  -> [Stmt fnident ident]
  -> Prog fnident ident
mkMain3 p wtype ident rvalue sts =
  Main [] (fromList (Decl wtype ident rvalue p : sts)) p

-- | > func ::= <type> <identifier> '(' <paramList>? ')' 'is' <stmt> 'end'
func :: Parsec (Func String String)
func =
  mkFunc
    wType
    identifier
    ("(" *> (concat <$> option paramList) <* ")")
    ("is" *> stmts <* "end")

{- |
Checks that function body does not contain any code after a return or exit statement.
-}
checkFunc :: Prog fnident ident -> Result Error (Prog fnident ident)
checkFunc s@(Main [] _ _) = Success s
checkFunc s@(Main fs _ _) = case res of
  Just p ->
    Failure
      Error
        { errorMessage =
            "Function can only be exited via a 'return' or 'exit' statement. \
            \There must not be any code following the last 'return' or 'exit' of any execution path."
        , position = p
        , width = 1
        }
  Nothing -> Success s
  where
    checkFunc' (Func _ _ _ ss _) = funcExit (D.last ss)
    res = asum $ map checkFunc' fs

{- |
Traverse Tree to find the last statement in a function body.
-}
funcExit :: Stmt fnident ident -> Maybe Pos
funcExit (Return _ _) = Nothing
funcExit (Exit _ _) = Nothing
funcExit (IfElse _ s1 s2 _) =
  funcExit (D.last s1)
    <|> funcExit (D.last s2)
funcExit (While _ s _) = funcExit (D.last s)
funcExit (BeginEnd s _) = funcExit (D.last s)
funcExit (Skip p) = Just p
funcExit (Decl _ _ _ p) = Just p
funcExit (Asgn _ _ p) = Just p
funcExit (Read _ p) = Just p
funcExit (Free _ p) = Just p
funcExit (Print _ p) = Just p
funcExit (PrintLn _ p) = Just p

{- |
Parser which parses the program with trailing statement checks.
-}
parseWithError
  :: Parsec (Prog fnident ident) -> String -> Result Error (Prog fnident ident)
parseWithError parser sourceCode = case res of
  Success x -> checkFunc x
  e -> e
  where
    res = parse @Error parser sourceCode

-- | > <param> ::= <type> <identifier>
param :: Parsec (WType, String)
param = wType <~> identifier

{- |
Parser that handles the function parameter list.
-}
paramList :: Parsec [(WType, String)]
paramList = param `sepBy1` ","

-- | > <lvalue> ::= <ident> | <array-elem> | <pair-elem>
lValue :: Parsec (LValue String)
lValue =
  choice
    [lValueOrIdent, mkLVPairElem pairElem]

{- |
Parser that handles the identifier and array element parsers disambiguation.
-}
mkIdentOrArrayElem
  :: Parsec Pos
  -> Parsec String
  -> Parsec (Maybe [Expr String])
  -> Parsec (LValue String)
mkIdentOrArrayElem = liftA3 mkIdentOrArrayElem'
  where
    mkIdentOrArrayElem' p str (Just e) = LVArrayElem (ArrayIndex str e p) p
    mkIdentOrArrayElem' p str Nothing = LVIdent str p

{- |
Parser that handles the lValues parsers disambiguation.
-}
lValueOrIdent :: Parsec (LValue String)
lValueOrIdent =
  mkIdentOrArrayElem
    pos
    identifier
    (option (some (arrayIndex "[" *> expr <* "]")))

{- |
Labels the array index parser.
-}
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

-- | > <array-liter> ::= '[' <argList>? ']'
arrayLiter :: Parsec [Expr String]
arrayLiter = arrayLiteral "[" *> optionalArgList <* "]"

-- | > <newpair> ::= "newpair" '(' <expr> ',' <expr> ')'
newPair :: Parsec (RValue fnident String)
newPair = mkRVNewPair ("newpair" *> "(" *> expr) ("," *> expr <* ")")

-- | > <pair-elem> ::= "fst" <lvalue> | "snd" <lvalue>
pairElem :: Parsec (PairElem String)
pairElem = mkFstElem ("fst" *> lValue) <|> mkSndElem ("snd" *> lValue)

-- | > <fn-call> ::= <identifier> '(' <argList>? ')'
fnCall :: Parsec (RValue String String)
fnCall = mkRVCall (mkCall "call" *> identifier) ("(" *> optionalArgList <* ")")

-- | > <argList>?
optionalArgList :: Parsec [Expr String]
optionalArgList = concat <$> option argList

-- | > <argList> ::= <expr> (',' <expr>)*
argList :: Parsec [Expr String]
argList = expr `sepBy1` ","

{- |
Parser that constructs a sequnce of statements.
-}
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
      [ mkSkip <* "skip"
      , decl
      , asgn
      , mkRead $ "read" *> lValue
      , mkFree $ "free" *> expr
      , mkReturn $ "return" *> expr
      , mkExit $ "exit" *> expr
      , mkPrint $ "print" *> expr
      , mkPrintLn ("println" *> (expr <|> _arrayLiteral <|> _pairLookup))
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

{- |
Labels the function call in error messages.
-}
mkCall :: Parsec a -> Parsec a
mkCall = label (Set.singleton "function call")

{- |
Labels the function declaration in error messages.
-}
mkFunc' :: Parsec a -> Parsec a
mkFunc' = label (Set.fromList ["function declaration"])

{- |
Labels the array literal in error messages.
-}
arrayLiteral :: Parsec a -> Parsec a
arrayLiteral = label (Set.singleton "array literal")

{- |
Explains the empty program body rror message.
-}
_emptyProgram :: Parsec b
_emptyProgram = verifiedExplain (const "missing main program body") "end"

{- |
Explains the unclosed end of scope error message.
-}
_unclosedEnd :: String -> Parsec b
_unclosedEnd place = verifiedExplain (const $ "unclosed " ++ place) eof

{- |
Explains the missing do keyword in 'While' error message.
-}
_missingDo :: Parsec b
_missingDo =
  verifiedExplain
    (const "the condition of a while loop must be closed with `do`")
    stmts

{- |
Explains the missing done keyword in 'While' error message.
-}
_done :: Parsec ()
_done = explain "unclosed while loop" "done"

{- |
Explains trailing semi-colon after end error message.
-}
_semiColonAfterEnd :: Parsec ()
_semiColonAfterEnd =
  preventativeExplain
    (const "semi-colons cannot follow the `end` of the program")
    ";"

{- |
Explain the extra semi-colon in sequencing statements error message.
-}
_extraSemiColon :: Parsec b
_extraSemiColon =
  verifiedExplain
    ( const
        "extra semi-colons are not valid, there must be exactly one between each statement"
    )
    ";"

{- |
Explains array literal usage in error messages.
-}
_arrayLiteral :: Parsec b
_arrayLiteral = verifiedExplain (const "array literals can only appear in assignments") "["

{- |
Explaions pair lookup usage in error messages.
-}
_pairLookup :: Parsec b
_pairLookup =
  verifiedExplain
    (const "tuple extraction is only allowed in assignments")
    ("fst" <|> "snd")

{- |
Explains missing 'else' keyword in 'IfElse' error messages.
-}
_else :: Parsec ()
_else = explain "all if statements must have an else clause" "else"

{- |
Explains missing 'fi' keyword in 'IfElse' error messages.
-}
_fi :: Parsec ()
_fi = explain "unclosed if statement" "fi"

{- |
Explains missing 'then' keyword in 'IfElse' error messages.
-}
_then :: Parsec ()
_then = explain "the condition of an if statement must be closed with `then`" "then"

{- |
Explains missing types in function declarations error messages.
-}
_checkFunc :: Parsec ()
_checkFunc =
  preventWith
    ( SpecializedGen
        { messages = \x -> ["function declaration for `" ++ x ++ "` missing type"]
        , adjustWidth = const id
        }
    )
    (identifier <* "()")

{- |
Explains the late function declaration error message.
-}
_funcLateDefine :: Parsec ()
_funcLateDefine =
  preventativeExplain
    (const "function declaration must be at the beginning of the program")
    func
