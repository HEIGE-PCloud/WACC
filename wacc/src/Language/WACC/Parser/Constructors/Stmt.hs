{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Lifted constructors for data types defined in "Language.WACC.AST.Stmt".
-}
module Language.WACC.Parser.Constructors.Stmt
  ( -- * 'AST.PairElem'
    mkFstElem
  , mkSndElem

    -- * 'AST.LValue'
  , mkLVIdent
  , mkLVArrayElem
  , mkLVPairElem

    -- * 'AST.RValue'
  , mkRVExpr
  , mkRVArrayLit
  , mkRVNewPair
  , mkRVPairElem
  , mkRVCall

    -- * 'AST.Stmt'
  , mkSkip
  , mkDecl
  , mkAsgn
  , mkRead
  , mkFree
  , mkReturn
  , mkExit
  , mkPrint
  , mkPrintLn
  , mkIfElse
  , mkWhile
  , mkBeginEnd
  )
where

import Language.WACC.AST
  ( ArrayIndex
  , Expr
  , LValue
  , PairElem
  , RValue
  , Stmt
  , Stmts
  , WType
  )
import qualified Language.WACC.AST as AST
import Text.Gigaparsec.Patterns (deriveLiftedConstructors)
import Text.Gigaparsec.Position (Pos)

pattern FstElem :: LValue ident Pos -> Pos -> PairElem ident Pos
pattern FstElem lv p = AST.FstElem lv p

pattern SndElem :: LValue ident Pos -> Pos -> PairElem ident Pos
pattern SndElem lv p = AST.SndElem lv p

pattern LVIdent :: ident -> Pos -> LValue ident Pos
pattern LVIdent v p = AST.LVIdent v p

pattern LVArrayElem :: ArrayIndex ident Pos -> Pos -> LValue ident Pos
pattern LVArrayElem ae p = AST.LVArrayElem ae p

pattern LVPairElem :: PairElem ident Pos -> Pos -> LValue ident Pos
pattern LVPairElem pe p = AST.LVPairElem pe p

pattern RVExpr :: Expr ident Pos -> Pos -> RValue fnident ident Pos
pattern RVExpr x p = AST.RVExpr x p

pattern RVArrayLit :: [Expr ident Pos] -> Pos -> RValue fnident ident Pos
pattern RVArrayLit xs p = AST.RVArrayLit xs p

pattern RVNewPair
  :: Expr ident Pos -> Expr ident Pos -> Pos -> RValue fnident ident Pos
pattern RVNewPair x y p = AST.RVNewPair x y p

pattern RVPairElem :: PairElem ident Pos -> Pos -> RValue fnident ident Pos
pattern RVPairElem pe p = AST.RVPairElem pe p

pattern RVCall :: fnident -> [Expr ident Pos] -> Pos -> RValue fnident ident Pos
pattern RVCall f xs p = AST.RVCall f xs p

pattern Skip :: Pos -> Stmt fnident ident Pos
pattern Skip p = AST.Skip p

pattern Decl
  :: WType -> ident -> RValue fnident ident Pos -> Pos -> Stmt fnident ident Pos
pattern Decl t v rv p = AST.Decl t v rv p

pattern Asgn
  :: LValue ident Pos
  -> RValue fnident ident Pos
  -> Pos
  -> Stmt fnident ident Pos
pattern Asgn lv rv p = AST.Asgn lv rv p

pattern Read :: LValue ident Pos -> Pos -> Stmt fnident ident Pos
pattern Read lv p = AST.Read lv p

pattern Free :: Expr ident Pos -> Pos -> Stmt fnident ident Pos
pattern Free x p = AST.Free x p

pattern Return :: Expr ident Pos -> Pos -> Stmt fnident ident Pos
pattern Return x p = AST.Return x p

pattern Exit :: Expr ident Pos -> Pos -> Stmt fnident ident Pos
pattern Exit x p = AST.Exit x p

pattern Print :: Expr ident Pos -> Pos -> Stmt fnident ident Pos
pattern Print x p = AST.Print x p

pattern PrintLn :: Expr ident Pos -> Pos -> Stmt fnident ident Pos
pattern PrintLn x p = AST.PrintLn x p

pattern IfElse
  :: Expr ident Pos
  -> Stmts fnident ident Pos
  -> Stmts fnident ident Pos
  -> Pos
  -> Stmt fnident ident Pos
pattern IfElse x t f p = AST.IfElse x t f p

pattern While
  :: Expr ident Pos -> Stmts fnident ident Pos -> Pos -> Stmt fnident ident Pos
pattern While x b p = AST.While x b p

pattern BeginEnd :: Stmts fnident ident Pos -> Pos -> Stmt fnident ident Pos
pattern BeginEnd b p = AST.BeginEnd b p

$( deriveLiftedConstructors
    "mk"
    [ 'FstElem
    , 'SndElem
    , 'LVIdent
    , 'LVArrayElem
    , 'LVPairElem
    , 'RVExpr
    , 'RVArrayLit
    , 'RVNewPair
    , 'RVPairElem
    , 'RVCall
    , 'Skip
    , 'Decl
    , 'Asgn
    , 'Read
    , 'Free
    , 'Return
    , 'Exit
    , 'Print
    , 'PrintLn
    , 'IfElse
    , 'While
    , 'BeginEnd
    ]
 )
