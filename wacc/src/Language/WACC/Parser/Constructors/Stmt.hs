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

pattern FstElem :: LValue Pos ident -> Pos -> PairElem Pos ident
pattern FstElem lv p = AST.FstElem lv p

pattern SndElem :: LValue Pos ident -> Pos -> PairElem Pos ident
pattern SndElem lv p = AST.SndElem lv p

pattern LVIdent :: ident -> Pos -> LValue Pos ident
pattern LVIdent v p = AST.LVIdent v p

pattern LVArrayElem :: ArrayIndex Pos ident -> Pos -> LValue Pos ident
pattern LVArrayElem ae p = AST.LVArrayElem ae p

pattern LVPairElem :: PairElem Pos ident -> Pos -> LValue Pos ident
pattern LVPairElem pe p = AST.LVPairElem pe p

pattern RVExpr :: Expr Pos ident -> Pos -> RValue Pos fnident ident
pattern RVExpr x p = AST.RVExpr x p

pattern RVArrayLit :: [Expr Pos ident] -> Pos -> RValue Pos fnident ident
pattern RVArrayLit xs p = AST.RVArrayLit xs p

pattern RVNewPair
  :: Expr Pos ident -> Expr Pos ident -> Pos -> RValue Pos fnident ident
pattern RVNewPair x y p = AST.RVNewPair x y p

pattern RVPairElem :: PairElem Pos ident -> Pos -> RValue Pos fnident ident
pattern RVPairElem pe p = AST.RVPairElem pe p

pattern RVCall :: fnident -> [Expr Pos ident] -> Pos -> RValue Pos fnident ident
pattern RVCall f xs p = AST.RVCall f xs p

pattern Skip :: Pos -> Stmt Pos fnident ident
pattern Skip p = AST.Skip p

pattern Decl
  :: WType -> ident -> RValue Pos fnident ident -> Pos -> Stmt Pos fnident ident
pattern Decl t v rv p = AST.Decl t v rv p

pattern Asgn
  :: LValue Pos ident
  -> RValue Pos fnident ident
  -> Pos
  -> Stmt Pos fnident ident
pattern Asgn lv rv p = AST.Asgn lv rv p

pattern Read :: LValue Pos ident -> Pos -> Stmt Pos fnident ident
pattern Read lv p = AST.Read lv p

pattern Free :: Expr Pos ident -> Pos -> Stmt Pos fnident ident
pattern Free x p = AST.Free x p

pattern Return :: Expr Pos ident -> Pos -> Stmt Pos fnident ident
pattern Return x p = AST.Return x p

pattern Exit :: Expr Pos ident -> Pos -> Stmt Pos fnident ident
pattern Exit x p = AST.Exit x p

pattern Print :: Expr Pos ident -> Pos -> Stmt Pos fnident ident
pattern Print x p = AST.Print x p

pattern PrintLn :: Expr Pos ident -> Pos -> Stmt Pos fnident ident
pattern PrintLn x p = AST.PrintLn x p

pattern IfElse
  :: Expr Pos ident
  -> Stmts Pos fnident ident
  -> Stmts Pos fnident ident
  -> Pos
  -> Stmt Pos fnident ident
pattern IfElse x t f p = AST.IfElse x t f p

pattern While
  :: Expr Pos ident -> Stmts Pos fnident ident -> Pos -> Stmt Pos fnident ident
pattern While x b p = AST.While x b p

pattern BeginEnd :: Stmts Pos fnident ident -> Pos -> Stmt Pos fnident ident
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
