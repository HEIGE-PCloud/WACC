{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WACC.TAC.Stmt where

import Data.DList
-- import Data.Either (Either)
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class
-- import Language.WACC.TAC.Expr (ExprTACs (..))
-- import Language.WACC.TAC.FType (flatten)
-- import Language.WACC.TAC.RValue

import Language.WACC.TAC.FType (flatten)
import Language.WACC.TAC.State (TACM, freshLabel, freshTemp, into, putTACs)
import Language.WACC.TAC.TAC
  ( BasicBlock (..)
  , BlockLabel (..)
  , Jump (..)
  , Label (..)
  , TAC (..)
  , Var (..)
  )
import Language.WACC.TAC.Value
import Language.WACC.TypeChecking (BType)

type instance TACIdent (AST.Stmt fnident ident BType) = ident

data StmtTACs ident fnident
  = Blocks (DList (Label fnident -> BasicBlock ident fnident))
  | BlockTerminal (Jump ident fnident)

instance (Enum fnident, Enum ident) => FnToTAC (AST.Stmt fnident ident BType) where
  type
    TACFnRepr (AST.Stmt fnident ident BType) =
      Maybe
        (StmtTACs ident fnident)

  type TACFnIdent (AST.Stmt fnident ident BType) = fnident

  fnToTAC (AST.Skip ann) = do
    putTACs mempty
    pure Nothing
  fnToTAC (AST.Decl _ x rv _) = do
    fnToTAC rv `into` Var x
    pure Nothing
  fnToTAC (AST.Asgn lv rv ann) = do
    f <- toTAC lv
    f (LVStore rv)
    pure Nothing
  fnToTAC (AST.Read lv ann) = do
    f <- toTAC lv
    f LVRead
    pure Nothing
  fnToTAC (AST.Free e ann) = do
    t <- freshTemp
    toTAC e `into` t
    putTACs [Free t]
    pure Nothing
  fnToTAC (AST.Return e ann) = do
    t <- freshTemp
    toTAC e `into` t
    pure $ Just $ BlockTerminal $ Ret t
  fnToTAC (AST.IfElse e s1 s2 ann) = undefined
  fnToTAC (AST.While x s ann) = undefined
  fnToTAC (AST.Print x ann) = do
    t <- freshTemp
    toTAC x `into` t
    putTACs [Print t (flatten ann)]
    pure Nothing
  fnToTAC (AST.PrintLn x ann) = do
    t <- freshTemp
    toTAC x `into` t
    putTACs [PrintLn t (flatten ann)]
    pure Nothing
  fnToTAC (AST.Exit e ann) = do
    t <- freshTemp
    toTAC e `into` t
    pure $ Just $ BlockTerminal $ Exit t
  fnToTAC (AST.While e s ann) = undefined
  fnToTAC (AST.BeginEnd s ann) = undefined

