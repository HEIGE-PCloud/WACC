{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WACC.TAC.Stmt where

import Data.DList
-- import Data.Either (Either)

-- import Language.WACC.TAC.Expr (ExprTACs (..))
-- import Language.WACC.TAC.FType (flatten)
-- import Language.WACC.TAC.RValue

import qualified Data.List.NonEmpty as NE
import Language.WACC.AST.Stmt (Stmts (..))
import qualified Language.WACC.AST.Stmt as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.FType (flatten)
import Language.WACC.TAC.State
  ( TACM
  , appendBlock
  , collectTACs
  , completeBlock
  , freshLabel
  , into
  , putTACs
  , tempWith
  )
import Language.WACC.TAC.TAC
  ( BasicBlock (..)
  , Jump (..)
  , Label (..)
  , TAC (..)
  , Var (..)
  )
import Language.WACC.TAC.Value
import Language.WACC.TypeChecking (BType)

type instance TACIdent (AST.Stmt fnident ident BType) = ident

type instance TACIdent (AST.Stmts fnident ident BType) = ident

data StmtTACs ident fnident
  = Blocks
      ( Jump ident fnident
        -> TACM ident fnident (DList (BasicBlock ident fnident), Jump ident fnident)
      )
  | BlockTerminal (Jump ident fnident)

instance
  (Enum fnident, Enum ident, Ord fnident)
  => FnToTAC (AST.Stmt fnident ident BType)
  where
  type
    TACFnRepr (AST.Stmt fnident ident BType) =
      Maybe
        (StmtTACs ident fnident)

  type TACFnIdent (AST.Stmt fnident ident BType) = fnident

  fnToTAC (AST.Skip _) = pure Nothing
  fnToTAC (AST.Decl _ x rv _) = Nothing <$ fnToTAC rv `into` Var x
  fnToTAC (AST.Asgn lv rv _) = Nothing <$ lvToTAC lv (LVStore rv)
  fnToTAC (AST.Read lv _) = Nothing <$ lvToTAC lv LVRead
  fnToTAC (AST.Free e _) = do
    t <- tempWith (toTAC e)
    putTACs [Free t]
    pure Nothing
  fnToTAC (AST.Return e _) = Just . BlockTerminal . Ret <$> tempWith (toTAC e)
  fnToTAC (AST.Print x ann) = do
    t <- tempWith (toTAC x)
    putTACs [Print t (flatten ann)]
    pure Nothing
  fnToTAC (AST.PrintLn x ann) = do
    t <- tempWith (toTAC x)
    putTACs [PrintLn t (flatten ann)]
    pure Nothing
  fnToTAC (AST.Exit e ann) = do
    t <- freshTemp
    toTAC e `into` t
    pure $ Just $ BlockTerminal $ Exit t
  fnToTAC (AST.While e s ann) = undefined
  fnToTAC (AST.BeginEnd s ann) = undefined

instance
  (Enum fnident, Enum ident, Ord fnident)
  => FnToTAC (AST.Stmts fnident ident BType)
  where
  type TACFnIdent (AST.Stmts fnident ident BType) = fnident
  type
    TACFnRepr (AST.Stmts fnident ident BType) =
      ( Jump ident fnident
        -> TACM ident fnident (DList (BasicBlock ident fnident), Label fnident)
      )
  fnToTAC stmts = do
    l <- freshLabel
    (fnToTAC' l . NE.toList . unwrap) stmts
    where
      fnToTAC'
        :: (Enum fnident, Enum ident, Ord fnident)
        => fnident
        -> [AST.Stmt fnident ident BType]
        -> TACM
            ident
            fnident
            ( Jump ident fnident
              -> TACM ident fnident (DList (BasicBlock ident fnident), Label fnident)
            )
      fnToTAC' kp (x : xs) =
        fnToTAC x >>= \case
          Nothing -> fnToTAC' kp xs
          Just (BlockTerminal j) -> do
            completeBlock j kp
            l <- freshLabel
            fnToTAC' l xs
          Just (Blocks f) -> do
            ts <- collectTACs
            fl <- freshLabel
            g <- fnToTAC' fl xs
            pure $ \j -> do
              (bs, fj) <- f j
              appendBlock (BasicBlock (toList ts) fj) kp
              (bs', _) <- g j
              pure (bs <> bs', Label kp)
      fnToTAC' kp [] = pure $ \j -> (mempty, Label kp) <$ completeBlock j kp
