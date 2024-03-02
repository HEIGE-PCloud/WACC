{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

{- |
TAC Translation actions for WACC @Stmt@s.
-}
module Language.WACC.TAC.Stmt (StmtTACs (..), stmtsToTAC) where

import Data.DList
import qualified Data.List.NonEmpty as NE
import Language.WACC.AST (Annotated (getAnn))
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

{- |
TAC translation result for a WACC @Stmt@ translation action.
-}
data StmtTACs ident fnident
  = -- | Basic Block translation with result left open with @Jump@ for continuation.
    Blocks (Jump ident fnident -> TACM ident fnident (Jump ident fnident))
  | -- | Jump Translation for terminal instructions like @Ret@ and @Exit@.
    BlockTerminal (Jump ident fnident)

{- |
Defines instance of @FnToTAC@ for WACC @Stmt@s. This instance is used to translate WACC @Stmt@s AST Nodes to TAC.
-}
instance
  (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => FnToTAC (AST.Stmt fnident ident BType)
  where
  -- \| TAC translation result for a WACC @Stmt@ translation action, with @Jump@ continuation.
  type
    TACFnRepr (AST.Stmt fnident ident BType) =
      Maybe
        (StmtTACs ident fnident)

  -- \| Label identifier type derived from @fnident@ type parameter of @AST.Stmt@.
  type TACFnIdent (AST.Stmt fnident ident BType) = fnident

  -- \| Translates a WACC @Stmt@ AST Node to TAC.
  fnToTAC (AST.Skip _) = pure Nothing
  fnToTAC (AST.Decl _ x rv _) = Nothing <$ fnToTAC rv `into` Var x
  fnToTAC (AST.Asgn lv rv _) = Nothing <$ lvToTAC lv (LVStore rv)
  fnToTAC (AST.Read lv _) = Nothing <$ lvToTAC lv LVRead
  fnToTAC (AST.Free e _) = do
    t <- tempWith (toTAC e)
    putTACs [Free t]
    pure Nothing
  fnToTAC (AST.Return e _) = Just . BlockTerminal . Ret <$> tempWith (toTAC e)
  fnToTAC (AST.Print x _) = do
    t <- tempWith (toTAC x)
    putTACs [Print t (flatten $ getAnn x)]
    pure Nothing
  fnToTAC (AST.PrintLn x _) = do
    t <- tempWith (toTAC x)
    putTACs [PrintLn t (flatten $ getAnn x)]
    pure Nothing
  fnToTAC (AST.Exit e _) = Just . BlockTerminal . Exit <$> tempWith (toTAC e)
  fnToTAC (AST.IfElse e s1 s2 _) = do
    t <- tempWith (toTAC e)
    fl <- freshLabel
    gl <- freshLabel

    pure $ Just $ Blocks $ \j -> do
      fa <- stmtsToTAC s1 fl
      fa j
      ga <- stmtsToTAC s2 gl
      ga j
      pure (CJump t (Label fl) (Label gl))
  fnToTAC (AST.While e s _) = do
    cl <- freshLabel
    fl <- freshLabel
    pure $ Just $ Blocks $ \j -> do
      t <- tempWith (toTAC e)
      cj' <- case j of
        Jump l -> pure $ CJump t (Label fl) l
        cj@(CJump {}) -> do
          l <- freshLabel
          appendBlock (BasicBlock [] cj) l
          pure $ CJump t (Label fl) (Label l)
        _ -> pure j
      completeBlock cj' cl
      fa <- stmtsToTAC s fl
      fa (Jump $ Label cl)
      pure $ Jump $ Label cl
  fnToTAC (AST.BeginEnd s _) = do
    fl <- freshLabel
    pure $ Just $ Blocks $ \j -> do
      fa <- stmtsToTAC s fl
      fa j
      pure . Jump $ Label fl

stmtsToTAC
  :: (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => AST.Stmts fnident ident BType
  -> fnident
  -> TACM
      ident
      fnident
      (Jump ident fnident -> TACM ident fnident ())
stmtsToTAC stmts l = fnToTAC stmts >>= ($ l)

{- |
Defines instance of @FnToTAC@ for WACC @Stmts@s. This instance is used to translate WACC @Stmts@s AST Nodes to TAC.
-}
instance
  (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => FnToTAC (AST.Stmts fnident ident BType)
  where
  -- \| Label identifier type derived from @fnident@ type parameter of @AST.Stmts@.
  type TACFnIdent (AST.Stmts fnident ident BType) = fnident

  -- \| TAC translation result for a WACC @Stmts@ translation action, with @Jump@ continuation.
  type
    TACFnRepr (AST.Stmts fnident ident BType) =
      ( fnident
        -> TACM
            ident
            fnident
            (Jump ident fnident -> TACM ident fnident ())
      )

  -- \| Translates a WACC @Stmts@ AST Node to TAC.
  fnToTAC stmts = pure $ \l ->
    (fnToTAC' l . NE.toList . unwrap) stmts
    where
      fnToTAC'
        :: (Enum fnident, Enum ident, Eq ident, Ord fnident)
        => fnident
        -> [AST.Stmt fnident ident BType]
        -> TACM
            ident
            fnident
            (Jump ident fnident -> TACM ident fnident ())
      fnToTAC' kp (x : xs) =
        fnToTAC x >>= \case
          Nothing -> fnToTAC' kp xs
          Just (BlockTerminal j) -> pure $ \_ -> completeBlock j kp
          Just (Blocks f) -> do
            ts <- collectTACs
            fl <- freshLabel
            g <- fnToTAC' fl xs
            pure $ \j -> do
              fj <- f j
              appendBlock (BasicBlock (toList ts) fj) kp
              g j
      fnToTAC' kp [] = pure $ \j -> completeBlock j kp
