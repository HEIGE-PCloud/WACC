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
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
  ( TACM
  , appendBlock
  , collectTACs
  , completeBlock
  , freshLabel
  , freshTemp
  , into
  , loadConst
  , putTACs
  , tempWith
  )
import Language.WACC.TAC.TAC
  ( BasicBlock (..)
  , BinOp (..)
  , Jump (..)
  , TAC (..)
  , Var (..)
  )
import Language.WACC.TAC.Value
import Language.WACC.TypeChecking

type instance TACIdent (AST.Stmt fnident ident BType) = ident

type instance TACIdent (AST.Stmts fnident ident BType) = ident

{- |
TAC translation result for a WACC @Stmt@ translation action.
-}
data StmtTACs ident fnident
  = -- |
    -- Basic Block translation with result left open with @Jump@ for
    -- continuation.
    Blocks (Jump ident fnident -> TACM ident fnident (Jump ident fnident))
  | -- | Jump Translation for terminal instructions like @Ret@ and @Exit@.
    BlockTerminal (Jump ident fnident)

printArray
  :: (Enum ident, Ord lident)
  => Bool
  -> Var ident
  -> BType
  -> TACM ident lident ()
printArray addNewline array bt = do
  lengthShift <- loadConst (sizeOf FInt)
  dataBaseAddr <- freshTemp
  let
    printInstr
      | addNewline = PrintLn
      | otherwise = Print
    printType = case bt of
      BArray BChar -> FString
      _ -> flatten bt
  putTACs
    [ -- dataBaseAddr := array + lengthShift
      BinInstr dataBaseAddr array PtrAdd lengthShift
    , -- print(ln) dataBaseAddr
      printInstr dataBaseAddr printType
    ]

{- |
Defines instance of @FnToTAC@ for WACC @Stmt@s. This instance is used to
translate WACC @Stmt@s AST Nodes to TAC.
-}
instance
  (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => FnToTAC (AST.Stmt fnident ident BType)
  where
  -- TAC translation result for a WACC @Stmt@ translation action, with @Jump@
  -- continuation.
  type
    TACFnRepr (AST.Stmt fnident ident BType) =
      Maybe
        (StmtTACs ident fnident)

  -- Label identifier type derived from @fnident@ type parameter of @AST.Stmt@.
  type TACFnIdent (AST.Stmt fnident ident BType) = fnident

  -- Translates a WACC @Stmt@ AST Node to TAC.
  fnToTAC (AST.Skip _) = pure Nothing
  fnToTAC (AST.Decl _ v rv _) = Nothing <$ fnToTAC rv `into` Var v
  fnToTAC (AST.Asgn lv rv _) = Nothing <$ lvToTAC lv (LVStore rv)
  fnToTAC (AST.Read lv _) = Nothing <$ lvToTAC lv LVRead
  fnToTAC (AST.Free x _) = do
    ptr <- tempWith (toTAC x)
    putTACs
      [ -- free ptr
        Free ptr
      ]
    pure Nothing
  fnToTAC (AST.Return x _) = Just . BlockTerminal . Ret <$> tempWith (toTAC x)
  fnToTAC (AST.Print x _) = do
    temp <- tempWith (toTAC x)
    case getAnn x of
      t@BArray {} -> printArray False temp t
      t ->
        putTACs
          [ -- print temp
            Print temp (flatten t)
          ]
    pure Nothing
  fnToTAC (AST.PrintLn x _) = do
    temp <- tempWith (toTAC x)
    case getAnn x of
      t@BArray {} -> printArray True temp t
      t ->
        putTACs
          [ -- println temp
            PrintLn temp (flatten t)
          ]
    pure Nothing
  fnToTAC (AST.Exit x _) = Just . BlockTerminal . Exit <$> tempWith (toTAC x)
  fnToTAC (AST.IfElse condExpr ifBody elseBody _) = do
    cond <- tempWith (toTAC condExpr)
    ifLabel <- freshLabel
    elseLabel <- freshLabel
    pure $ Just $ Blocks $ \contJump -> do
      stmtsThen ifBody ifLabel contJump
      stmtsThen elseBody elseLabel contJump
      pure $ CJump cond ifLabel elseLabel
  fnToTAC (AST.While condExpr body _) = do
    condLabel <- freshLabel
    bodyLabel <- freshLabel
    pure $ Just $ Blocks $ \contJump -> do
      cond <- tempWith (toTAC condExpr)
      condJump <- case contJump of
        Jump elseLabel -> pure $ CJump cond bodyLabel elseLabel
        condJump@(CJump {}) -> do
          elseIfLabel <- freshLabel
          appendBlock (BasicBlock [] condJump) elseIfLabel
          pure $ CJump cond bodyLabel elseIfLabel
        _ -> pure contJump
      completeBlock condJump condLabel
      let
        jumpToCond = Jump condLabel
      stmtsThen body bodyLabel jumpToCond
      pure jumpToCond
  fnToTAC (AST.BeginEnd body _) = do
    bodyLabel <- freshLabel
    completeBody <- stmtsToTAC body bodyLabel
    pure $ Just $ Blocks $ \contJump -> do
      completeBody contJump
      pure $ Jump bodyLabel

{- |
Partially translate a sequence of statements from a starting label.

Returns an action which accepts the continuation jump.
-}
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
Fully translate a sequence of statements from a starting label and a
continuation jump in a single action.
-}
stmtsThen
  :: (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => AST.Stmts fnident ident BType
  -> fnident
  -> Jump ident fnident
  -> TACM ident fnident ()
stmtsThen stmts l j = stmtsToTAC stmts l >>= ($ j)

{- |
Defines instance of @FnToTAC@ for WACC @Stmts@s. This instance is used to
translate WACC @Stmts@s AST Nodes to TAC.
-}
instance
  (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => FnToTAC (AST.Stmts fnident ident BType)
  where
  -- Label identifier type derived from @fnident@ type parameter of @AST.Stmts@.
  type TACFnIdent (AST.Stmts fnident ident BType) = fnident

  -- TAC translation result for a WACC @Stmts@ translation action, with @Jump@
  -- continuation.
  type
    TACFnRepr (AST.Stmts fnident ident BType) =
      ( fnident
        -> TACM
            ident
            fnident
            (Jump ident fnident -> TACM ident fnident ())
      )

  -- Translates a WACC @Stmts@ AST Node to TAC.
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
      fnToTAC' curLabel (stmt : rest) =
        fnToTAC stmt >>= \case
          Nothing -> fnToTAC' curLabel rest
          Just (BlockTerminal terminalJump) ->
            pure $ const $ completeBlock terminalJump curLabel
          Just (Blocks completeBlocks) -> do
            curTACs <- collectTACs
            nextLabel <- freshLabel
            pure $ \curJump -> do
              contJump <- completeBlocks (Jump nextLabel)
              appendBlock (BasicBlock (toList curTACs) contJump) curLabel
              completeNext <- fnToTAC' nextLabel rest
              completeNext curJump
      fnToTAC' curLabel [] = pure $ \contJump -> completeBlock contJump curLabel
