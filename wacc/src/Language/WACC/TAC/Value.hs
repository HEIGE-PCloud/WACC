{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC @lvalue@s and @rvalue@s.
-}
module Language.WACC.TAC.Value (LVMode (..), lvToTAC) where

import Control.Monad (zipWithM_)
import Language.WACC.AST (LValue (..), PairElem (..), RValue (..), getAnn)
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr ()
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking

pairElemOffset :: PairElem ident a -> Int
pairElemOffset (FstElem _ _) = 0
pairElemOffset (SndElem _ _) = 8

pairElemLValue :: PairElem ident a -> LValue ident a
pairElemLValue (FstElem lv _) = lv
pairElemLValue (SndElem lv _) = lv

{- |
TAC translation modes for WACC @lvalue@s.
-}
data LVMode ident lident
  = -- | Load the @lvalue@ into the target variable.
    LVLoad
  | -- | Read into the @lvalue@ from standard input.
    LVRead
  | -- | Store an @rvalue@ into the @lvalue@.
    LVStore (RValue lident ident BType)

type instance TACIdent (LValue ident a) = ident

{- |
This instance takes an additional 'LVMode' parameter:

> type
>   TACRepr (LValue ident BType) lident =
>     LVMode ident lident -> TACM ident lident ()
-}
instance (Enum ident, Eq ident) => ToTAC (LValue ident BType) where
  type
    TACRepr (LValue ident BType) lident =
      LVMode ident lident -> TACM ident lident ()
  toTAC (LVIdent v t) = pure $ \case
    LVLoad -> do
      target <- getTarget
      move target (Var v)
    LVRead -> putTACs [Read (Var v) (flatten t)]
    LVStore rv -> fnToTAC rv `into` Var v
  toTAC (LVArrayElem ai _) = pure $ \case
    LVLoad -> toTAC ai >>= ($ Nothing)
    LVRead -> toTAC ai >>= ($ Just readBaseCase)
    LVStore rv -> toTAC ai >>= ($ Just (storeBaseCase rv))
    where
      readBaseCase v offset ft = do
        temp <- freshTemp
        putTACs [Read temp ft, Store v offset temp ft]
      storeBaseCase rv v offset ft = do
        temp <- tempWith (fnToTAC rv)
        putTACs [Store v offset temp ft]
  toTAC (LVPairElem pe t) = pure $ \case
    LVLoad -> toTAC pe
    LVRead -> do
      target <- loadLV
      offset <- loadOffset
      temp <- freshTemp
      putTACs [Read temp ft, Store target offset temp ft]
    LVStore rv -> do
      target <- loadLV
      offset <- loadOffset
      temp <- tempWith (fnToTAC rv)
      putTACs [Store target offset temp ft]
    where
      loadOffset = loadConst (pairElemOffset pe)
      loadLV = tempWith $ lvToTAC (pairElemLValue pe) LVLoad
      ft = flatten t

{- |
Translate an @lvalue@ in a single action.
-}
lvToTAC
  :: (Enum ident, Eq ident, Ord lident)
  => LValue ident BType
  -> LVMode ident lident
  -> TACM ident lident ()
lvToTAC lv mode = toTAC lv >>= ($ mode)

type instance TACIdent (PairElem ident a) = ident

instance (Enum ident, Eq ident) => ToTAC (PairElem ident BType) where
  type TACRepr (PairElem ident BType) lident = ()
  toTAC pe = do
    let
      lv = case pe of
        FstElem lv' _ -> lv'
        SndElem lv' _ -> lv'
    pair <- tempWith (lvToTAC lv LVLoad)
    target <- getTarget
    offset <- loadConst (pairElemOffset pe)
    putTACs [LoadM target pair offset (flatten $ getAnn pe)]

type instance TACIdent (RValue fnident ident a) = ident

instance
  (Enum ident, Eq ident, Ord fnident)
  => FnToTAC (RValue fnident ident BType)
  where
  type TACFnRepr (RValue fnident ident BType) = ()
  type TACFnIdent (RValue fnident ident BType) = fnident
  fnToTAC (RVExpr x _) = toTAC x
  fnToTAC (RVArrayLit xs (BArray t)) = do
    target <- getTarget
    let
      ft = flatten t
      elemCount = length xs
      elemSize = sizeOf ft
      evaluateAndStore x i = do
        temp <- tempWith (toTAC x)
        offset <- loadConst (elemSize * i + sizeOf FInt)
        putTACs [Store target offset temp ft]
    arraySize <- loadConst (elemSize * elemCount + sizeOf FInt)
    arrayLength <- loadConst elemCount
    lengthOffset <- loadConst 0
    putTACs
      [ Malloc target arraySize
      , Store target lengthOffset arrayLength FInt
      ]
    zipWithM_ evaluateAndStore xs [0 ..]
  fnToTAC (RVNewPair x y _) = do
    temp1 <- tempWith (toTAC x)
    temp2 <- tempWith (toTAC y)
    target <- getTarget
    fstOffset <- loadConst 0
    sndOffset <- loadConst 8
    pairSize <- loadConst 16
    putTACs
      [ Malloc target pairSize
      , Store target fstOffset temp1 (flatten $ getAnn x)
      , Store target sndOffset temp2 (flatten $ getAnn y)
      ]
  fnToTAC (RVPairElem pe _) = toTAC pe
  fnToTAC (RVCall f xs _) = do
    args <- mapM (tempWith . toTAC) xs
    target <- getTarget
    putTACs [Call target (Label f) args]
  fnToTAC _ = error "attempted to translate ill-typed array literal"
