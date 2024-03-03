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

import Control.Monad (when, zipWithM_)
import Language.WACC.AST (LValue (..), PairElem (..), RValue (..), getAnn)
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr (aiToTAC)
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking
import Prelude hiding (fst, snd)

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
    LVRead ->
      putTACs
        [ -- v := read
          Read (Var v) (flatten t)
        ]
    LVStore rv -> fnToTAC rv `into` Var v
  toTAC (LVArrayElem ai _) =
    pure $
      aiToTAC ai . \case
        LVLoad -> Nothing
        LVRead -> Just readBaseCase
        LVStore rv -> Just (storeBaseCase rv)
    where
      -- Read a value from stdin to a temporary variable, then store its value
      -- into the array at the calculated offset.
      readBaseCase array offset ft = do
        temp <- freshTemp
        putTACs
          [ -- temp := read
            Read temp ft
          , -- array[offset] := temp
            Store array offset temp ft
          ]
      -- Translate the rvalue into a temporary variable, then store its value
      -- into the array at the calculated offset.
      storeBaseCase rv array offset ft = do
        temp <- tempWith (fnToTAC rv)
        putTACs
          [ -- array[offset] := temp
            Store array offset temp ft
          ]
  toTAC (LVPairElem pe t) = pure $ \case
    LVLoad -> toTAC pe
    LVRead -> do
      target <- loadLV
      offset <- loadOffset
      temp <- freshTemp
      putTACs
        [ -- temp := read
          Read temp ft
        , -- target[offset] := temp
          Store target offset temp ft
        ]
    LVStore rv -> do
      target <- loadLV
      offset <- loadOffset
      temp <- tempWith (fnToTAC rv)
      putTACs
        [ -- target[offset] := temp
          Store target offset temp ft
        ]
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
    pair <- tempWith (lvToTAC (pairElemLValue pe) LVLoad)
    target <- getTarget
    offset <- loadConst (pairElemOffset pe)
    putTACs
      [ -- target := pair[offset]
        LoadM target pair offset (flatten $ getAnn pe)
      ]

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
      arrayBytes = indexOffset elemCount
      lengthBytes = sizeOf FInt
      -- Allocate an extra null terminator byte for char arrays.
      isCharArray = t == BChar
      extraCharByte = fromEnum isCharArray
      -- Calculate the byte offset of an index.
      indexOffset i = lengthBytes + elemSize * i
      -- Evaluate x into a temporary variable and store the value into the array
      -- at index i.
      evaluateAndStore x i = do
        temp <- tempWith (toTAC x)
        offset <- loadConst (indexOffset i)
        putTACs
          [ -- target[offset] := x
            Store target offset temp ft
          ]
    arraySize <- loadConst (arrayBytes + extraCharByte)
    const0 <- loadConst 0
    arrayLength <- case xs of
      -- Reuse the temporary variable storing the constant 0.
      [] -> pure const0
      _ -> loadConst elemCount
    putTACs
      [ -- target := malloc arraySize
        Malloc target arraySize
      , -- target[0] := arrayLength
        Store target const0 arrayLength FInt
      ]
    when isCharArray $ do
      nullByteOffset <- loadConst arrayBytes
      putTACs
        [ -- target[nullByteOffset] := 0
          Store target nullByteOffset const0 FChar
        ]
    zipWithM_ evaluateAndStore xs [0 ..]
  fnToTAC (RVNewPair fstExpr sndExpr _) = do
    fst <- tempWith (toTAC fstExpr)
    snd <- tempWith (toTAC sndExpr)
    target <- getTarget
    fstOffset <- loadConst 0
    sndOffset <- loadConst maxSize
    pairSize <- loadConst (2 * maxSize)
    putTACs
      [ -- target := malloc pairSize
        Malloc target pairSize
      , -- target[fstOffset] := fst
        Store target fstOffset fst (flatten $ getAnn fstExpr)
      , -- target[sndOffset] := snd
        Store target sndOffset snd (flatten $ getAnn sndExpr)
      ]
  fnToTAC (RVPairElem pe _) = toTAC pe
  fnToTAC (RVCall f xs _) = do
    -- Evaluate arguments before the function call.
    args <- mapM (tempWith . toTAC) xs
    target <- getTarget
    putTACs
      [ -- target := call f(args...)
        Call target f args
      ]
  fnToTAC _ = error "attempted to translate ill-typed array literal"
