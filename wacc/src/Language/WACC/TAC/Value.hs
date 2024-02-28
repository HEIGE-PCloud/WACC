{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC @lvalue@s and @rvalue@s.
-}
module Language.WACC.TAC.Value (LVMode (..)) where

import Control.Monad (zipWithM_)
import Language.WACC.AST (LValue (..), PairElem (..), RValue (..), getAnn)
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr ()
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking

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

instance (Enum ident) => ToTAC (LValue ident BType) where
  type
    TACRepr (LValue ident BType) lident =
      LVMode ident lident -> TACM ident lident ()
  toTAC (LVIdent v t) = pure $ \case
    -- TODO: copy value of source code variable into target TAC variable
    LVLoad -> pure ()
    LVRead -> putTACs [Read (Var v) (flatten t)]
    LVStore rv -> fnToTAC rv `into` Var v

type instance TACIdent (RValue fnident ident a) = ident

instance (Enum ident) => FnToTAC (RValue fnident ident BType) where
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
  fnToTAC (RVPairElem (FstElem lv t) _) = do
    tlv <- toTAC lv
    pair <- tempWith (tlv LVLoad)
    target <- getTarget
    offset <- loadConst 0
    putTACs [LoadM target pair offset (flatten t)]
  fnToTAC (RVPairElem (SndElem lv t) _) = do
    tlv <- toTAC lv
    pair <- tempWith (tlv LVLoad)
    target <- getTarget
    offset <- loadConst 8
    putTACs [LoadM target pair offset (flatten t)]
  fnToTAC (RVCall f xs _) = do
    args <- mapM (tempWith . toTAC) xs
    target <- getTarget
    putTACs [Call target (Label f) args]
  fnToTAC _ = error "attempted to translate ill-typed array literal"
