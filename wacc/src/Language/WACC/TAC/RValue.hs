{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC @rvalue@s.
-}
module Language.WACC.TAC.RValue where

import Control.Monad (zipWithM_)
import Language.WACC.AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr ()
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking

type instance TACIdent (RValue fnident ident a) = ident

instance
  (Enum fnident, Enum ident)
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
  fnToTAC (RVCall f xs _) = do
    args <- mapM (tempWith . toTAC) xs
    target <- getTarget
    putTACs [Call target (Label f) args]
