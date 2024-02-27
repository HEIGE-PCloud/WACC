{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC @lvalue@s.
-}
module Language.WACC.TAC.LValue () where

import Language.WACC.AST (LValue (..), RValue)
import Language.WACC.TAC.Class
import Language.WACC.TAC.FType (flatten)
import Language.WACC.TAC.RValue ()
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking (BType)

type instance TACIdent (LValue ident a) = ident

instance (Enum ident) => ToTAC (LValue ident BType) where
  type
    TACRepr (LValue ident BType) lident =
      (Maybe (RValue lident ident BType) -> TACM ident lident ())
  toTAC (LVIdent v t) = pure $ \case
    Just rv -> fnToTAC rv `into` Var v
    Nothing -> putTACs [Read (Var v) (flatten t)]
