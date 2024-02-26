{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC @rvalue@s.
-}
module Language.WACC.TAC.RValue where

import Language.WACC.AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr (ExprTACs (..))
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking

type instance TACIdent (RValue fnident ident a) = ident

instance
  (Enum fnident, Enum ident)
  => FnToTAC (RValue fnident ident BType)
  where
  type TACFnRepr (RValue fnident ident BType) = ExprTACs ident fnident
  type TACFnIdent (RValue fnident ident BType) = fnident
  fnToTAC (RVExpr x _) = toTAC x
