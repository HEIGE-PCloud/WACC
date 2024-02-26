{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC @rvalue@s.
-}
module Language.WACC.TAC.RValue where

import Data.Semigroup (sconcat)
import Language.WACC.AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr (ExprTACs (..), loadCI)
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
  fnToTAC (RVNewPair x y _) =
    [ sconcat
      [ xts
      , yts
      , c0ts
      , c8ts
      , c16ts
      , [ Malloc t (exprV c16ts)
        , Store t (exprV c0ts) (exprV xts) undefined
        , Store t (exprV c8ts) (exprV yts) undefined
        ]
          t
      ]
    | xts <- toTAC x
    , yts <- toTAC y
    , c0ts <- loadCI 0
    , c8ts <- loadCI 8
    , c16ts <- loadCI 16
    , t <- freshTemp
    ]
