{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC expressions.
-}
module Language.WACC.TAC.Expr (ExprTACs (..)) where

import Data.Bool (bool)
import Data.Char (ord)
import Data.DList (DList)
import GHC.IsList (IsList (..))
import Language.WACC.AST (ArrayIndex (..), Expr (WAtom), WAtom (..))
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking (BType)
import Prelude hiding (GT, LT)

{- |
Three-address code generated for expressions.
-}
data ExprTACs ident lident = ExprTACs
  { exprV :: Var ident
  -- ^ The variable storing the result of the expression.
  , exprTACs :: DList (TAC ident lident)
  -- ^ The generated instructions.
  }

{- |
This instance enables a concise syntax for 'ExprTACs':

> [tac1, tac2, ...] var === ExprTACs var [tac1, tac2, ...]
-}
instance IsList (Var ident -> ExprTACs ident lident) where
  type Item (Var ident -> ExprTACs ident lident) = TAC ident lident
  fromList ts v = ExprTACs v (fromList ts)
  toList = toList . exprTACs . ($ undefined)

{- |
The result is assumed to be stored in the rightmost variable.
-}
instance Semigroup (ExprTACs ident lident) where
  ExprTACs _ ts1 <> ExprTACs v ts2 = ExprTACs v (ts1 <> ts2)

loadCI :: (Enum ident) => Int -> TACM ident lident (ExprTACs ident lident)
loadCI x = [[LoadCI t x] t | t <- freshTemp]

unExpr
  :: (Enum ident, Enum lident)
  => UnOp
  -> Expr ident BType
  -> TACM ident lident (ExprTACs ident lident)
unExpr op x =
  [xts <> [UnInstr t op (exprV xts)] t | xts <- toTAC x, t <- freshTemp]

binExpr
  :: (Enum ident, Enum lident)
  => Expr ident BType
  -> BinOp
  -> Expr ident BType
  -> TACM ident lident (ExprTACs ident lident)
binExpr x op y =
  [ xts <> yts <> [BinInstr t (exprV xts) op (exprV yts)] t
  | xts <- toTAC x
  , yts <- toTAC y
  , t <- freshTemp
  ]

type instance TACIdent (Expr ident a) = ident

instance (Enum ident) => ToTAC (Expr ident BType) where
  type TACRepr (Expr ident BType) lident = ExprTACs ident lident
  toTAC (WAtom (IntLit x _) _) = loadCI $ fromEnum x
  toTAC (WAtom (BoolLit b _) _) = loadCI $ bool 0 1 b
  toTAC (WAtom (CharLit c _) _) = loadCI $ ord c
  toTAC (WAtom (StringLit s _) _) = [[LoadCS t s] t | t <- freshTemp]
  toTAC (WAtom (Null _) _) = loadCI 0
  toTAC (WAtom (Ident v _) _) = pure $ [] (Var v)
  toTAC (WAtom (ArrayElem (ArrayIndex _ _ _) _) _) = undefined
  toTAC (AST.Not x _) = unExpr Not x
  toTAC (AST.Negate x _) = unExpr Negate x
  toTAC (AST.Len x _) = undefined
  toTAC (AST.Ord x _) = toTAC x
  toTAC (AST.Chr x _) =
    [ xts <> [CheckBounds 0 xv 127] xv
    | xts <- toTAC x
    , let
        xv = exprV xts
    ]
  toTAC (AST.Mul x y _) = binExpr x Mul y
  toTAC (AST.Div x y _) = binExpr x Div y
  toTAC (AST.Mod x y _) = binExpr x Mod y
  toTAC (AST.Add x y _) = binExpr x Add y
  toTAC (AST.Sub x y _) = binExpr x Sub y
  toTAC (AST.GT x y _) = binExpr x GT y
  toTAC (AST.GTE x y _) = binExpr x GTE y
  toTAC (AST.LT x y _) = binExpr x LT y
  toTAC (AST.LTE x y _) = binExpr x LTE y
  toTAC (AST.Eq x y _) = undefined
  toTAC (AST.Ineq x y _) = undefined
  toTAC (AST.And x y _) = binExpr x And y
  toTAC (AST.Or x y _) = binExpr x Or y
