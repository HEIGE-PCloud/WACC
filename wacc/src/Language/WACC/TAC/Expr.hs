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

unInstr
  :: (Enum ident)
  => UnOp
  -> Var ident
  -> TACM ident lident (ExprTACs ident lident)
unInstr op v = [[UnInstr t op v] t | t <- freshTemp]

binInstr
  :: (Enum ident)
  => Var ident
  -> BinOp
  -> Var ident
  -> TACM ident lident (ExprTACs ident lident)
binInstr v1 op v2 = [[BinInstr t v1 op v2] t | t <- freshTemp]

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
  toTAC (AST.Not x _) =
    [xts <> ts | xts <- toTAC x, ts <- unInstr Not (exprV xts)]
  toTAC (AST.Negate x _) =
    [xts <> ts | xts <- toTAC x, ts <- unInstr Neg (exprV xts)]
