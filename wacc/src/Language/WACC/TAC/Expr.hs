{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC expressions.
-}
module Language.WACC.TAC.Expr () where

import Data.Bool (bool)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Language.WACC.AST
  ( ArrayIndex (..)
  , Expr (WAtom)
  , WAtom (..)
  )
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking (BType (..))
import Prelude hiding (GT, LT)

type instance TACIdent (ArrayIndex ident a) = ident

{- |
This instance allows a custom base case to be provided for the TAC instruction
generator:

> type
>   TACRepr (ArrayIndex ident BType) lident =
>     Maybe (Var ident -> Offset ident -> FType -> TACM ident lident ())
>     -> TACM ident lident ()
-}
instance (Enum ident) => ToTAC (ArrayIndex ident BType) where
  type
    TACRepr (ArrayIndex ident BType) lident =
      Maybe (Var ident -> Offset ident -> FType -> TACM ident lident ())
      -> TACM ident lident ()
  toTAC (ArrayIndex v xs t) = pure $ \mf -> do
    target <- getTarget
    offset <- loadConst (sizeOf FInt)
    let
      loadOffset v' offset' t' = do
        target' <- getTarget
        putTACs [LoadM target' v' offset' t']
      mkOffsetTACs x ft = do
        scalar <- loadConst (sizeOf ft)
        index <- tempWith (toTAC x)
        temp1 <- freshTemp
        temp2 <- freshTemp
        putTACs
          [ BinInstr temp1 index Mul scalar
          , BinInstr temp2 temp1 Add offset
          ]
        pure temp2
      chainIndexTACs v' [x] (BArray t') = do
        let
          ft = flatten t'
        offset' <- mkOffsetTACs x ft
        (fromMaybe loadOffset mf) v' offset' ft `into` target
      chainIndexTACs v' (x : xs') (BArray t') = do
        let
          ft = flatten t'
        offset' <- mkOffsetTACs x ft
        temp <- tempWith (loadOffset v' offset' ft)
        chainIndexTACs temp xs' t'
      chainIndexTACs _ _ _ = error "attempted to translate invalid ArrayIndex"
    chainIndexTACs (Var v) xs t

{- |
Load an integer constant using 'LoadCI'.
-}
loadCI :: Int -> TACM ident lident ()
loadCI x = do
  t <- getTarget
  putTACs [LoadCI t x]

unOp :: (Enum ident) => UnOp -> Expr ident BType -> TACM ident lident ()
unOp op x = do
  temp <- tempWith (toTAC x)
  t <- getTarget
  putTACs [UnInstr t op temp]

binInstr
  :: (Enum ident)
  => Expr ident BType
  -> (Var ident -> Var ident -> Var ident -> TAC ident lident)
  -> Expr ident BType
  -> TACM ident lident ()
binInstr x instr y = do
  temp1 <- tempWith (toTAC x)
  temp2 <- tempWith (toTAC y)
  t <- getTarget
  putTACs [instr t temp1 temp2]

binOp
  :: (Enum ident)
  => Expr ident BType
  -> BinOp
  -> Expr ident BType
  -> TACM ident lident ()
binOp x op y = binInstr x (\t xv yv -> BinInstr t xv op yv) y

type instance TACIdent (Expr ident a) = ident

instance (Enum ident) => ToTAC (Expr ident BType) where
  type TACRepr (Expr ident BType) lident = ()
  toTAC (WAtom (IntLit x _) _) = loadCI $ fromEnum x
  toTAC (WAtom (BoolLit b _) _) = loadCI $ bool 0 1 b
  toTAC (WAtom (CharLit c _) _) = loadCI $ ord c
  toTAC (WAtom (StringLit s _) _) = do
    t <- getTarget
    putTACs [LoadCS t s]
  toTAC (WAtom (Null _) _) = loadCI 0
  -- TODO: copy value of source code variable into target TAC variable
  toTAC (WAtom (Ident _ _) _) = pure ()
  toTAC (WAtom (ArrayElem ai _) _) = toTAC ai >>= ($ Nothing)
  toTAC (AST.Not x _) = unOp Not x
  toTAC (AST.Negate x _) = unOp Negate x
  toTAC (AST.Len x _) = do
    array <- tempWith (toTAC x)
    offset <- loadConst 0
    target <- getTarget
    putTACs [LoadM target array offset FInt]
  toTAC (AST.Ord x _) = toTAC x
  toTAC (AST.Chr x _) = do
    target <- getTarget
    toTAC x
    putTACs [CheckBounds 0 target 127]
  toTAC (AST.Mul x y _) = binOp x Mul y
  toTAC (AST.Div x y _) = binOp x Div y
  toTAC (AST.Mod x y _) = binOp x Mod y
  toTAC (AST.Add x y _) = binOp x Add y
  toTAC (AST.Sub x y _) = binOp x Sub y
  toTAC (AST.GT x y _) = binOp x GT y
  toTAC (AST.GTE x y _) = binOp x GTE y
  toTAC (AST.LT x y _) = binOp x LT y
  toTAC (AST.LTE x y _) = binOp x LTE y
  toTAC (AST.Eq x y _) = binOp x Eq y
  toTAC (AST.Ineq x y _) = binOp x Ineq y
  toTAC (AST.And x y _) = binOp x And y
  toTAC (AST.Or x y _) = binOp x Or y
