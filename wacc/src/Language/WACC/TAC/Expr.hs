{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TAC translation actions for WACC expressions.
-}
module Language.WACC.TAC.Expr (aiToTAC) where

import Data.Maybe (fromMaybe)
import Language.WACC.AST
  ( ArrayIndex (..)
  , Expr (WAtom)
  , WAtom (..)
  )
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class (TACIdent, ToTAC (..))
import Language.WACC.TAC.FType (FType, flatten, sizeOf, pattern FInt)
import Language.WACC.TAC.State
  ( TACM
  , freshTemp
  , getTarget
  , into
  , loadConst
  , move
  , putTACs
  , tempWith
  )
import Language.WACC.TAC.TAC
  ( BinOp (..)
  , CheckBoundsReason (..)
  , Offset
  , TAC (..)
  , UnOp (..)
  , Var (..)
  )
import Language.WACC.TypeChecking (BType (..))
import Prelude hiding (GT, LT, length)

type instance TACIdent (ArrayIndex ident a) = ident

{- |
This instance allows a custom base case to be provided for the TAC instruction
generator:

> type
>   TACRepr (ArrayIndex ident BType) lident =
>     Maybe (Var ident -> Offset ident -> FType -> TACM ident lident ())
>     -> TACM ident lident ()

The custom base case is provided with a variable containing the innermost array
(the 'Var'), a variable containing the last calculated offset (the 'Offset'),
and the element 'FType'. The default base case loads the innermost element into
the target variable.

No side effects are performed until the nested action is executed with a custom
base case or 'Nothing'. See 'aiToTAC' for a simpler calling convention.
-}
instance (Enum ident, Eq ident) => ToTAC (ArrayIndex ident BType) where
  type
    TACRepr (ArrayIndex ident BType) lident =
      Maybe (Var ident -> Offset ident -> FType -> TACM ident lident ())
      -> TACM ident lident ()
  toTAC (ArrayIndex v xs t) = pure $ \mf -> do
    target <- getTarget
    lengthOffset <- loadConst 0
    lengthShift <- loadConst (sizeOf FInt)
    let
      -- Load a value of type ft starting at address (array + offset).
      loadOffset array offset ft = do
        localTarget <- getTarget
        putTACs
          [ -- localTarget := array[offset]
            LoadM localTarget array offset ft
          ]
      -- Evaluate the offset of the xth array element (sizeOf ft * x + 4) into a
      -- fresh temporary variable, which is then returned. Checks the index
      -- against the length of array.
      mkOffsetTACs array x ft = do
        scalar <- loadConst (sizeOf ft)
        index <- tempWith (toTAC x)
        length <- freshTemp
        scaledIndex <- freshTemp
        result <- freshTemp
        putTACs
          [ -- length := len array
            LoadM length array lengthOffset FInt
          , -- assert 0 <= index < length
            CheckBounds index length ArrayIndexCheck
          , -- scaledIndex := index * scalar
            BinInstr scaledIndex index Mul scalar
          , -- result := scaledIndex + lengthShift
            BinInstr result scaledIndex Add lengthShift
          ]
        pure result
      -- Base case (single indexing expression): use a custom base case if one
      -- is provided, otherwise use loadOffset.
      chainIndexTACs array [x] (BArray t') = do
        let
          ft = flatten t'
        offset <- mkOffsetTACs array x ft
        (fromMaybe loadOffset mf) array offset ft `into` target
      -- Inductive case: use loadOffset on the current indexing expression and
      -- recurse on the inner array.
      chainIndexTACs outerArray (x : xs') (BArray t') = do
        let
          ft = flatten t'
        offset <- mkOffsetTACs outerArray x ft
        innerArray <- tempWith (loadOffset outerArray offset ft)
        chainIndexTACs innerArray xs' t'
      -- Impossible case (no indexing expressions).
      chainIndexTACs _ _ _ = error "attempted to translate invalid ArrayIndex"
    chainIndexTACs (Var v) xs t

{- |
Translate an 'ArrayIndex' in a single action.
-}
aiToTAC
  :: (Enum ident, Eq ident, Ord lident)
  => ArrayIndex ident BType
  -> Maybe (Var ident -> Offset ident -> FType -> TACM ident lident ())
  -> TACM ident lident ()
aiToTAC ai mBaseCase = toTAC ai >>= ($ mBaseCase)

{- |
Load the index of a value of an enumerable type into the target register using
'LoadCI'.
-}
loadEnum :: (Enum a, Ord lident) => a -> TACM ident lident ()
loadEnum x = do
  target <- getTarget
  putTACs
    [ -- target := $(fromEnum x)
      LoadCI target (fromEnum x)
    ]

{- |
Apply a unary operator to an expression, storing the result in the target
variable.
-}
unOp
  :: (Enum ident, Eq ident, Ord lident)
  => UnOp
  -> Expr ident BType
  -> TACM ident lident ()
unOp op x = do
  arg <- tempWith (toTAC x)
  target <- getTarget
  putTACs
    [ -- target := op arg
      UnInstr target op arg
    ]

{- |
Apply a ternary 'TAC' constructor to the target variable and two expressions.
-}
binInstr
  :: (Enum ident, Eq ident, Ord lident)
  => Expr ident BType
  -> (Var ident -> Var ident -> Var ident -> TAC ident lident)
  -> Expr ident BType
  -> TACM ident lident ()
binInstr x instr y = do
  argX <- tempWith (toTAC x)
  argY <- tempWith (toTAC y)
  target <- getTarget
  putTACs [instr target argX argY]

{- |
Apply a binary operator to two expressions, storing the result in the target
variable.
-}
binOp
  :: (Enum ident, Eq ident, Ord lident)
  => Expr ident BType
  -> BinOp
  -> Expr ident BType
  -> TACM ident lident ()
binOp x op y = binInstr x mkBinInstr y
  where
    mkBinInstr target argX = BinInstr target argX op

type instance TACIdent (Expr ident a) = ident

{- |
The final result of an expression is stored in the target variable, which can be
set using 'into' and 'tempWith'.
-}
instance (Enum ident, Eq ident) => ToTAC (Expr ident BType) where
  type TACRepr (Expr ident BType) lident = ()

  -- Atomic expressions:
  toTAC (WAtom (IntLit x _) _) = loadEnum x
  toTAC (WAtom (BoolLit b _) _) = loadEnum b
  toTAC (WAtom (CharLit c _) _) = loadEnum c
  toTAC (WAtom (StringLit s _) _) = do
    target <- getTarget
    putTACs
      [ -- target := s
        LoadCS target s
      ]
  toTAC (WAtom (Null _) _) = loadEnum (0 :: Int)
  toTAC (WAtom (Ident v _) _) = do
    target <- getTarget
    move target (Var v)
  toTAC (WAtom (ArrayElem ai _) _) = aiToTAC ai Nothing
  -- Unary operators:
  toTAC (AST.Not x _) = unOp Not x
  toTAC (AST.Negate x _) = unOp Negate x
  toTAC (AST.Len x _) = do
    array <- tempWith (toTAC x)
    lengthOffset <- loadConst 0
    target <- getTarget
    putTACs
      [ -- target := array[lengthOffset]
        LoadM target array lengthOffset FInt
      ]
  toTAC (AST.Ord x _) = toTAC x
  toTAC (AST.Chr x _) = do
    target <- getTarget
    toTAC x
    validCharUpperBound <- loadConst 128
    putTACs
      [ -- assert 0 <= target < validCharUpperBound
        CheckBounds target validCharUpperBound ChrCheck
      ]
  -- Binary operators:
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
