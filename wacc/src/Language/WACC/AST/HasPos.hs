{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Type class for extracting positions from AST nodes.
-}
module Language.WACC.AST.HasPos (HasPos (..)) where

import Language.WACC.AST.Expr (Expr (..))
import Language.WACC.AST.Stmt (RValue (..))
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT)

{- |
AST nodes with associated positions.
-}
class HasPos a where
  getPos :: a -> Pos

instance HasPos Pos where
  getPos = id

instance HasPos (Expr ident) where
  getPos (WAtom _ p) = p
  getPos (Not _ p) = p
  getPos (Negate _ p) = p
  getPos (Len _ p) = p
  getPos (Ord _ p) = p
  getPos (Chr _ p) = p
  getPos (Mul _ _ p) = p
  getPos (Div _ _ p) = p
  getPos (Mod _ _ p) = p
  getPos (Add _ _ p) = p
  getPos (Sub _ _ p) = p
  getPos (GT _ _ p) = p
  getPos (GTE _ _ p) = p
  getPos (LT _ _ p) = p
  getPos (LTE _ _ p) = p
  getPos (Eq _ _ p) = p
  getPos (Ineq _ _ p) = p
  getPos (And _ _ p) = p
  getPos (Or _ _ p) = p

instance HasPos (RValue fnident ident) where
  getPos (RVExpr _ p) = p
  getPos (RVArrayLit _ p) = p
  getPos (RVNewPair _ _ p) = p
  getPos (RVPairElem _ p) = p
  getPos (RVCall _ _ p) = p
