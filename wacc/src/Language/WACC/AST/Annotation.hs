{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Type class for extracting annotations from AST nodes.
-}
module Language.WACC.AST.Annotation (Annotated (..), HasPos (..)) where

import Language.WACC.AST.Expr (Expr (..))
import Language.WACC.AST.Stmt (LValue (..), RValue (..))
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT)

{- |
Annotated AST nodes.
-}
class Annotated a where
  -- | Annotation type.
  type Ann a

  -- | Get the annotation of a node.
  getAnn :: a -> Ann a

instance Annotated (Expr ann ident) where
  type Ann (Expr ann ident) = ann
  getAnn (WAtom _ x) = x
  getAnn (Not _ x) = x
  getAnn (Negate _ x) = x
  getAnn (Len _ x) = x
  getAnn (Ord _ x) = x
  getAnn (Chr _ x) = x
  getAnn (Mul _ _ x) = x
  getAnn (Div _ _ x) = x
  getAnn (Mod _ _ x) = x
  getAnn (Add _ _ x) = x
  getAnn (Sub _ _ x) = x
  getAnn (GT _ _ x) = x
  getAnn (GTE _ _ x) = x
  getAnn (LT _ _ x) = x
  getAnn (LTE _ _ x) = x
  getAnn (Eq _ _ x) = x
  getAnn (Ineq _ _ x) = x
  getAnn (And _ _ x) = x
  getAnn (Or _ _ x) = x

instance Annotated (LValue ann ident) where
  type Ann (LValue ann ident) = ann
  getAnn (LVIdent _ x) = x
  getAnn (LVArrayElem _ x) = x
  getAnn (LVPairElem _ x) = x

instance Annotated (RValue ann fnident ident) where
  type Ann (RValue ann fnident ident) = ann
  getAnn (RVExpr _ x) = x
  getAnn (RVArrayLit _ x) = x
  getAnn (RVNewPair _ _ x) = x
  getAnn (RVPairElem _ x) = x
  getAnn (RVCall _ _ x) = x

{- |
AST nodes with associated positions.
-}
class HasPos a where
  -- | Get the 'Pos' associated with a node.
  getPos :: a -> Pos

instance HasPos Pos where
  getPos = id

instance (Annotated a, Ann a ~ Pos) => HasPos a where
  getPos = getAnn
