{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Type class for extracting annotations from AST nodes.
-}
module Language.WACC.AST.Annotation (Annotated (..), HasPos (..)) where

import Language.WACC.AST.Expr
import Language.WACC.AST.Prog
import Language.WACC.AST.Stmt
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

instance Annotated (ArrayIndex ident ann) where
  type Ann (ArrayIndex ident ann) = ann
  getAnn (ArrayIndex _ _ x) = x

instance Annotated (WAtom ident ann) where
  type Ann (WAtom ident ann) = ann
  getAnn (IntLit _ x) = x
  getAnn (BoolLit _ x) = x
  getAnn (CharLit _ x) = x
  getAnn (StringLit _ x) = x
  getAnn (Null x) = x
  getAnn (Ident _ x) = x
  getAnn (ArrayElem _ x) = x

instance Annotated (Expr ident ann) where
  type Ann (Expr ident ann) = ann
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

instance Annotated (PairElem ident ann) where
  type Ann (PairElem ident ann) = ann
  getAnn (FstElem _ x) = x
  getAnn (SndElem _ x) = x

instance Annotated (LValue ident ann) where
  type Ann (LValue ident ann) = ann
  getAnn (LVIdent _ x) = x
  getAnn (LVArrayElem _ x) = x
  getAnn (LVPairElem _ x) = x

instance Annotated (RValue fnident ident ann) where
  type Ann (RValue fnident ident ann) = ann
  getAnn (RVExpr _ x) = x
  getAnn (RVArrayLit _ x) = x
  getAnn (RVNewPair _ _ x) = x
  getAnn (RVPairElem _ x) = x
  getAnn (RVCall _ _ x) = x

instance Annotated (Stmt fnident ident ann) where
  type Ann (Stmt fnident ident ann) = ann
  getAnn (Skip x) = x
  getAnn (Decl _ _ _ x) = x
  getAnn (Asgn _ _ x) = x
  getAnn (Read _ x) = x
  getAnn (Free _ x) = x
  getAnn (Return _ x) = x
  getAnn (Exit _ x) = x
  getAnn (Print _ x) = x
  getAnn (PrintLn _ x) = x
  getAnn (IfElse _ _ _ x) = x
  getAnn (While _ _ x) = x
  getAnn (BeginEnd _ x) = x

instance Annotated (Prog typ fnident ident ann) where
  type Ann (Prog typ fnident ident ann) = ann
  getAnn (Main _ _ x) = x

instance Annotated (Func typ fnident ident ann) where
  type Ann (Func typ fnident ident ann) = ann
  getAnn (Func _ _ _ _ x) = x

{- |
This instance is needed instead of @'HasPos' 'Pos'@ to avoid overlapping
instances.
-}
instance Annotated Pos where
  type Ann Pos = Pos
  getAnn = id

{- |
AST nodes with associated positions.
-}
class HasPos a where
  -- | Get the 'Pos' associated with a node.
  getPos :: a -> Pos

instance (Annotated a, Ann a ~ Pos) => HasPos a where
  getPos = getAnn
