{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.AST (TestAST (..)) where

import Data.Functor.Identity (Identity (Identity))
import GHC.Exts (IsString (fromString))
import qualified Language.WACC.AST as AST
import Prelude hiding (GT, LT)

newtype TestIdent p = TestIdent String deriving (Eq, Show)

instance IsString (TestIdent p) where
  fromString = TestIdent

newtype TestFnIdent p q = TestFnIdent String deriving (Eq, Show)

instance IsString (TestFnIdent p q) where
  fromString = TestFnIdent

type TestLValue p = AST.LValue TestAST TestIdent p

type TestRValue p = AST.RValue TestFnIdent TestAST TestIdent p

data TestAST (p :: k)
  = IntLit Int
  | BoolLit Bool
  | CharLit Char
  | StringLit String
  | Null
  | forall q. Ident (TestIdent q)
  | forall q. ArrayElem (AST.ArrayElem TestAST TestIdent q)
  | forall q. Parens (TestAST q)
  | forall q. Not (TestAST q)
  | forall q. Negate (TestAST q)
  | forall q. Len (TestAST q)
  | forall q. Ord (TestAST q)
  | forall q. Chr (TestAST q)
  | forall q r. Mul (TestAST q) (TestAST r)
  | forall q r. Div (TestAST q) (TestAST r)
  | forall q r. Mod (TestAST q) (TestAST r)
  | forall q r. Add (TestAST q) (TestAST r)
  | forall q r. Sub (TestAST q) (TestAST r)
  | forall q r. GT (TestAST q) (TestAST r)
  | forall q r. GTE (TestAST q) (TestAST r)
  | forall q r. LT (TestAST q) (TestAST r)
  | forall q r. LTE (TestAST q) (TestAST r)
  | forall q r. Eq (TestAST q) (TestAST r)
  | forall q r. InEq (TestAST q) (TestAST r)
  | forall q r. And (TestAST q) (TestAST r)
  | forall q r. Or (TestAST q) (TestAST r)
  | Skip
  | forall q r. Decl (TestIdent q) (TestRValue r)
  | forall q r. Asgn (TestLValue q) (TestRValue r)
  | forall q. Read (TestLValue q)
  | forall q. Free (TestAST q)
  | forall q. Return (TestAST q)
  | forall q. Exit (TestAST q)
  | forall q. Print (TestAST q)
  | forall q. PrintLn (TestAST q)
  | forall q r s. IfElse (TestAST q) (TestAST r) (TestAST s)
  | forall q r. While (TestAST q) (TestAST r)
  | forall q. BeginEnd (TestAST q)
  | forall q r. Seq (TestAST q) (TestAST r)

deriving instance Show (TestAST p)

instance Eq (TestAST p) where
  (==) = astEq
    where
      astEq :: TestAST p -> TestAST q -> Bool
      astEq (IntLit i1) (IntLit i2) = i1 == i2
      astEq (BoolLit b1) (BoolLit b2) = b1 == b2
      astEq (CharLit c1) (CharLit c2) = c1 == c2
      astEq (StringLit s1) (StringLit s2) = s1 == s2
      astEq Null Null = True
      astEq (Ident i1) (Ident i2) = idEq i1 i2
      astEq (ArrayElem (AST.Index1 i1 t1)) (ArrayElem (AST.Index1 i2 t2)) =
        idEq i1 i2 && astEq t1 t2
      astEq (Parens t1) (Parens t2) = astEq t1 t2
      astEq (Not t1) (Not t2) = astEq t1 t2
      astEq (Negate t1) (Negate t2) = astEq t1 t2
      astEq (Len t1) (Len t2) = astEq t1 t2
      astEq (Ord t1) (Ord t2) = astEq t1 t2
      astEq (Chr t1) (Chr t2) = astEq t1 t2
      astEq (Mul lt1 rt1) (Mul lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (Div lt1 rt1) (Div lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (Mod lt1 rt1) (Mod lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (Add lt1 rt1) (Add lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (Sub lt1 rt1) (Sub lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (GT lt1 rt1) (GT lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (GTE lt1 rt1) (GTE lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (LT lt1 rt1) (LT lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (LTE lt1 rt1) (LTE lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (Eq lt1 rt1) (Eq lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (InEq lt1 rt1) (InEq lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (And lt1 rt1) (And lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq (Or lt1 rt1) (Or lt2 rt2) = astEq lt1 lt2 && astEq rt1 rt2
      astEq Skip Skip = True
      astEq (Decl i1 rv1) (Decl i2 rv2) = idEq i1 i2 && rvEq rv1 rv2
      astEq (Asgn lv1 rv1) (Asgn lv2 rv2) = lvEq lv1 lv2 && rvEq rv1 rv2
      astEq _ _ = False

      aeEq
        :: AST.ArrayElem TestAST TestIdent t
        -> AST.ArrayElem TestAST TestIdent t'
        -> Bool
      aeEq (AST.Index1 i1 t1) (AST.Index1 i2 t2) = idEq i1 i2 && astEq t1 t2
      aeEq (AST.IndexN ae1 t1) (AST.IndexN ae2 t2) = astEq t1 t2 && aeEq ae1 ae2
      aeEq _ _ = False

      callEq
        :: AST.FnCall TestAST args ret -> AST.FnCall TestAST args' ret' -> Bool
      callEq AST.Call AST.Call = True
      callEq (AST.Arg c1 t1) (AST.Arg c2 t2) = astEq t1 t2 && callEq c1 c2
      callEq _ _ = False

      fnIdEq :: TestFnIdent p q -> TestFnIdent r s -> Bool
      fnIdEq (TestFnIdent s1) (TestFnIdent s2) = s1 == s2

      idEq :: TestIdent p -> TestIdent q -> Bool
      idEq (TestIdent s1) (TestIdent s2) = s1 == s2

      lvEq :: TestLValue p -> TestLValue q -> Bool
      lvEq (AST.LVIdent i1) (AST.LVIdent i2) = idEq i1 i2
      lvEq (AST.LVArrayElem ae1) (AST.LVArrayElem ae2) = aeEq ae1 ae2
      lvEq (AST.LVPairElem pe1) (AST.LVPairElem pe2) = peEq lvEq pe1 pe2
      lvEq _ _ = False

      peEq
        :: ( forall pt1 pt2
              . value TestAST TestIdent pt1
             -> value TestAST TestIdent pt2
             -> Bool
           )
        -> AST.PairElem value TestAST TestIdent t
        -> AST.PairElem value TestAST TestIdent t'
        -> Bool
      peEq vEq (AST.FstElem v1) (AST.FstElem v2) = vEq v1 v2
      peEq vEq (AST.SndElem v1) (AST.SndElem v2) = vEq v1 v2
      peEq _ _ _ = False

      rvEq :: TestRValue p -> TestRValue q -> Bool
      rvEq (AST.RVExpr t1) (AST.RVExpr t2) = astEq t1 t2
      rvEq (AST.RVArrayLit ts1) (AST.RVArrayLit ts2) =
        and $ zipWith astEq ts1 ts2
      rvEq (AST.RVNewPair lt1 rt1) (AST.RVNewPair lt2 rt2) =
        astEq lt1 lt2 && astEq rt1 rt2
      rvEq (AST.RVPairElem pe1) (AST.RVPairElem pe2) = peEq rvEq pe1 pe2
      rvEq (AST.RVCall f1 c1) (AST.RVCall f2 c2) = fnIdEq f1 f2 && callEq c1 c2
      rvEq _ _ = False

type instance AST.Ann cls TestAST = Identity

type instance AST.Ident cls TestAST = TestIdent

instance AST.AtomExpr TestAST where
  intLit = Identity IntLit
  boolLit = Identity BoolLit
  charLit = Identity CharLit
  stringLit = Identity StringLit
  null = Identity Null
  ident = Identity Ident
  arrayElem = Identity ArrayElem
  parens = Identity Parens

instance AST.UnExpr TestAST where
  not = Identity Not
  negate = Identity Negate
  len = Identity Len
  ord = Identity Ord
  chr = Identity Chr

instance AST.BinExpr TestAST where
  mul = Identity Mul
  div = Identity Div
  mod = Identity Mod
  add = Identity Add
  sub = Identity Sub
  gt = Identity GT
  gte = Identity GTE
  lt = Identity LT
  lte = Identity LTE
  eq = Identity Eq
  ineq = Identity InEq
  and = Identity And
  or = Identity Or

instance AST.Stmt TestAST where
  type FnIdent erasure TestAST = TestFnIdent
  type Expr erasure TestAST = TestAST
  skip = Identity Skip
  decl = Identity Decl
  asgn = Identity Asgn
  read = Identity Read
  free = Identity Free
  return = Identity Return
  exit = Identity Exit
  print = Identity Print
  println = Identity PrintLn
  ifElse = Identity IfElse
  while = Identity While
  beginEnd = Identity BeginEnd
  seq = Identity Seq
