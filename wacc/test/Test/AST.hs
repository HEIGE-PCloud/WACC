{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Test.AST (TestAST (..)) where

import Data.Functor.Identity (Identity (Identity))
import GHC.Exts (IsString (fromString))
import qualified Language.WACC.AST as AST
import Prelude hiding (GT, LT)

newtype TestIdent = TestIdent String deriving (Eq, Show)

instance IsString TestIdent where
  fromString = TestIdent

newtype TestFnIdent = TestFnIdent String deriving (Eq, Show)

instance IsString TestFnIdent where
  fromString = TestFnIdent

type TestLValue = AST.LValue TestAST TestIdent

type TestRValue = AST.RValue TestFnIdent TestAST TestIdent

data TestAST
  = IntLit Int
  | BoolLit Bool
  | CharLit Char
  | StringLit String
  | Null
  | Ident TestIdent
  | ArrayElem (AST.ArrayElem TestAST TestIdent)
  | Parens TestAST
  | Not TestAST
  | Negate TestAST
  | Len TestAST
  | Ord TestAST
  | Chr TestAST
  | Mul TestAST TestAST
  | Div TestAST TestAST
  | Mod TestAST TestAST
  | Add TestAST TestAST
  | Sub TestAST TestAST
  | GT TestAST TestAST
  | GTE TestAST TestAST
  | LT TestAST TestAST
  | LTE TestAST TestAST
  | Eq TestAST TestAST
  | InEq TestAST TestAST
  | And TestAST TestAST
  | Or TestAST TestAST
  | Skip
  | Decl TestIdent TestRValue
  | Asgn TestLValue TestRValue
  | Read TestLValue
  | Free TestAST
  | Return TestAST
  | Exit TestAST
  | Print TestAST
  | PrintLn TestAST
  | IfElse TestAST TestAST TestAST
  | While TestAST TestAST
  | BeginEnd TestAST
  | Seq TestAST TestAST
  deriving (Eq, Show)

type instance AST.Ann cls TestAST (t :: k) = Identity

type instance AST.Ident cls TestAST = TestIdent

instance AST.Expr TestAST where
  intLit = Identity IntLit
  boolLit = Identity BoolLit
  charLit = Identity CharLit
  stringLit = Identity StringLit
  null = Identity Null
  ident = Identity Ident
  arrayElem = Identity ArrayElem
  parens = Identity Parens
  not = Identity Not
  negate = Identity Negate
  len = Identity Len
  ord = Identity Ord
  chr = Identity Chr
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
  type FnIdent TestAST = TestFnIdent
  type StmtExpr TestAST = TestAST
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
