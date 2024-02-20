{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.WACC.X86.TAC where

import Data.Map (Map)
import Language.WACC.AST.WType (WType)

data TAC ident lident
  = BinInstr (Var ident) (Var ident) BinOp (Var ident)
  | Eqr (Var ident) (Var ident) (Var ident)
  | InEqr (Var ident) (Var ident) (Var ident)
  | Eqv (Var ident) (Var ident) (Var ident)
  | InEqv (Var ident) (Var ident) (Var ident)
  | UnInstr (Var ident) UnOp (Var ident)
  | Store (Var ident) (Offset ident) (Var ident)
  | LoadCI (Var ident) Int (Var ident)
  | LoadCS (Var ident) String (Var ident)
  | LoadM (Var ident) (Var ident) (Offset ident)
  | Call (Var ident) (Label lident) [Var ident]
  | Print (Var ident) WType
  | PrintLn (Var ident) WType
  | Exit (Var ident)
  | Read (Var ident) WType
  | Malloc (Var ident) (Offset ident)
  | Free (Var ident)

data BinOp = Add | Sub | Mul | Div | Mod | And | Or | Lt | Gt | Le | Ge

data UnOp = Neg | Not

data Var ident = Temp ident | Var ident

type Offset = Var

newtype Label lident = Label lident

data Jump ident lident
  = Jump (Label lident)
  | CJump (Var ident) (Label lident) (Label lident)
  | Ret (Var ident)

newtype BlockLabel ident = BlockLabel ident

data Func ident lident
  = Func lident [Var ident] (Map ident (BasicBlock ident lident))

data BasicBlock ident lident = BasicBlock
  { block :: [TAC ident lident]
  , nextBlock :: Jump ident lident
  }

type TACProgram ident lident = Map lident (Func ident lident)

{-
int f() is
  int x = 1;
  int y = 2;
  return x + y
end

int g() is
  int x = 3;
  int y = 4;
  return x + y
end

int main() is
  int z = f();
  return z
end
-}
