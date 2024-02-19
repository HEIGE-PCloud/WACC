{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.WACC.X86.TAC where

import Data.Map (Map)
import Language.WACC.AST.WType (WType)

data TAC ident lident
  = BinInstr (Val ident) (Val ident) BinOp (Val ident)
  | UnInstr (Val ident) UnOp (Val ident)
  | Store (Var ident) (Offset ident) (Val ident)
  | LoadCI (Var ident) Int (Val ident)
  | LoadCS (Var ident) String (Val ident)
  | LoadM (Var ident) (Offset ident) (Val ident)
  | Call (Var ident) (Label lident) [Var ident]
  | Args [Var ident]
  | Print (Var ident) WType
  | PrintLn (Var ident) WType
  | Exit (Var ident)
  | Read (Var ident) WType

data BinOp = Add | Sub | Mul | Div | Mod | And | Or | Eq | Neq | Lt | Gt | Le | Ge

data UnOp = Neg | Not

data Var ident = Temp ident | Var ident

data Val ident = VarAddr (Var ident) | VarDeref (Var ident)

type Offset = Var

newtype Label lident = Label lident

data Jump ident lident
  = Jump (Label lident)
  | CJump (Val ident) (Label lident) (Label lident)
  | Ret (Val ident)

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
