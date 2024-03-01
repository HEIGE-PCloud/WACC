{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Defines the three-address code (TAC) representation of the WACC language.
-}
module Language.WACC.TAC.TAC where

import Data.Map (Map)
import Language.WACC.TAC.FType (FType)

{- |
ADT representing the three-address code (TAC) instructions.
-}
data TAC ident lident
  = -- | > <var> := <var> <binop> <var>
    BinInstr (Var ident) (Var ident) BinOp (Var ident)
  | -- | > <var> := <unop> <var>
    UnInstr (Var ident) UnOp (Var ident)
  | -- | > <var>[<offset>] := <var>
    Store (Var ident) (Offset ident) (Var ident) FType
  | -- | > <var> := Int
    LoadCI (Var ident) Int
  | -- | > <var> := String
    LoadCS (Var ident) String
  | -- | > <var> := <var>[<Offset>]
    LoadM (Var ident) (Var ident) (Offset ident) FType
  | -- |
    -- > <var> := call <lident>([<var>])
    --
    -- where @lident@ is a function identifier
    Call (Var ident) lident [Var ident]
  | -- |
    -- > print <var>
    --
    -- where type annotates which print function to use
    Print (Var ident) FType
  | -- |
    -- > println <var>
    --
    -- where type annotates which println function to use
    PrintLn (Var ident) FType
  | -- |
    -- > <var> := read
    --
    -- where input is read from stdin and annotated with type
    Read (Var ident) FType
  | -- | > <var> := malloc <Offset>
    Malloc (Var ident) (Offset ident)
  | -- | > free <var>
    Free (Var ident)
  | -- |
    -- > assert <low> <= <var> <= <high>
    --
    -- Throw a runtime error if the assertion fails.
    CheckBounds Int (Var ident) Int
  | -- | > <var> := <var>
    Move (Var ident) (Var ident)
  deriving (Eq, Show, Functor)

-- | Binary operators in TAC
data BinOp
  = Mul
  | Div
  | Mod
  | Add
  | Sub
  | GT
  | GTE
  | LT
  | LTE
  | Eq
  | Ineq
  | And
  | Or
  deriving (Eq, Show)

-- | Unary operators in TAC
data UnOp = Not | Negate deriving (Eq, Show)

-- | Variables in TAC, either temporary or named in the source code.
data Var ident = Temp ident | Var ident deriving (Eq, Show, Ord, Functor)

-- | Offsets in TAC, either temporary or named in the source code.
type Offset = Var

-- | Jump instructions in TAC Basic Blocks for control flow.
data Jump ident lident
  = -- |  > jmp <lident>
    Jump lident
  | -- |
    -- > cjmp <var> <lident> <lident>
    --
    -- Jump to the first label if @var@ is non-zero. Otherwise, jump to the
    -- second label.
    CJump (Var ident) lident lident
  | -- |
    -- > ret <var>
    --
    -- Return @var@ to the caller block and continue execution there.
    Ret (Var ident)
  | -- |  > exit <var>
    Exit (Var ident)
  deriving (Eq, Show, Functor)

-- | Function representation in TAC.
data TACFunc ident lident
  = TACFunc
      lident
      -- ^ label of the function body
      [Var ident]
      -- ^ parameters of the functions
      (Map lident (BasicBlock ident lident))
      -- ^ Map of the related sub basic blocks in function body
  deriving (Eq, Show)

-- | A basic block consisting a sequence of TAC instructions and a jump to the next block
data BasicBlock ident lident = BasicBlock
  { block :: [TAC ident lident]
  , nextBlock :: Jump ident lident
  }
  deriving (Eq, Show, Functor)

-- | The top-level mapping of function identifiers to their corresponding blocks.
type TACProgram ident lident = Map lident (TACFunc ident lident)
