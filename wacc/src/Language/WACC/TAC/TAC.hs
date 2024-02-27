{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

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
  | -- | > <var> := <var>[<Offset>]
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
    Call (Var ident) (Label lident) [Var ident]
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
  | -- |  > exit <var>
    Exit (Var ident)
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
  deriving (Eq, Show)

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
data Var ident = Temp ident | Var ident deriving (Eq, Show, Ord)

-- | Offsets in TAC, either temporary or named in the source code.
type Offset = Var

-- | Labels in TAC for jumps and function calls to other basic blocks.
newtype Label lident = Label lident deriving (Eq, Show, Ord)

-- | Jump instructions in TAC Basic Blocks for control flow.
data Jump ident lident
  = -- |  > jmp <lident>
    Jump (Label lident)
  | -- |
    -- > cjmp <var> <lident> <lident>
    --
    -- Jump to the first label if @var@ is non-zero. Otherwise, jump to the
    -- second label.
    CJump (Var ident) (Label lident) (Label lident)
  | -- |
    -- > ret <var>
    --
    -- Return @var@ to the caller block and continue execution there.
    Ret (Var ident)
  deriving (Eq, Show)

-- | Block labels in TAC for basic blocks.
newtype BlockLabel ident = BlockLabel ident deriving (Eq, Show)

-- | Function representation in TAC.
data Func ident lident
  = Func
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
  deriving (Eq, Show)

-- | The top-level mapping of function identifiers to their corresponding blocks.
type TACProgram ident lident = Map lident (Func ident lident)
