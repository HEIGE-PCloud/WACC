{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.WACC.X86.Operand where

import Language.WACC.X86.IntLit (IntLit)
import Language.WACC.X86.Memory (Memory)
import Language.WACC.X86.Register (Register)
import Language.WACC.X86.Size (Size (..))

data OpType = IM | RM | MM

type OperandQ = Operand Q

type OperandD = Operand D

type OperandW = Operand W

type OperandB = Operand B

type OperandQMM = OperandQ MM

type OperandQRM = OperandQ RM

type OperandQIM = OperandQ IM

type OperandDMM = OperandD MM

type OperandDRM = OperandD RM

type OperandDIM = OperandD IM

type OperandWMM = OperandW MM

type OperandWRM = OperandW RM

type OperandWIM = OperandW IM

type OperandBMM = OperandB MM

type OperandBRM = OperandB RM

type OperandBIM = OperandB IM

data Operand (size :: Size) (opType :: OpType) where
  Imm :: IntLit size -> Operand size IM
  Reg :: Register size -> Operand size RM
  Mem :: Memory -> Operand size MM

deriving instance Eq (Operand size opType)

deriving instance Ord (Operand size opType)

deriving instance Show (Operand size opType)
