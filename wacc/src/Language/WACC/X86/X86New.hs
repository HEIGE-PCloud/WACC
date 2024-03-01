{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.WACC.X86.X86New where

import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import GHC.TypeError (ErrorMessage (Text))
import GHC.TypeLits (TypeError)

data Size
  = Q
  | D
  | W
  | B

data Mem = IM | RM | MM

type RegisterQ = Register Q

type RegisterD = Register D

type RegisterW = Register W

type RegisterB = Register B

type OperandQ = Operand Q

type OperandD = Operand D

type OperandW = Operand W

type OperandB = Operand B

data Register (size :: Size) where
  Rax :: RegisterQ
  Eax :: RegisterD
  Ax :: RegisterW
  Al :: RegisterB
  Ah :: RegisterB
  Rbx :: RegisterQ
  Ebx :: RegisterD
  Bx :: RegisterW
  Bl :: RegisterB
  Bh :: RegisterB
  Rcx :: RegisterQ
  Ecx :: RegisterD
  Cx :: RegisterW
  Cl :: RegisterB
  Ch :: RegisterB
  Rdx :: RegisterQ
  Edx :: RegisterD
  Dx :: RegisterW
  Dl :: RegisterB
  Dh :: RegisterB
  Rsi :: RegisterQ
  Esi :: RegisterD
  Si :: RegisterW
  Sil :: RegisterB
  Rdi :: RegisterQ
  Edi :: RegisterD
  Di :: RegisterW
  Dil :: RegisterB
  Rbp :: RegisterQ
  Ebp :: RegisterD
  Bp :: RegisterW
  Bpl :: RegisterB
  Rsp :: RegisterQ
  Esp :: RegisterD
  Sp :: RegisterW
  Spl :: RegisterB
  R8 :: RegisterQ
  R8d :: RegisterD
  R8w :: RegisterW
  R8b :: RegisterB
  R9 :: RegisterQ
  R9d :: RegisterD
  R9w :: RegisterW
  R9b :: RegisterB
  R10 :: RegisterQ
  R10d :: RegisterD
  R10w :: RegisterW
  R10b :: RegisterB
  R11 :: RegisterQ
  R11d :: RegisterD
  R11w :: RegisterW
  R11b :: RegisterB
  R12 :: RegisterQ
  R12d :: RegisterD
  R12w :: RegisterW
  R12b :: RegisterB
  R13 :: RegisterQ
  R13d :: RegisterD
  R13w :: RegisterW
  R13b :: RegisterB
  R14 :: RegisterQ
  R14d :: RegisterD
  R14w :: RegisterW
  R14b :: RegisterB
  R15 :: RegisterQ
  R15d :: RegisterD
  R15w :: RegisterW
  R15b :: RegisterB

deriving instance Show (Register size)

deriving instance Eq (Register size)

deriving instance Ord (Register size)

data RegisterF where
  RaxF :: RegisterF
  RbxF :: RegisterF
  RcxF :: RegisterF
  RdxF :: RegisterF
  RsiF :: RegisterF
  RdiF :: RegisterF
  RbpF :: RegisterF
  RspF :: RegisterF
  R8F :: RegisterF
  R9F :: RegisterF
  R10F :: RegisterF
  R11F :: RegisterF
  R12F :: RegisterF
  R13F :: RegisterF
  R14F :: RegisterF
  R15F :: RegisterF

deriving instance Show RegisterF

deriving instance Eq RegisterF

deriving instance Ord RegisterF

regFamily :: Register size -> RegisterF
regFamily Rax = RaxF
regFamily Eax = RaxF
regFamily Ax = RaxF
regFamily Al = RaxF
regFamily Ah = RaxF
regFamily Rbx = RbxF
regFamily Ebx = RbxF
regFamily Bx = RbxF
regFamily Bl = RbxF
regFamily Bh = RbxF
regFamily Rcx = RcxF
regFamily Ecx = RcxF
regFamily Cx = RcxF
regFamily Cl = RcxF
regFamily Ch = RcxF
regFamily Rdx = RdxF
regFamily Edx = RdxF
regFamily Dx = RdxF
regFamily Dl = RdxF
regFamily Dh = RdxF
regFamily Rsi = RsiF
regFamily Esi = RsiF
regFamily Si = RsiF
regFamily Sil = RsiF
regFamily Rdi = RdiF
regFamily Edi = RdiF
regFamily Di = RdiF
regFamily Dil = RdiF
regFamily Rbp = RbpF
regFamily Ebp = RbpF
regFamily Bp = RbpF
regFamily Bpl = RbpF
regFamily Rsp = RspF
regFamily Esp = RspF
regFamily Sp = RspF
regFamily Spl = RspF
regFamily R8 = R8F
regFamily R8d = R8F
regFamily R8w = R8F
regFamily R8b = R8F
regFamily R9 = R9F
regFamily R9d = R9F
regFamily R9w = R9F
regFamily R9b = R9F
regFamily R10 = R10F
regFamily R10d = R10F
regFamily R10w = R10F
regFamily R10b = R10F
regFamily R11 = R11F
regFamily R11d = R11F
regFamily R11w = R11F
regFamily R11b = R11F
regFamily R12 = R12F
regFamily R12d = R12F
regFamily R12w = R12F
regFamily R12b = R12F
regFamily R13 = R13F
regFamily R13d = R13F
regFamily R13w = R13F
regFamily R13b = R13F
regFamily R14 = R14F
regFamily R14d = R14F
regFamily R14w = R14F
regFamily R14b = R14F
regFamily R15 = R15F
regFamily R15d = R15F
regFamily R15w = R15F
regFamily R15b = R15F

type family SizeLT (a :: Size) (b :: Size) :: Constraint where
  SizeLT B W = ()
  SizeLT B D = ()
  SizeLT B Q = ()
  SizeLT W D = ()
  SizeLT W Q = ()
  SizeLT D Q = ()
  SizeLT _ _ =
    TypeError
      ( 'Text
          "The size of the first operand must be smaller than the size of the second operand"
      )

type family SizeEq (a :: Size) (b :: Size) :: Constraint where
  SizeEq Q Q = ()
  SizeEq D D = ()
  SizeEq W W = ()
  SizeEq B B = ()
  SizeEq _ _ = TypeError ('Text "The size of both operands must be equal")

type family SizeLTE (a :: Size) (b :: Size) :: Constraint where
  SizeLTE B W = ()
  SizeLTE B D = ()
  SizeLTE B Q = ()
  SizeLTE W D = ()
  SizeLTE W Q = ()
  SizeLTE D Q = ()
  SizeLTE Q Q = ()
  SizeLTE D D = ()
  SizeLTE W W = ()
  SizeLTE B B = ()
  SizeLTE _ _ =
    TypeError
      ( 'Text
          "The size of the first operand must be smaller than or equal to the size of the second operand"
      )

type family NotBothMemory (a :: Mem) (b :: Mem) :: Constraint where
  NotBothMemory 'MM 'MM = TypeError ('Text "Can not have two memory operands")
  NotBothMemory _ _ = ()

type family NotBothImmediate (a :: Mem) (b :: Mem) :: Constraint where
  NotBothImmediate 'IM 'IM =
    TypeError ('Text "Can not have two immediate operands")
  NotBothImmediate _ _ = ()

type family NotImmediate (a :: Mem) :: Constraint where
  NotImmediate 'IM = TypeError ('Text "Can not have an immediate operand here")
  NotImmediate _ = ()

type family ValidImm (s1 :: Size) (m1 :: Mem) (s2 :: Size) (m2 :: Mem) :: Constraint where
  ValidImm _ _ _ IM =
    TypeError ('Text "Can not have an immediate operand at the second argument")
  ValidImm s1 IM s2 RM = SizeLTE s1 s2
  ValidImm s1 _ s2 MM = ()
  ValidImm s1 MM s2 _ = ()
  ValidImm s1 _ s2 _ = SizeEq s1 s2

class (NotBothMemory a b, NotBothImmediate a b) => ValidMemory a b

instance (NotBothMemory a b, NotBothImmediate a b) => ValidMemory a b

data Instruction where
  Lab :: Label -> Instruction
  Je :: Label -> Instruction
  Jne :: Label -> Instruction
  Jl :: Label -> Instruction
  Jle :: Label -> Instruction
  Jg :: Label -> Instruction
  Jge :: Label -> Instruction
  Jmp :: Label -> Instruction
  Call :: Label -> Instruction
  Ret :: Instruction
  Cltd :: Instruction
  Mov
    :: (ValidMemory mem1 mem2, ValidImm size1 mem1 size2 mem2)
    => Operand size1 mem1
    -> Operand size2 mem2
    -> Instruction
  Movzx
    :: (SizeLT size1 size2, ValidMemory mem1 mem2)
    => Operand size1 mem1
    -> Operand size2 mem2
    -> Instruction
  Cmovl
    :: (ValidMemory mem1 mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  Cmovle
    :: (ValidMemory mem1 mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  Cmovg
    :: (ValidMemory mem1 mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  Cmovge
    :: (ValidMemory mem1 mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  Cmovne
    :: (ValidMemory mem1 mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  Cmove
    :: (ValidMemory mem1 mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  Idivl :: Operand size mem -> Instruction -- https://www.felixcloutier.com/x86/idiv
  Addq
    :: (NotBothMemory mem1 mem2, NotImmediate mem2)
    => OperandQ mem1
    -> OperandQ mem2
    -> Instruction

data Memory where
  MI :: Integer -> Memory
  MReg :: RegisterQ -> Memory
  MTwoReg :: RegisterQ -> RegisterQ -> Memory
  MScale :: RegisterQ -> RegisterQ -> Integer -> Memory
  MRegI :: Integer -> RegisterQ -> Memory
  MTwoRegI :: Integer -> RegisterQ -> RegisterQ -> Memory
  MScaleI :: Integer -> RegisterQ -> RegisterQ -> Integer -> Memory
  MRegL :: Label -> RegisterQ -> Memory

data IntLiteral (size :: Size) where
  IntLitQ :: Int64 -> IntLiteral Q
  IntLitD :: Int32 -> IntLiteral D
  IntLitW :: Int16 -> IntLiteral W
  IntLitB :: Int8 -> IntLiteral B

deriving instance Show (IntLiteral size)

data Operand (size :: Size) (memory :: Mem) where
  Imm :: IntLiteral size -> Operand size IM
  Reg :: Register size -> Operand size RM
  Mem :: Memory -> Operand size MM

data Label = I Integer | R Runtime | S String
  deriving (Eq, Ord, Data, Show)

data Runtime
  = ArrLoad1
  | ArrLoad4
  | ArrLoad8
  | ArrStore1
  | ArrStore4
  | ArrStore8
  | PrintI
  | PrintB
  | PrintC
  | PrintS
  | PrintP
  | PrintLn
  | Free
  | Malloc
  | ReadI
  | ReadC
  | ErrOutOfMemory
  | ErrOutOfBounds
  | ErrOverflow
  | ErrDivByZero
  | Exit
  deriving (Eq, Ord, Typeable, Data, Show)

class ATNT a where
  formatA :: a -> String

instance ATNT (IntLiteral size) where
  formatA :: IntLiteral size -> String
  formatA (IntLitQ i) = "$" ++ show i
  formatA (IntLitD i) = "$" ++ show i
  formatA (IntLitW i) = "$" ++ show i
  formatA (IntLitB i) = "$" ++ show i

instance ATNT (Register size) where
  formatA :: Register size -> String
  formatA r = '%' : (toLower <$> show r)

instance ATNT (Operand size mem) where
  formatA :: Operand size mem -> String
  formatA (Imm i) = '$' : formatA i
  formatA (Reg r) = formatA r
  formatA (Mem m) = formatA m

instance ATNT Memory where
  formatA :: Memory -> String
  formatA (MI i) = '$' : show i
  formatA (MReg r) = formatA r
  formatA (MTwoReg r1 r2) = formatA r1 ++ "," ++ formatA r2
  formatA (MScale r1 r2 i) = formatA r1 ++ "," ++ formatA r2 ++ "," ++ show i
  formatA (MRegI i r) = show i ++ "(" ++ formatA r ++ ")"
  formatA (MTwoRegI i r1 r2) = show i ++ "(" ++ formatA r1 ++ "," ++ formatA r2 ++ ")"
  formatA (MScaleI i r1 r2 j) = show i ++ "(" ++ formatA r1 ++ "," ++ formatA r2 ++ "," ++ show j ++ ")"
  formatA (MRegL l r) = formatA l ++ "(" ++ formatA r ++ ")"

instance ATNT Label where
  formatA :: Label -> String
  formatA (I i) = "L" ++ show i
  formatA (R r) = show r
  formatA (S s) = s

instance ATNT Instruction where
  formatA :: Instruction -> String
  formatA (Lab l) = formatA l ++ ":"
  formatA (Je l) = "je " ++ formatA l
  formatA (Jne l) = "jne " ++ formatA l
  formatA (Addq o1 o2) = "addq " ++ formatA o1 ++ "," ++ formatA o2

-- ...

type Program = [Instruction]

instr1 :: Instruction
instr1 = Mov (Reg Rax) (Reg Rax)

instr2 :: Instruction
instr2 = Mov (Mem (MTwoReg Rax Rax)) (Reg Rbx)

instr3 :: Instruction
instr3 = Mov (Imm (IntLitB 1)) (Reg Al)
