{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.WACC.X86.X86New where

import Data.Char (toLower)
import Data.Data (Data (toConstr), Typeable, showConstr)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import Data.List (intercalate)
import Data.Type.Bool (type Not, type (||))
import GHC.TypeError (ErrorMessage (ShowType, Text, (:<>:)))
import GHC.TypeLits (Nat, TypeError)
import Language.WACC.X86.TH (ATNT (..), genATNTInstruction)

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

type IntLitQ = IntLit Q

type IntLitD = IntLit D

type IntLitW = IntLit W

type IntLitB = IntLit B

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

regF :: Register size -> RegisterF
regF Rax = RaxF
regF Eax = RaxF
regF Ax = RaxF
regF Al = RaxF
regF Ah = RaxF
regF Rbx = RbxF
regF Ebx = RbxF
regF Bx = RbxF
regF Bl = RbxF
regF Bh = RbxF
regF Rcx = RcxF
regF Ecx = RcxF
regF Cx = RcxF
regF Cl = RcxF
regF Ch = RcxF
regF Rdx = RdxF
regF Edx = RdxF
regF Dx = RdxF
regF Dl = RdxF
regF Dh = RdxF
regF Rsi = RsiF
regF Esi = RsiF
regF Si = RsiF
regF Sil = RsiF
regF Rdi = RdiF
regF Edi = RdiF
regF Di = RdiF
regF Dil = RdiF
regF Rbp = RbpF
regF Ebp = RbpF
regF Bp = RbpF
regF Bpl = RbpF
regF Rsp = RspF
regF Esp = RspF
regF Sp = RspF
regF Spl = RspF
regF R8 = R8F
regF R8d = R8F
regF R8w = R8F
regF R8b = R8F
regF R9 = R9F
regF R9d = R9F
regF R9w = R9F
regF R9b = R9F
regF R10 = R10F
regF R10d = R10F
regF R10w = R10F
regF R10b = R10F
regF R11 = R11F
regF R11d = R11F
regF R11w = R11F
regF R11b = R11F
regF R12 = R12F
regF R12d = R12F
regF R12w = R12F
regF R12b = R12F
regF R13 = R13F
regF R13d = R13F
regF R13w = R13F
regF R13b = R13F
regF R14 = R14F
regF R14d = R14F
regF R14w = R14F
regF R14b = R14F
regF R15 = R15F
regF R15d = R15F
regF R15w = R15F
regF R15b = R15F

type family SizeToNat (a :: Size) :: Nat where
  SizeToNat Q = 64
  SizeToNat D = 32
  SizeToNat W = 16
  SizeToNat B = 8

type family LT (a :: Size) (b :: Size) :: Bool where
  LT B W = 'True
  LT B D = 'True
  LT B Q = 'True
  LT W D = 'True
  LT W Q = 'True
  LT D Q = 'True
  LT _ _ = 'False

type family EQ (a :: Size) (b :: Size) :: Bool where
  EQ Q Q = 'True
  EQ D D = 'True
  EQ W W = 'True
  EQ B B = 'True
  EQ _ _ = 'False

type family LTE (a :: Size) (b :: Size) :: Bool where
  LTE a b = a `LT` b || a `EQ` b

type family GT (a :: Size) (b :: Size) :: Bool where
  GT a b = Not (a `LTE` b)

type family GTE (a :: Size) (b :: Size) :: Bool where
  GTE a b = Not (a `LT` b)

type family If (a :: Bool) (b :: Constraint) (c :: Constraint) :: Constraint where
  If 'True a _ = a
  If 'False _ b = b

type family Unless (a :: Bool) (b :: Constraint) :: Constraint where
  Unless a b = If a () b

type family NotImm (a :: Mem) :: Constraint where
  NotImm 'IM = TypeError ('Text "Can not have an immediate operand here")
  NotImm _ = ()

type family ValidImm (s1 :: Size) (m1 :: Mem) (s2 :: Size) (m2 :: Mem) :: Constraint where
  ValidImm _ _ _ IM =
    TypeError ('Text "The second operand can not be an immediate value")
  ValidImm s1 IM s2 RM =
    Unless
      (s1 `LTE` s2)
      ( TypeError
          ( 'Text
              "The size of the immediate value must be smaller than or equal to the size of the register"
          )
      )
  ValidImm s1 _ s2 MM = ()
  ValidImm s1 MM s2 _ = ()
  ValidImm s1 _ s2 _ =
    Unless
      (s1 `EQ` s2)
      ( TypeError
          ( 'Text
              "The sizes of the registers must be equal, the first register has a size of "
              ':<>: 'ShowType s1
              ':<>: 'Text " while the second register has a size of "
              ':<>: 'ShowType s2
          )
      )

type family ValidSizeLT (s1 :: Size) (s2 :: Size) :: Constraint where
  ValidSizeLT s1 s2 =
    Unless
      (s1 `LT` s2)
      ( TypeError
          ( 'Text
              "The size of the first operand must be smaller than the size of the second operand, the first operand has a size of "
              ':<>: 'ShowType s1
              ':<>: 'Text " ("
              ':<>: 'ShowType (SizeToNat s1)
              ':<>: 'Text " bits) while the second operand has a size of "
              ':<>: 'ShowType s2
              ':<>: 'Text " ("
              ':<>: 'ShowType (SizeToNat s2)
              ':<>: 'Text " bits)"
          )
      )

type family ValidMem (a :: Mem) (b :: Mem) :: Constraint where
  ValidMem MM MM = TypeError ('Text "Can not have two memory operands")
  ValidMem IM IM = TypeError ('Text "Can not have two immediate operands")
  ValidMem _ IM =
    TypeError ('Text "The second operand can not be an immediate value")
  ValidMem _ _ = ()

data Directive
  = DirInt Integer
  | -- | String directives (insert ascii binary at location)
    DirAsciz String
  | DirText
  | DirSection
  | DirGlobl Label
  deriving (Typeable, Data, Show)

data Instruction where
  Comment :: String -> Instruction
  Lab :: Label -> Instruction
  Dir :: Directive -> Instruction
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
    :: (ValidMem mem1 mem2, ValidImm size1 mem1 size2 mem2)
    => Operand size1 mem1
    -> Operand size2 mem2
    -> Instruction
  Movzx
    :: (ValidSizeLT size1 size2, ValidMem mem1 mem2)
    => Operand size1 mem1
    -> Operand size2 mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovl
    :: (ValidMem mem1 mem2, NotImm mem1, NotImm mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovle
    :: (ValidMem mem1 mem2, NotImm mem1, NotImm mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovg
    :: (ValidMem mem1 mem2, NotImm mem1, NotImm mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovge
    :: (ValidMem mem1 mem2, NotImm mem1, NotImm mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovne
    :: (ValidMem mem1 mem2, NotImm mem1, NotImm mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmove
    :: (ValidMem mem1 mem2, NotImm mem1, NotImm mem2)
    => Operand size mem1
    -> Operand size mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/idiv
  Idivl :: Operand size mem -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addq
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandQ mem1
    -> OperandQ mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addl
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandD mem1
    -> OperandD mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addw
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandW mem1
    -> OperandW mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addb
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandB mem1
    -> OperandB mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subq
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandQ mem1
    -> OperandQ mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subl
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandD mem1
    -> OperandD mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subw
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandW mem1
    -> OperandW mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subb
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandB mem1
    -> OperandB mem2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/imul
  --   Yeah... Imul can take in one or two or three operands.
  --   I love x86
  Imulq
    :: (ValidMem mem1 mem2, NotImm mem2)
    => OperandQ mem1
    -> OperandQ mem2
    -> Instruction

-- TODO: You can't operate on 64-bit immediate values unless with mov #imm, reg
-- https://stackoverflow.com/questions/62771323/why-we-cant-move-a-64-bit-immediate-value-to-memory

data Memory where
  MI :: Integer -> Memory
  MReg :: RegisterQ -> Memory
  MTwoReg :: RegisterQ -> RegisterQ -> Memory
  MScale :: RegisterQ -> RegisterQ -> Integer -> Memory
  MRegI :: Integer -> RegisterQ -> Memory
  MTwoRegI :: Integer -> RegisterQ -> RegisterQ -> Memory
  MScaleI :: Integer -> RegisterQ -> RegisterQ -> Integer -> Memory
  MRegL :: Label -> RegisterQ -> Memory

data IntLit (size :: Size) where
  IntLitQ :: Int64 -> IntLitQ
  IntLitD :: Int32 -> IntLitD
  IntLitW :: Int16 -> IntLitW
  IntLitB :: Int8 -> IntLitB

deriving instance Show (IntLit size)

data Operand (size :: Size) (memory :: Mem) where
  Imm :: IntLit size -> Operand size IM
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

instance ATNT Integer where
  formatA :: Integer -> String
  formatA = show

instance ATNT String where
  formatA :: String -> String
  formatA = id

instance ATNT (IntLit size) where
  formatA :: IntLit size -> String
  formatA (IntLitQ i) = show i
  formatA (IntLitD i) = show i
  formatA (IntLitW i) = show i
  formatA (IntLitB i) = show i

instance ATNT (Register size) where
  formatA :: Register size -> String
  formatA r = '%' : (toLower <$> show r)

instance ATNT (Operand size mem) where
  formatA :: Operand size mem -> String
  formatA (Imm i) = '$' : formatA i
  formatA (Reg r) = formatA r
  formatA (Mem m) = formatA m

paren :: String -> String
paren x = "(" ++ x ++ ")"

instance ATNT Memory where
  formatA :: Memory -> String
  formatA (MI x) = paren (show x)
  formatA (MReg r) = paren (formatA r)
  formatA (MTwoReg r1 r2) = paren (intercalate ", " (map formatA [r1, r2]))
  formatA (MScale r1 r2 sc) = paren (intercalate ", " (map formatA [r1, r2] ++ [show sc]))
  formatA (MRegI x r) = show x ++ paren (formatA r)
  formatA (MTwoRegI x r1 r2) = show x ++ paren (intercalate ", " (map formatA [r1, r2]))
  formatA (MScaleI x r1 r2 sc) =
    show x
      ++ paren (intercalate ", " (map formatA [r1, r2] ++ [show sc]))
  formatA (MRegL l r) = formatA l ++ paren (formatA r)

instance ATNT Label where
  formatA :: Label -> String
  formatA (I i) = "L" ++ show i
  formatA (R r) = show r
  formatA (S s) = s

dirName :: Directive -> String
dirName = ifDirective . map toLower . showConstr . toConstr
  where
    ifDirective str = case str of
      'd' : 'i' : 'r' : cs -> '.' : cs -- dealing with directives
      cs -> cs

instance ATNT Directive where
  formatA d@(DirInt x) = unwords [dirName d, show x]
  formatA d@(DirAsciz str) = unwords [dirName d, show str]
  formatA d@(DirGlobl l) = unwords [dirName d, formatA l]
  formatA d@(DirSection) = unwords [dirName d, ".rodata"]
  formatA d = dirName d

$(genATNTInstruction ''Instruction)

-- $(inspectCode (genATNT ''Instruction))

type Program = [Instruction]

instr1 :: Instruction
instr1 = Mov (Reg Rax) (Reg Rax)

instr2 :: Instruction
instr2 = Mov (Mem (MTwoReg Rax Rax)) (Reg Rbx)

instr3 :: Instruction
instr3 = Mov (Imm (IntLitB 1)) (Reg Al)

instr4 :: Instruction
instr4 = Movzx (Reg Al) (Reg Rax)

instr5 :: Instruction
instr5 = Comment "this is a comment"

instr6 :: Instruction
instr6 = Lab (R ArrLoad1)
