{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WACC.X86.X86 where

-- import Data.Ix hiding (Ix)
import Data.Array
import Data.Char (toLower)
import Data.Data (Data (toConstr), Typeable, showConstr)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import Data.List (intercalate)
import Data.Set (Set, insert, singleton)
import GHC.TypeError (ErrorMessage (ShowType, Text, (:<>:)))
import GHC.TypeLits (TypeError)
import Language.WACC.X86.ATNT (ATNT (..), genATNTInstruction)
import Language.WACC.X86.Size

data OpType = IM | RM | MM

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
  Rip :: RegisterQ

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
  RipF :: RegisterF

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
regF Rip = RipF

type family If (a :: Bool) (b :: Constraint) (c :: Constraint) :: Constraint where
  If 'True a _ = a
  If 'False _ b = b

type family Unless (a :: Bool) (b :: Constraint) :: Constraint where
  Unless a b = If a () b

type family NotMem (a :: OpType) :: Constraint where
  NotMem 'MM = TypeError ('Text "Can not have a memory operand here")
  NotMem _ = ()

type family NotImm (a :: OpType) :: Constraint where
  NotImm 'IM = TypeError ('Text "Can not have an immediate operand here")
  NotImm _ = ()

type family ValidImm (s1 :: Size) (m1 :: OpType) (s2 :: Size) (m2 :: OpType) :: Constraint where
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

type family ValidOpType (a :: OpType) (b :: OpType) :: Constraint where
  ValidOpType MM MM = TypeError ('Text "Can not have two memory operands")
  ValidOpType IM IM = TypeError ('Text "Can not have two immediate operands")
  ValidOpType _ IM =
    TypeError ('Text "The second operand can not be an immediate value")
  ValidOpType _ _ = ()

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
  Dir :: Directive -> Instruction
  Lab :: Label -> Instruction
  Je :: Label -> Instruction
  Jne :: Label -> Instruction
  Jl :: Label -> Instruction
  Jle :: Label -> Instruction
  Jg :: Label -> Instruction
  Jge :: Label -> Instruction
  Jo :: Label -> Instruction
  Jmp :: Label -> Instruction
  Call :: Label -> Instruction
  Ret :: Instruction
  Cltd :: Instruction
  Popq :: OperandQ type1 -> Instruction
  Popl :: OperandD type1 -> Instruction
  Pushq :: OperandQ type1 -> Instruction
  Pushl :: OperandD type1 -> Instruction
  Negl :: (NotImm type1) => OperandD type1 -> Instruction
  Mov
    :: (ValidOpType type1 type2, ValidImm size1 type1 size2 type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  Movzx
    :: (ValidSizeLT size1 size2, ValidOpType type1 type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/movsx:movsxd
  Movslq :: Operand size1 type1 -> Operand size2 type2 -> Instruction
  -- | https://www.felixcloutier.com/x86/movsx:movsxd
  Movsbq :: Operand size1 type1 -> Operand size2 type2 -> Instruction
  -- | https://www.felixcloutier.com/x86/movsx:movsxd
  Movzbl :: Operand size1 type1 -> Operand size2 type2 -> Instruction
  Cmovl
    :: (ValidOpType type1 type2, NotImm type1, NotImm type2)
    => Operand size type1
    -> Operand size type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovle
    :: (ValidOpType type1 type2, NotImm type1, NotImm type2)
    => Operand size type1
    -> Operand size type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovg
    :: (ValidOpType type1 type2, NotImm type1, NotImm type2)
    => Operand size type1
    -> Operand size type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovge
    :: (ValidOpType type1 type2, NotImm type1, NotImm type2)
    => Operand size type1
    -> Operand size type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovne
    :: (ValidOpType type1 type2, NotImm type1, NotImm type2)
    => Operand size type1
    -> Operand size type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmove
    :: (ValidOpType type1 type2, NotImm type1, NotImm type2)
    => Operand size type1
    -> Operand size type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/idiv
  Idivl :: Operand size mem -> Instruction
  -- | https://www.felixcloutier.com/x86/setcc
  Sete :: OperandB type1 -> Instruction
  Setne :: OperandB type1 -> Instruction
  Setl :: OperandB type1 -> Instruction
  Setle :: OperandB type1 -> Instruction
  Setg :: OperandB type1 -> Instruction
  Setge :: OperandB type1 -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addq
    :: (ValidOpType type1 type2, NotImm type2)
    => OperandQ type1
    -> OperandQ type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addl
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addw
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addb
    :: (ValidOpType type1 type2, NotImm type2)
    => OperandB type1
    -> OperandB type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subq
    :: (ValidOpType type1 type2, NotImm type2)
    => OperandQ type1
    -> OperandQ type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subl
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subw
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subb
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/imul
  --   Yeah... Imul can take in one or two or three operands.
  --   I love x86
  Imulq
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  Imull
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Andq
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Andl
    :: (ValidOpType type1 type2, NotImm type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  Cmpq
    :: OperandQ type1 -> OperandQ type2 -> Instruction
  Cmpl
    :: (ValidOpType type1 type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  Cmpw
    :: (ValidOpType type1 type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  Cmpb
    :: (ValidOpType type1 type2)
    => Operand size1 type1
    -> Operand size2 type2
    -> Instruction
  Movq
    :: OperandQ type1 -> OperandQ type2 -> Instruction
  Movl
    :: Operand size1 type1 -> Operand size2 type2 -> Instruction
  Movw
    :: Operand size1 type1 -> Operand size2 type2 -> Instruction
  Movb
    :: Operand size1 type1 -> Operand size2 type2 -> Instruction
  Leaq
    :: (ValidOpType type1 type2) => OperandQ type1 -> OperandQ type2 -> Instruction

-- TODO: You can't operate on 64-bit immediate values unless with mov #imm, reg
-- https://stackoverflow.com/questions/62771323/why-we-cant-move-a-64-bit-immediate-value-to-memory

deriving instance Show Instruction

data Memory where
  MI :: Integer -> Memory
  MReg :: RegisterQ -> Memory
  MTwoReg :: RegisterQ -> RegisterQ -> Memory
  MScale :: RegisterQ -> RegisterQ -> Integer -> Memory
  MRegI :: Integer -> RegisterQ -> Memory
  MTwoRegI :: Integer -> RegisterQ -> RegisterQ -> Memory
  MScaleI :: Integer -> RegisterQ -> RegisterQ -> Integer -> Memory
  MRegL :: Label -> RegisterQ -> Memory

deriving instance Eq Memory

deriving instance Ord Memory

deriving instance Show Memory

data IntLit (size :: Size) where
  IntLitQ :: Int64 -> IntLitQ
  IntLitD :: Int32 -> IntLitD
  IntLitW :: Int16 -> IntLitW
  IntLitB :: Int8 -> IntLitB

deriving instance Show (IntLit size)

deriving instance Eq (IntLit size)

deriving instance Ord (IntLit size)

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
  | ErrBadChar
  | Exit
  deriving (Ix, Eq, Ord, Typeable, Data, Show)

instance ATNT Int8 where
  formatA :: Int8 -> String
  formatA = show

instance ATNT Int16 where
  formatA :: Int16 -> String
  formatA = show

instance ATNT Int32 where
  formatA :: Int32 -> String
  formatA = show

instance ATNT Int64 where
  formatA :: Int64 -> String
  formatA = show

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
  formatA (R r) = '_' : map toLower (show r)
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
  formatA d@DirSection = unwords [dirName d, ".rodata"]
  formatA d = dirName d

$(genATNTInstruction ''Instruction)

instance ATNT Program where
  formatA :: Program -> String
  formatA = unlines . map formatA

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

startEnd :: (Runtime, Runtime)
startEnd = (ArrLoad1, Exit)

runtimeDeps :: Array Runtime (Set Runtime)
runtimeDeps = array startEnd [(r, deps r) | r <- range startEnd]
  where
    deps :: Runtime -> Set Runtime
    deps ArrLoad1 = insert ArrLoad1 (deps ErrOutOfBounds)
    deps ArrLoad4 = insert ArrLoad4 (deps ErrOutOfBounds)
    deps ArrLoad8 = insert ArrLoad8 (deps ErrOutOfBounds)
    deps ArrStore1 = insert ArrStore1 (deps ErrOutOfBounds)
    deps ArrStore4 = insert ArrStore4 (deps ErrOutOfBounds)
    deps ArrStore8 = insert ArrStore8 (deps ErrOutOfBounds)
    deps ErrOutOfMemory = insert ErrOutOfMemory (deps PrintS)
    deps ErrOutOfBounds = insert ErrOutOfBounds (deps PrintS)
    deps ErrOverflow = insert ErrOverflow (deps PrintS)
    deps ErrDivByZero = insert ErrDivByZero (deps PrintS)
    deps r = singleton r
