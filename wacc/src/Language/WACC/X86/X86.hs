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
import Language.WACC.X86.IntLit (IntLit (..))
import Language.WACC.X86.Label (Label (..))
import Language.WACC.X86.Memory (Memory (..))
import Language.WACC.X86.Operand
import Language.WACC.X86.Register (Register (..))
import Language.WACC.X86.Runtime (Runtime (..))
import Language.WACC.X86.Size (EQ, LT, LTE, Size (..), SizeToNat)

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
  Js :: Label -> Instruction
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
  Movzbq :: Operand size1 type1 -> Operand size2 type2 -> Instruction
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

startEnd :: (Runtime, Runtime)
startEnd = (PrintI, Exit)

runtimeDeps :: Array Runtime (Set Runtime)
runtimeDeps = array startEnd [(r, deps r) | r <- range startEnd]
  where
    deps :: Runtime -> Set Runtime
    deps ErrOutOfMemory = insert ErrOutOfMemory (deps PrintS)
    deps ErrOutOfBounds = insert ErrOutOfBounds (deps PrintS)
    deps ErrOverflow = insert ErrOverflow (deps PrintS)
    deps ErrDivByZero = insert ErrDivByZero (deps PrintS)
    deps ErrNull = insert ErrNull (deps PrintS)
    deps Malloc = insert Malloc (deps ErrOutOfMemory)
    deps r = singleton r
