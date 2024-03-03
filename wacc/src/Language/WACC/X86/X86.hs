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

import Data.Array (Array, Ix (range), array)
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
  ( OpType (..)
  , Operand (..)
  , OperandB
  , OperandD
  , OperandQ
  )
import Language.WACC.X86.Register (Register (..))
import Language.WACC.X86.Runtime (Runtime (..))
import Language.WACC.X86.Size (EQ, GTE, LT, LTE, Size (..), SizeToNat)
import System.IO (hPutStr)

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
  ValidOpType _ _ = ()

data Directive
  = DirInt Integer
  | DirAsciz String
  | DirText
  | DirSection
  | DirGlobl Label
  deriving (Typeable, Data, Show)

type family ValidPop (s' :: Size) (s :: Size) (m :: OpType) :: Constraint where
  ValidPop _ B _ = TypeError ('Text "Can not pop a word value")
  ValidPop _ _ 'IM = TypeError ('Text "Can not pop an immediate value")
  ValidPop s' s' 'RM = ()
  ValidPop _ _ 'RM = TypeError ('Text "Incorrect register size")
  ValidPop s' s 'MM =
    Unless (s `GTE` s') (TypeError ('Text "Incorrect memory size"))

type family ValidPush (s' :: Size) (s :: Size) (m :: OpType) :: Constraint where
  ValidPush B B 'IM = ()
  ValidPush _ B _ = TypeError ('Text "Can not push a word value")
  ValidPush _ Q 'IM = TypeError ('Text "Can not push an 64bit immediate value")
  ValidPush s' s' 'IM = ()
  ValidPush s' s' 'RM = ()
  ValidPush _ _ 'RM = TypeError ('Text "Incorrect register size")
  ValidPush s' s 'MM =
    Unless (s `GTE` s') (TypeError ('Text "Incorrect memory size"))

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
  -- | https://www.felixcloutier.com/x86/pop
  Popq :: (ValidPop Q s t) => Operand s t -> Instruction
  Popl :: (ValidPop D s t) => Operand s t -> Instruction
  Popw :: (ValidPop W s t) => Operand s t -> Instruction
  Popb :: (ValidPop B s t) => Operand s t -> Instruction
  -- | https://www.felixcloutier.com/x86/push
  Pushq :: (ValidPush Q s t) => Operand s t -> Instruction
  Pushl :: (ValidPush D s t) => Operand s t -> Instruction
  Pushw :: (ValidPush W s t) => Operand s t -> Instruction
  Pushb :: (ValidPush B s t) => Operand s t -> Instruction
  Negl :: (NotImm t1) => OperandD t1 -> Instruction
  Mov
    :: (ValidOpType t1 t2, ValidImm s1 t1 s2 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Movzx
    :: (ValidSizeLT s1 s2, ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/movsx:movsxd
  Movslq
    :: (ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/movsx:movsxd
  Movsbq
    :: (ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/movsx:movsxd
  Movzbq
    :: (ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Cmovl
    :: (ValidOpType t1 t2, NotImm t1, NotImm t2)
    => Operand size t1
    -> Operand size t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovle
    :: (ValidOpType t1 t2, NotImm t1, NotImm t2)
    => Operand size t1
    -> Operand size t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovg
    :: (ValidOpType t1 t2, NotImm t1, NotImm t2)
    => Operand size t1
    -> Operand size t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovge
    :: (ValidOpType t1 t2, NotImm t1, NotImm t2)
    => Operand size t1
    -> Operand size t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmovne
    :: (ValidOpType t1 t2, NotImm t1, NotImm t2)
    => Operand size t1
    -> Operand size t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/cmovcc
  Cmove
    :: (ValidOpType t1 t2, NotImm t1, NotImm t2)
    => Operand size t1
    -> Operand size t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/idiv
  Idivl :: Operand size mem -> Instruction
  -- | https://www.felixcloutier.com/x86/setcc
  Sete :: OperandB t1 -> Instruction
  Setne :: OperandB t1 -> Instruction
  Setl :: OperandB t1 -> Instruction
  Setle :: OperandB t1 -> Instruction
  Setg :: OperandB t1 -> Instruction
  Setge :: OperandB t1 -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addq
    :: (ValidOpType t1 t2, NotImm t2)
    => OperandQ t1
    -> OperandQ t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addl
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addw
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Addb
    :: (ValidOpType t1 t2, NotImm t2)
    => OperandB t1
    -> OperandB t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subq
    :: (ValidOpType t1 t2, NotImm t2)
    => OperandQ t1
    -> OperandQ t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subl
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subw
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/sub
  Subb
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/imul
  --   Yeah... Imul can take in one or two or three operands.
  --   I love x86
  Imulq
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Imull
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Andq
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  -- | https://www.felixcloutier.com/x86/add
  Andl
    :: (ValidOpType t1 t2, NotImm t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Cmpq
    :: (ValidOpType t1 t2) => OperandQ t1 -> OperandQ t2 -> Instruction
  Cmpl
    :: (ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Cmpw
    :: (ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Cmpb
    :: (ValidOpType t1 t2)
    => Operand s1 t1
    -> Operand s2 t2
    -> Instruction
  Movq
    :: (ValidOpType t1 t2) => Operand s1 t1 -> Operand s2 t2 -> Instruction
  Movl
    :: (ValidOpType t1 t2) => Operand s1 t1 -> Operand s2 t2 -> Instruction
  Movw
    :: (ValidOpType t1 t2) => Operand s1 t1 -> Operand s2 t2 -> Instruction
  Movb
    :: (ValidOpType t1 t2) => Operand s1 t1 -> Operand s2 t2 -> Instruction
  Leaq
    :: (ValidOpType t1 t2) => Operand s1 t1 -> Operand s2 t2 -> Instruction

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
  streamA h = mapM_ streamInstr
    where
      streamInstr i = streamA h i *> hPutStr h "\n"

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
