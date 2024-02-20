{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.WACC.X86.X86
  ( formatA
  , Instr (..)
  , Operand (..)
  , Register (..)
  , Label (..)
  , Directive (..)
  , Prog
  , callee
  , caller
  , args
  )
where

import Data.Char
import Data.Data
import Data.List (intercalate)
import Data.Typeable ()
import Language.WACC.Error (quote)

data Label = I Int | C CCall
  deriving (Data)

-- | TODO formatting for the underscores is not quite done. Also what's the @ stuff?
data CCall = Prints | Println | Printf | Fflush | Puts
  deriving (Show, Data)

type Prog = [Instr]

-- | cannot have two Mem operands for the same instruction
data Instr
  = Lab Int
  | Ret
  | Pushq Operand
  | Popq Operand
  | Movq Operand Operand
  | Leaq Operand Operand
  | Subq Operand Operand
  | Addq Operand Operand
  | Cmpq Operand Operand
  | Call Label
  | Je Label
  | Jmp Label
  | Dir Directive
  | Comment String
  deriving (Typeable, Data)

data Directive
  = DirInt Int
  | -- | String directives (insert ascii binary at location)
    DirAsciz String
  | DirText
  | DirSection
  | DirRodata
  | DirGlobl Label
  deriving (Typeable, Data)

data Operand = Imm Int | Reg Register | Mem Memory
  deriving (Data)

data Memory
  = -- | (53) :- immediate memory access
    MI Int
  | -- | (%rax) :- single register memory access
    MReg Register
  | -- | (%rsp, %rax) = M[R[rsp] + R[rax]] :- register sum memory access
    MTwoReg Register Register
  | -- | (%rsp, %rax, 4) = M[R[rsp] + R[rax]*4] :- register scaled (by 1,2,4 or 8) memory access
    MScale Register Register Int
  | -- | 7(%rax) = M[7 + R[rax]] :- offset single register memory access
    MRegI Int Register
  | -- | 7(%rsp, %rax) = M[7 + R[rsp] + R[rax]] :- offset register sum memory access
    MTwoRegI Int Register Register
  | -- | 7(%rsp, %rax, 4) = M[7 + R[rsp] + R[rax]*4] :- offset register scaled (by 1,2,4 or 8) memory access
    MScaleI Int Register Register Int
  | -- | f4(%rax) :- offset to label, single register memory access
    MRegL Label Register
  deriving (Data)

{- |
[source diagram for below](https://scientia.doc.ic.ac.uk/api/resources/11646/file/Lecture4_StackProcedures.pdf#page=17)
Each kind of register in x86-64 segregated below. Feel free to change in case a homogenous type is preferred
-}
data Register
  = -- | stack pointer
    Rsp
  | -- | return value
    Rax
  | -- | callee saved starts here
    Rbx
  | Rbp
  | R12
  | R13
  | R14
  | R15
  | -- | caller saved starts here
    R10
  | R11
  | -- | arguments from caller starts here
    Rdi
  | Rsi
  | Rdx
  | Rcx
  | R8
  | R9
  deriving (Show, Data)

callee :: [Register]
callee = [Rbx, Rbp, R12, R13, R14, R15]

caller :: [Register]
caller = [R10, R11]

args :: [Register]
args = [Rdi, Rsi, Rdx, Rcx, R8, R9]

class ATNT a where
  formatA :: a -> String

instance ATNT Int where
  formatA = show

instance ATNT Register where
  formatA r = '%' : (map toLower (show r))

paren :: String -> String
paren x = "(" ++ x ++ ")"

instance ATNT Memory where
  formatA (MI x) = paren (formatA x)
  formatA (MReg r) = paren (formatA r)
  formatA (MTwoReg r1 r2) = paren (intercalate ", " (map formatA [r1, r2]))
  formatA (MScale r1 r2 s) = paren (intercalate ", " (map formatA [r1, r2] ++ [formatA s]))
  formatA (MRegI x r) = formatA x ++ paren (formatA r)
  formatA (MTwoRegI x r1 r2) = formatA x ++ paren (intercalate ", " (map formatA [r1, r2]))
  formatA (MScaleI x r1 r2 s) = formatA x ++ paren (intercalate ", " (map formatA [r1, r2] ++ [formatA s]))
  formatA (MRegL l r) = formatA l ++ paren (formatA r)

instance ATNT Operand where
  formatA (Imm x) = '$' : formatA x
  formatA (Reg r) = formatA r
  formatA (Mem m) = formatA m

{- |
use magic to get name of the constructor as a string and make it lower case
So constr Mov becomes instr mov
-}
instrName :: Instr -> String
instrName = (map toLower) . showConstr . toConstr

dirName :: Directive -> String
dirName = ifDirective . (map toLower) . showConstr . toConstr
  where
    ifDirective str = case str of
      'd' : 'i' : 'r' : cs -> '.' : cs -- dealing with directives
      cs -> cs

instance ATNT CCall where
  formatA = map toLower . show

instance ATNT Label where
  formatA (I x) = 'f' : formatA x
  formatA (C c) = formatA c

instance ATNT Instr where
  formatA (Lab x) = 'f' : formatA x ++ ":"
  formatA Ret = "ret"
  formatA i@(Pushq op) = unwords [instrName i, formatA op]
  formatA i@(Popq op) = unwords [instrName i, formatA op]
  formatA i@(Movq op1 op2) = unwords [instrName i, formatA op1, formatA op2]
  formatA i@(Leaq op1 op2) = unwords [instrName i, formatA op1, formatA op2]
  formatA i@(Subq op1 op2) = unwords [instrName i, formatA op1, formatA op2]
  formatA i@(Addq op1 op2) = unwords [instrName i, formatA op1, formatA op2]
  formatA i@(Cmpq op1 op2) = unwords [instrName i, formatA op1, formatA op2]
  formatA i@(Call l) = unwords [instrName i, formatA l]
  formatA i@(Je l) = unwords [instrName i, formatA l]
  formatA i@(Jmp l) = unwords [instrName i, formatA l]
  formatA (Dir d) = formatA d
  formatA (Comment str) = "# " ++ str

instance ATNT Directive where
  formatA d@(DirInt x) = unwords [dirName d, formatA x]
  formatA d@(DirAsciz str) = unwords [dirName d, quote str]
  formatA d@(DirGlobl l) = unwords [dirName d, formatA l]
  formatA d = dirName d

instance ATNT [Instr] where
  formatA = unlines . map formatA