{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.WACC.X86.X86 (Instr, Prog, calleeSaved, callerSaved, args) where

import Data.Char
import Data.Data
import Data.List (intercalate)
import Data.Typeable ()
import Language.WACC.Error (quote)

data Label = I Int | CLib String
  deriving (Data)

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
  | -- | Int directives
    DirInt Int
  | -- | String directives (insert ascii binary at location)
    DirAsciz String
  | DirText
  | DirSection
  | DirRodata
  | DirGlobl Label
  | Comment String
  deriving (Typeable, Data)

data Operand = Imm Int | Reg Register | Mem Memory
  deriving (Data)

data Memory
  = -- | (53) :- immediate memory access
    MI Int
  | -- | (%rax) :- single register memory access
    MReg Register
  | -- | 7(%rax) = M[7 + R[rax]] :- offset single register memory access
    MRegI Int Register
  | -- | (%rsp, %rax) = M[R[rsp] + R[rax]] :- register sum memory access
    MTwoReg Register Register
  | -- | 7(%rsp, %rax) = M[7 + R[rsp] + R[rax]] :- offset register sum memory access
    MTwoRegI Int Register Register
  | -- | (%rsp, %rax, 4) = M[R[rsp] + R[rax]*4] :- register scaled (by 1,2,4 or 8) memory access
    MScale Register Register Int
  | -- | 7(%rsp, %rax, 4) = M[7 + R[rsp] + R[rax]*4] :- offset register scaled (by 1,2,4 or 8) memory access
    MScaleI Int Register Register Int
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
  | -- | callee saved
    Callee CalleeR
  | -- | caller saved
    Caller CallerR
  | -- | arguments from caller
    Arg ArgR
  deriving (Data)

data CalleeR = Rbx | Rbp | R12 | R13 | R14 | R15
  deriving (Show, Data)

data CallerR = R10 | R11
  deriving (Show, Data)

data ArgR = Rdi | Rsi | Rdx | Rcx | R8 | R9
  deriving (Show, Data)

calleeSaved :: [CalleeR]
calleeSaved = [Rbx, Rbp, R12, R13, R14, R15]

callerSaved :: [CallerR]
callerSaved = [R10, R11]

-- maybe add a list callerSaved ++ args,
-- as args are effectively caller saved

args :: [ArgR]
args = [Rdi, Rsi, Rdx, Rcx, R8, R9]

class ATNT a where
  formatA :: a -> String

instance ATNT Int where
  formatA = show

instance ATNT CalleeR where
  formatA = ('%' :) . map toLower . show

instance ATNT CallerR where
  formatA = ('%' :) . map toLower . show

instance ATNT ArgR where
  formatA = ('%' :) . map toLower . show

instance ATNT Register where
  formatA Rsp = "%rsp"
  formatA Rax = "%rax"
  formatA (Callee x) = formatA x
  formatA (Caller x) = formatA x
  formatA (Arg x) = formatA x

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

instance ATNT Operand where
  formatA (Imm x) = '$' : formatA x
  formatA (Reg r) = formatA r
  formatA (Mem m) = formatA m

{- |
use magic to get name of the constructor as a string and make it lower case
So constr Mov becomes instr mov
-}
instrName :: Instr -> String
instrName = ifDirective . (map toLower) . showConstr . toConstr
  where
    ifDirective str = case str of
      'd' : 'i' : 'r' : cs -> '.' : cs -- dealing with directives
      cs -> cs

instance ATNT Label where
  formatA (I x) = 'f' : formatA x
  formatA (CLib str) = str

instance ATNT Instr where
  formatA (Lab x) = 'f' : formatA x
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
  formatA i@(Comment str) = "# " ++ str
  formatA i@(DirInt x) = unwords [instrName i, formatA x]
  formatA i@(DirAsciz str) = unwords [instrName i, quote str]
  formatA i@(DirGlobl l) = unwords [instrName i, formatA l]
  formatA i = instrName i

instance ATNT [Instr] where
  formatA = unlines . map formatA
