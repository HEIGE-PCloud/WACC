{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.WACC.X86.X86 where

import Data.Char (toLower)
import Data.Data (Data (toConstr), Typeable, showConstr)
import Data.List (intercalate)
import Data.Typeable ()
import Language.WACC.Error (quote)

data Label = I Integer | R Runtime | S String
  deriving (Data)

data Runtime
  = PrintI
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
  | ErrOverflow
  deriving (Typeable, Data, Show)

type Prog = [Instr]

-- | cannot have two Mem operands for the same instruction
data Instr
  = Lab Label
  | Ret
  | Pushq Operand
  | Popq Operand
  | Movq Operand Operand
  | Movl Operand Operand
  | Movb Operand Operand
  | Movslq Operand Operand
  | Movsbq Operand Operand
  | Leaq Operand Operand
  | Subq Operand Operand
  | Addq Operand Operand
  | Andq Operand Operand
  | Cmpq Operand Operand
  | Cmpl Operand Operand
  | Cmpb Operand Operand
  | Call Label
  | Je Label
  | Jne Label
  | Jmp Label
  | Dir Directive
  | Comment String
  deriving (Typeable, Data)

data Directive
  = DirInt Integer
  | -- | String directives (insert ascii binary at location)
    DirAsciz String
  | DirText
  | DirSection
  | DirRodata
  | DirGlobl Label
  deriving (Typeable, Data)

data Operand = Imm Integer | Reg Register | Mem Memory
  deriving (Data)

data Memory
  = -- | (53) :- immediate memory access
    MI Integer
  | -- | (%rax) :- single register memory access
    MReg Register
  | -- | (%rsp, %rax) = M[R[rsp] + R[rax]] :- register sum memory access
    MTwoReg Register Register
  | -- | (%rsp, %rax, 4) = M[R[rsp] + R[rax]*4] :- register scaled (by 1,2,4 or 8) memory access
    MScale Register Register Integer
  | -- | 7(%rax) = M[7 + R[rax]] :- offset single register memory access
    MRegI Integer Register
  | -- | 7(%rsp, %rax) = M[7 + R[rsp] + R[rax]] :- offset register sum memory access
    MTwoRegI Integer Register Register
  | -- | 7(%rsp, %rax, 4) = M[7 + R[rsp] + R[rax]*4] :- offset register scaled (by 1,2,4 or 8) memory access
    MScaleI Integer Register Register Integer
  | -- | f4(%rax) :- offset to label, single register memory access
    MRegL Label Register
  deriving (Data)

{- |
[source diagram for below](https://scientia.doc.ic.ac.uk/api/resources/11646/file/Lecture4_StackProcedures.pdf#page=17)
Each kind of register in x86-64 segregated below. Feel free to change in case a homogenous type is preferred
-}
data Register
  = Rax
  | Eax
  | Ax
  | Ah
  | Al
  | Rbx
  | Ebx
  | Bx
  | Bh
  | Bl
  | Rcx
  | Ecx
  | Cx
  | Ch
  | Cl
  | Rdx
  | Edx
  | Dx
  | Dh
  | Dl
  | Rsi
  | Esi
  | Si
  | Sil
  | Rdi
  | Edi
  | Di
  | Dil
  | Rbp
  | Ebp
  | Bp
  | Bpl
  | Rsp
  | Esp
  | Sp
  | Spl
  | R8
  | R8d
  | R8w
  | R8b
  | R9
  | R9d
  | R9w
  | R9b
  | R10
  | R10d
  | R10w
  | R10b
  | R11
  | R11d
  | R11w
  | R11b
  | R12
  | R12d
  | R12w
  | R12b
  | R13
  | R13d
  | R13w
  | R13b
  | R14
  | R14d
  | R14w
  | R14b
  | R15
  | R15d
  | R15w
  | R15b
  | Rip
  deriving (Show, Data)

callee :: [Register]
callee = [Rbx, Rbp, R12, R13, R14, R15]

caller :: [Register]
caller = [R10, R11]

args :: [Register]
args = [Rdi, Rsi, Rdx, Rcx, R8, R9]

class ATNT a where
  formatA :: a -> String

instance ATNT Integer where
  formatA = show

instance ATNT Register where
  formatA r = '%' : map toLower (show r)

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
instrName = map toLower . showConstr . toConstr

dirName :: Directive -> String
dirName = ifDirective . map toLower . showConstr . toConstr
  where
    ifDirective str = case str of
      'd' : 'i' : 'r' : cs -> '.' : cs -- dealing with directives
      cs -> cs

instance ATNT Label where
  formatA (I x) = 'f' : formatA x
  formatA (R x) = '_' : map toLower (show x)
  formatA (S x) = x

instance ATNT Instr where
  formatA (Lab x) = formatA x ++ ":"
  formatA Ret = "ret"
  formatA i@(Pushq op) = formatUnOp i op
  formatA i@(Popq op) = formatUnOp i op
  formatA i@(Movq op1 op2) = formatBinOp i op1 op2
  formatA i@(Movl op1 op2) = formatBinOp i op1 op2
  formatA i@(Movb op1 op2) = formatBinOp i op1 op2
  formatA i@(Movslq op1 op2) = formatBinOp i op1 op2
  formatA i@(Movsbq op1 op2) = formatBinOp i op1 op2
  formatA i@(Leaq op1 op2) = formatBinOp i op1 op2
  formatA i@(Subq op1 op2) = formatBinOp i op1 op2
  formatA i@(Addq op1 op2) = formatBinOp i op1 op2
  formatA i@(Andq op1 op2) = formatBinOp i op1 op2
  formatA i@(Cmpq op1 op2) = formatBinOp i op1 op2
  formatA i@(Cmpl op1 op2) = formatBinOp i op1 op2
  formatA i@(Cmpb op1 op2) = formatBinOp i op1 op2
  formatA i@(Call l) = formatUnOp i l
  formatA i@(Je l) = formatUnOp i l
  formatA i@(Jne l) = formatUnOp i l
  formatA i@(Jmp l) = formatUnOp i l
  formatA (Dir d) = formatA d
  formatA (Comment str) = "# " ++ str

formatUnOp :: (ATNT a) => Instr -> a -> String
formatUnOp i op = unwords [instrName i, formatA op]

formatBinOp :: (ATNT a, ATNT b) => Instr -> a -> b -> [Char]
formatBinOp i op1 op2 = instrName i ++ " " ++ formatA op1 ++ ", " ++ formatA op2

instance ATNT Directive where
  formatA d@(DirInt x) = unwords [dirName d, formatA x]
  formatA d@(DirAsciz str) = unwords [dirName d, quote str]
  formatA d@(DirGlobl l) = unwords [dirName d, formatA l]
  formatA d = dirName d

instance ATNT [Instr] where
  formatA = unlines . map formatA
