{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.WACC.X86.X86
  ( Directive (..)
  , Instr
    ( Lab
    , Je
    , Jo
    , Jl
    , Jge
    , Jne
    , Jmp
    , Call
    , Cltd
    , Ret
    , Cmovl
    , Cmovge
    , Sete
    , Setne
    , Setl
    , Setle
    , Setg
    , Setge
    , Negl
    , Pushq
    , Popq
    , Movslq
    , Movsbq
    , Movzbl
    , Dir
    , Comment
    , Movq
    , Movl
    , Movb
    , Leaq
    , Subq
    , Subl
    , Addq
    , Addl
    , Imull
    , Idivl
    , Andq
    , Cmpq
    , Cmpl
    , Cmpb
    )
  , Label (..)
  , Memory (..)
  , Operand (..)
  , Prog
  , Register (..)
  , Runtime (..)
  , argRegs
  , callee
  , caller
  , ATNT (..)
  , ATNTs (..)
  , runtimeDeps
  )
where

import Data.Char (toLower)
import Data.Data (Data (toConstr), Typeable, showConstr)
import Data.List (intercalate)
import Data.Set (Set, fromList)
import Data.Typeable ()
import Language.WACC.Error (quote)

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

runtimeDeps :: Runtime -> Set Runtime
runtimeDeps r = fromList (deps r)
  where
    deps :: Runtime -> [Runtime]
    deps ArrLoad1 = ArrLoad1 : deps ErrOutOfBounds
    deps ArrLoad4 = ArrLoad4 : deps ErrOutOfBounds
    deps ArrLoad8 = ArrLoad8 : deps ErrOutOfBounds
    deps ArrStore1 = ArrStore1 : deps ErrOutOfBounds
    deps ArrStore4 = ArrStore4 : deps ErrOutOfBounds
    deps ArrStore8 = ArrStore8 : deps ErrOutOfBounds
    deps PrintI = [PrintI]
    deps PrintB = [PrintB]
    deps PrintC = [PrintC]
    deps PrintS = [PrintS]
    deps PrintP = [PrintP]
    deps PrintLn = [PrintLn]
    deps Free = [Free]
    deps Malloc = Malloc : deps ErrOutOfMemory
    deps ReadI = [ReadI]
    deps ReadC = [ReadC]
    deps ErrOutOfMemory = ErrOutOfMemory : deps PrintS
    deps ErrOutOfBounds = ErrOutOfBounds : deps PrintS
    deps ErrOverflow = ErrOverflow : deps PrintS
    deps ErrDivByZero = ErrDivByZero : deps PrintS
    deps Exit = [Exit]

type Prog = [Instr]

data Name
  = Mov
  | Lea
  | Sub
  | Add
  | And
  | Cmp
  | Imul
  deriving (Eq, Ord, Show, Typeable, Data)

data Size = Q | L | W | B
  deriving (Eq, Ord, Show, Typeable, Data)

-- | cannot have two Mem operands for the same instruction
data Instr
  = Lab Label
  | Je Label
  | Jo Label
  | Jl Label
  | Jge Label
  | Jne Label
  | Jmp Label
  | Call Label
  | Cltd
  | Ret
  | Cmovl Operand Operand
  | Cmovge Operand Operand
  | Idivl Operand
  | Sete Operand
  | Setne Operand
  | Setl Operand
  | Setle Operand
  | Setg Operand
  | Setge Operand
  | Negl Operand
  | Pushq Operand
  | Popq Operand
  | BinOp Name Size Operand Operand
  | Movslq Operand Operand
  | Movsbq Operand Operand
  | Movzbl Operand Operand
  | Dir Directive
  | Comment String
  deriving (Typeable, Data)

pattern Movq :: Operand -> Operand -> Instr
pattern Movq op1 op2 = BinOp Mov Q op1 op2

pattern Movl :: Operand -> Operand -> Instr
pattern Movl op1 op2 = BinOp Mov L op1 op2

pattern Movb :: Operand -> Operand -> Instr
pattern Movb op1 op2 = BinOp Mov B op1 op2

pattern Leaq :: Operand -> Operand -> Instr
pattern Leaq op1 op2 = BinOp Lea Q op1 op2

pattern Subq :: Operand -> Operand -> Instr
pattern Subq op1 op2 = BinOp Sub Q op1 op2

pattern Subl :: Operand -> Operand -> Instr
pattern Subl op1 op2 = BinOp Sub L op1 op2

pattern Addq :: Operand -> Operand -> Instr
pattern Addq op1 op2 = BinOp Add Q op1 op2

pattern Addl :: Operand -> Operand -> Instr
pattern Addl op1 op2 = BinOp Add L op1 op2

pattern Imull :: Operand -> Operand -> Instr
pattern Imull op1 op2 = BinOp Imul L op1 op2

pattern Andq :: Operand -> Operand -> Instr
pattern Andq op1 op2 = BinOp And Q op1 op2

pattern Cmpq :: Operand -> Operand -> Instr
pattern Cmpq op1 op2 = BinOp Cmp Q op1 op2

pattern Cmpl :: Operand -> Operand -> Instr
pattern Cmpl op1 op2 = BinOp Cmp L op1 op2

pattern Cmpb :: Operand -> Operand -> Instr
pattern Cmpb op1 op2 = BinOp Cmp B op1 op2

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
  deriving (Eq, Ord, Data, Show)

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
  deriving (Eq, Ord, Data, Show)

{- |
[source diagram for below](https://scientia.doc.ic.ac.uk/api/resources/11646/file/Lecture4_StackProcedures.pdf#page=17)
Each kind of register in x86-64 segregated below. Feel free to change in case a homogenous type is preferred
-}
data Register
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | Rbp
  | Rsp
  | Rip
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Ord, Show, Data)

callee :: [Register]
callee = [Rbx, Rbp, R12, R13, R14, R15]

caller :: [Register]
caller = [R10, R11]

argRegs :: [Register]
argRegs = [Rdi, Rsi, Rdx, Rcx, R8, R9]

class ATNT a where
  formatA :: a -> String

class ATNTs a where
  formatAs :: Size -> a -> String

instance ATNT Integer where
  formatA = show

{- | Formatting registers like Rax, Rbx, Rcx, etc
| Formatting registers like R8, R9, R10, etc
-}
data RegFormat = Pre String | Post String

partitionReg :: Register -> RegFormat
partitionReg r@R8 = Post (show r)
partitionReg r@R9 = Post (show r)
partitionReg r@R10 = Post (show r)
partitionReg r@R11 = Post (show r)
partitionReg r@R12 = Post (show r)
partitionReg r@R13 = Post (show r)
partitionReg r@R14 = Post (show r)
partitionReg r@R15 = Post (show r)
partitionReg r = let 'R' : cs = show r in Pre cs -- remove the 'R' at the start

instance ATNTs Register where
  formatAs Q r = '%' : map toLower (show r)
  formatAs L r =
    '%' : case partitionReg r of
      Post (str) -> str ++ "d"
      Pre (str) -> 'e' : str
  formatAs W r =
    '%' : case partitionReg r of
      Post (str) -> str ++ "w"
      Pre (str) -> str
  formatAs B r =
    '%' : case partitionReg r of
      Post (str) -> str ++ "b"
      Pre (str) -> lower str
    where
      lower "ax" = "al"
      lower "bx" = "bl"
      lower "cx" = "cl"
      lower "dx" = "dl"
      lower str = str ++ "l"

paren :: String -> String
paren x = "(" ++ x ++ ")"

instance ATNTs Memory where
  formatAs _ (MI x) = paren (formatA x)
  formatAs s (MReg r) = paren (formatAs s r)
  formatAs s (MTwoReg r1 r2) = paren (intercalate ", " (map (formatAs s) [r1, r2]))
  formatAs s (MScale r1 r2 sc) = paren (intercalate ", " (map (formatAs s) [r1, r2] ++ [formatA sc]))
  formatAs s (MRegI x r) = formatA x ++ paren (formatAs s r)
  formatAs s (MTwoRegI x r1 r2) = formatA x ++ paren (intercalate ", " (map (formatAs s) [r1, r2]))
  formatAs s (MScaleI x r1 r2 sc) =
    formatA x
      ++ paren (intercalate ", " (map (formatAs s) [r1, r2] ++ [formatA sc]))
  formatAs s (MRegL l r) = formatA l ++ paren (formatAs s r)

instance ATNTs Operand where
  formatAs _ (Imm x) = '$' : formatA x
  formatAs s (Reg r) = formatAs s r
  formatAs _ (Mem m) = formatAs Q m -- memory access always 64 bit

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
  formatA (BinOp name s op1 op2) = formatBinOp name s op1 op2
  formatA i@(Movslq op1 op2) = instrName i ++ " " ++ formatAs L op1 ++ ", " ++ formatAs Q op2
  formatA i@(Movsbq op1 op2) = instrName i ++ " " ++ formatAs B op1 ++ ", " ++ formatAs Q op2
  formatA i@(Movzbl op1 op2) = instrName i ++ " " ++ formatAs B op1 ++ ", " ++ formatAs L op2
  formatA i@(Cmovl op1 op2) = instrName i ++ " " ++ formatAs Q op1 ++ ", " ++ formatAs Q op2
  formatA i@(Cmovge op1 op2) = instrName i ++ " " ++ formatAs Q op1 ++ ", " ++ formatAs Q op2
  formatA i@(Idivl op) = formatUnOp i L op
  formatA i@(Sete l) = formatUnOp i B l
  formatA i@(Setne l) = formatUnOp i B l
  formatA i@(Setl l) = formatUnOp i B l
  formatA i@(Setle l) = formatUnOp i B l
  formatA i@(Setg l) = formatUnOp i B l
  formatA i@(Setge l) = formatUnOp i B l
  formatA i@(Negl l) = formatUnOp i B l
  formatA i@(Pushq op) = formatUnOp i Q op
  formatA i@(Popq op) = formatUnOp i Q op
  formatA i@(Call l) = formatLab i l
  formatA i@(Je l) = formatLab i l
  formatA i@(Jo l) = formatLab i l
  formatA i@(Jl l) = formatLab i l
  formatA i@(Jge l) = formatLab i l
  formatA i@(Jne l) = formatLab i l
  formatA i@(Jmp l) = formatLab i l
  formatA (Dir d) = formatA d
  formatA (Comment str) = "# " ++ str
  formatA Cltd = "cltd"
  formatA Ret = "ret"

formatUnOp :: Instr -> Size -> Operand -> String
formatUnOp i s op = unwords [instrName i, formatAs s op]

formatLab :: Instr -> Label -> String
formatLab i l = unwords [instrName i, formatA l]

formatBinOp :: (ATNTs a, ATNTs b) => Name -> Size -> a -> b -> [Char]
formatBinOp name s op1 op2 =
  map toLower (show name ++ show s)
    ++ " "
    ++ formatAs s op1
    ++ ", "
    ++ formatAs s op2

instance ATNT Directive where
  formatA d@(DirInt x) = unwords [dirName d, show x]
  formatA d@(DirAsciz str) = unwords [dirName d, quote str]
  formatA d@(DirGlobl l) = unwords [dirName d, formatA l]
  formatA d = dirName d

instance ATNT [Instr] where
  formatA instrs = unlines (map formatA instrs)
