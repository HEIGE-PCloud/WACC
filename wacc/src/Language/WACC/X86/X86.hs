module Language.WACC.X86.X86 where

type Label = Int

-- cannot have two Mem operands for the same instruction
data Instr
  = Lab Label
  | Mov Operand Operand
  | Lea Operand Operand
  | Sub Operand Operand
  | Add Operand Operand
  | Cmp Operand Operand
  | Call Label
  | Je Label
  | Jmp Label

data Operand = Imm Int | Reg Register | Mem Memory

data Memory
  = MI Int
  | MReg Register
  | MRegI Int Register
  | MTwoReg Register Register
  | MTwoRegI Int Register Register
  | MScale Register Register Int
  | MScaleI Int Register Register Int
-- = One Register | Two Register Register | Three Register Register Int

-- https://scientia.doc.ic.ac.uk/api/resources/11646/file/Lecture4_StackProcedures.pdf#page=17

{- ^ a nice diagram of below
Callee (saved), Caller (saved) are being segregated below
-}

data Register = Rsp | Rax | Callee CalleeR | Caller CallerR | Arg ArgR

data CalleeR = Rbx | Rbp | R12 | R13 | R14 | R15

data CallerR = R10 | R11

data ArgR = Rdi | Rsi | Rdx | Rcx | R8 | R9

calleeSaved = [Rbx, Rbp, R12, R13, R14, R15]

callerSaved = [R10, R11]

args = [Rdi, Rsi, Rdx, Rcx, R8, R9]
