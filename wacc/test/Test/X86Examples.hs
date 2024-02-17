module Test.X86Examples (x86Examples) where

import Language.WACC.X86.X86

writeReg :: Prog
writeReg = [Movq (Imm 4) (Reg Rax)]

-- first few lines of valid/IO/print/println.wacc
helloWorld :: Prog
helloWorld =
  [ DirGlobl (I 0)
  , DirSection
  , DirRodata
  , DirInt 12
  , Lab 1
  , DirAsciz "Hello World!"
  , DirText
  , Lab 0
  , Pushq (Reg $ Callee (calleeSaved !! 2))
  , Pushq (Reg $ Callee (calleeSaved !! 1))
  , Movq (Reg Rsp) (Reg $ Callee (calleeSaved !! 1))
  ]

x86Examples = [(writeReg, "writeReg"), (helloWorld, "helloWorld")]

-- 0	.globl main
-- 1	.section .rodata
-- 2	# length of .L.str0
-- 3		.int 12
-- 4	.L.str0:
-- 5		.asciz "Hello World!"
-- 6	.text
-- 7	main:
-- 8		pushq %rbp
-- 9		pushq %rbx
-- 10		movq %rsp, %rbp
-- 11		# Stack pointer unchanged, no stack allocated arguments
-- 12		leaq .L.str0(%rip), %rax
-- 13		pushq %rax
-- 14		popq %rax
-- 15		movq %rax, %rax
-- 16		movq %rax, %rdi
-- 17		# statement primitives do not return results (but will clobber r0/rax)
-- 18		call _prints
-- 19		call _println
-- 20		movq $0, %rax
-- 21		popq %rbx
-- 22		popq %rbp
-- 23		ret
