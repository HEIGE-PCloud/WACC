module Language.WACC.X86.Runtime where

import Language.WACC.X86.X86

x86Examples :: [(Prog, String)]
x86Examples = [(println, "println"), (free, "free"), (malloc, "malloc")]

{-
.section .rodata
	.int 2
.L._printi_str0:
	.asciz "%d"
.text
_printi:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	movl %edi, %esi
	leaq .L._printi_str0(%rip), %rdi
	movb $0, %al
	call printf@plt
	movq $0, %rdi
	call fflush@plt
	movq %rbp, %rsp
	popq %rbp
	ret
-}
printi :: Prog
printi =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 2
  , Lab (S ".L._printi_str0")
  , Dir $ DirAsciz "%d"
  , Dir DirText
  , Lab (R PrintI)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Movl (Reg Edi) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._printi_str0") Rip)) (Reg Rdi)
  , Movb (Imm 0) (Reg Al)
  , Call (S "printf@plt")
  , Movq (Imm 0) (Reg Rdi)
  , Call (S "fflush@plt")
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
.section
.rodata
.int 0
.L._println_str0:
  .asciz ""
.text
_println:
  pushq %rbp
  movq %rsp, %rbp
  andq $-16, %rsp
  leaq .L._println_str0(%rip), %rdi
  call puts@plt
  movq $0, %rdi
  call fflush@plt
  movq %rbp, %rsp
  popq %rbp
  ret
-}
println :: Prog
println =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 0
  , Lab (S ".L._println_str0")
  , Dir $ DirAsciz ""
  , Dir DirText
  , Lab (R PrintLn)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._println_str0") Rip)) (Reg Rdi)
  , Call (S "puts@plt")
  , Movq (Imm 0) (Reg Rdi)
  , Call (S "fflush@plt")
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
_free:
  pushq %rbp
  movq %rsp, %rbp
  andq $-16, %rsp
  call free@plt
  movq %rbp, %rsp
  popq %rbp
  ret
-}
free :: Prog
free =
  [ Lab (R Free)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Call (S "free@plt")
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc@plt
	cmpq $0, %rax
	je _errOutOfMemory
	movq %rbp, %rsp
	popq %rbp
	ret
-}
malloc :: Prog
malloc =
  [ Lab (R Malloc)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Call (S "malloc@plt")
  , Cmpq (Imm 0) (Reg Rax)
  , Je (S "_errOutOfMemory")
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

pprog :: Prog -> IO ()
pprog prog = putStrLn $ formatA prog
