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
.section .rodata
# length of .L._prints_str0
	.int 4
.L._prints_str0:
	.asciz "%.*s"
.text
_prints:
	pushq %rbp
	movq %rsp, %rbp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	movq %rdi, %rdx
	movl -4(%rdi), %esi
	leaq .L._prints_str0(%rip), %rdi
	# on x86, al represents the number of SIMD registers used as variadic arguments
	movb $0, %al
	call printf@plt
	movq $0, %rdi
	call fflush@plt
	movq %rbp, %rsp
	popq %rbp
	ret
-}
prints :: Prog
prints =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 4
  , Lab (S ".L._prints_str0")
  , Dir $ DirAsciz "%.*s"
  , Dir DirText
  , Lab (R PrintS)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Movq (Reg Rdi) (Reg Rdx)
  , Movl (Mem (MRegI (-4) Rdi)) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._prints_str0") Rip)) (Reg Rdi)
  , Movb (Imm 0) (Reg Al)
  , Call (S "printf@plt")
  , Movq (Imm 0) (Reg Rdi)
  , Call (S "fflush@plt")
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
.section .rodata
# length of .L._printb_str0
	.int 5
.L._printb_str0:
	.asciz "false"
# length of .L._printb_str1
	.int 4
.L._printb_str1:
	.asciz "true"
# length of .L._printb_str2
	.int 4
.L._printb_str2:
	.asciz "%.*s"
.text
_printb:
	pushq %rbp
	movq %rsp, %rbp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	cmpb $0, %dil
	jne .L_printb0
	leaq .L._printb_str0(%rip), %rdx
	jmp .L_printb1
.L_printb0:
	leaq .L._printb_str1(%rip), %rdx
.L_printb1:
	movl -4(%rdx), %esi
	leaq .L._printb_str2(%rip), %rdi
	# on x86, al represents the number of SIMD registers used as variadic arguments
	movb $0, %al
	call printf@plt
	movq $0, %rdi
	call fflush@plt
	movq %rbp, %rsp
	popq %rbp
	ret
-}
printb :: Prog
printb =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 5
  , Lab (S ".L._printb_str0")
  , Dir $ DirAsciz "false"
  , Dir $ DirInt 4
  , Lab (S ".L._printb_str1")
  , Dir $ DirAsciz "true"
  , Dir $ DirInt 4
  , Lab (S ".L._printb_str2")
  , Dir $ DirAsciz "%.*s"
  , Dir DirText
  , Lab (R PrintB)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Cmpb (Imm 0) (Reg Dil)
  , Jne (S ".L_printb0")
  , Leaq (Mem (MRegL (S ".L._printb_str0") Rip)) (Reg Rdx)
  , Jmp (S ".L_printb1")
  , Lab (S ".L_printb0")
  , Leaq (Mem (MRegL (S ".L._printb_str1") Rip)) (Reg Rdx)
  , Lab (S ".L_printb1")
  , Movl (Mem (MRegI (-4) Rdx)) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._printb_str2") Rip)) (Reg Rdi)
  , Movb (Imm 0) (Reg Al)
  , Call (S "printf@plt")
  , Movq (Imm 0) (Reg Rdi)
  , Call (S "fflush@plt")
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
.section .rodata
# length of .L._printc_str0
	.int 2
.L._printc_str0:
	.asciz "%c"
.text
_printc:
	pushq %rbp
	movq %rsp, %rbp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	movb %dil, %sil
	leaq .L._printc_str0(%rip), %rdi
	# on x86, al represents the number of SIMD registers used as variadic arguments
	movb $0, %al
	call printf@plt
	movq $0, %rdi
	call fflush@plt
	movq %rbp, %rsp
	popq %rbp
	ret
-}
printc :: Prog
printc =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 2
  , Lab (S ".L._printc_str0")
  , Dir $ DirAsciz "%c"
  , Dir DirText
  , Lab (R PrintC)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (-16)) (Reg Rsp)
  , Movb (Reg Dil) (Reg Sil)
  , Leaq (Mem (MRegL (S ".L._printc_str0") Rip)) (Reg Rdi)
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

{-
.section .rodata
# length of .L._errOutOfMemory_str0
	.int 27
.L._errOutOfMemory_str0:
	.asciz "fatal error: out of memory\n"
.text
_errOutOfMemory:
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	leaq .L._errOutOfMemory_str0(%rip), %rdi
	call _prints
	movb $-1, %dil
	call exit@plt
-}
errOutOfMemory :: Prog
errOutOfMemory =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 27
  , Lab (S ".L._errOutOfMemory_str0")
  , Dir $ DirAsciz "fatal error: out of memory\n"
  , Dir DirText
  , Lab (S "_errOutOfMemory")
  , Andq (Imm (-16)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOutOfMemory_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (-1)) (Reg Dil)
  , Call (S "exit@plt")
  ]

{-
.section .rodata
# length of .L._errOverflow_str0
	.int 52
.L._errOverflow_str0:
	.asciz "fatal error: integer overflow or underflow occurred\n"
.text
_errOverflow:
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	leaq .L._errOverflow_str0(%rip), %rdi
	call _prints
	movb $-1, %dil
	call exit@plt
-}
errOverflow :: Prog
errOverflow =
  [ Dir DirSection
  , Dir DirRodata
  , Dir $ DirInt 52
  , Lab (S ".L._errOverflow_str0")
  , Dir $ DirAsciz "fatal error: integer overflow or underflow occurred\n"
  , Dir DirText
  , Lab (S "_errOverflow")
  , Andq (Imm (-16)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOverflow_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (-1)) (Reg Dil)
  , Call (S "exit@plt")
  ]

-- | Print a program, useful for debugging in GHCi
printProg :: Prog -> IO ()
printProg prog = putStrLn $ formatA prog
