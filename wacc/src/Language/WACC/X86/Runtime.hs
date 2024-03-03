module Language.WACC.X86.Runtime where

import qualified Data.DList as D
import qualified Data.Map as M
import Language.WACC.X86.ATNT (formatA)
import Language.WACC.X86.X86

x86Examples :: [(Program, String)]
x86Examples =
  [ (printi, "printi")
  , (printb, "printb")
  , (printc, "printc")
  , (printp, "printp")
  , (prints, "prints")
  , (println, "println")
  , (free, "free")
  , (malloc, "malloc")
  , (readi, "readi")
  , (readc, "readc")
  , (errOutOfMemory, "errOutOfMemory")
  , (errOutOfBounds, "errOutOfBounds")
  , (errOverflow, "errOverflow")
  , (errDivByZero, "errDivByZero")
  , (exit, "exit")
  ]

runtimeLib :: M.Map Runtime (D.DList Instruction)
runtimeLib =
  M.fromList
    [ (PrintI, D.fromList printi)
    , (PrintB, D.fromList printb)
    , (PrintC, D.fromList printc)
    , (PrintS, D.fromList prints)
    , (PrintP, D.fromList printp)
    , (PrintLn, D.fromList println)
    , (Free, D.fromList free)
    , (Malloc, D.fromList malloc)
    , (ReadI, D.fromList readi)
    , (ReadC, D.fromList readc)
    , (ErrOutOfMemory, D.fromList errOutOfMemory)
    , (ErrOutOfBounds, D.fromList errOutOfBounds)
    , (ErrOverflow, D.fromList errOverflow)
    , (ErrDivByZero, D.fromList errDivByZero)
    , (ErrNull, D.fromList errNull)
    , (Exit, D.fromList exit)
    ]

cprintf :: Label
cprintf = S "printf@plt"

cfflush :: Label
cfflush = S "fflush@plt"

cputs :: Label
cputs = S "puts@plt"

cexit :: Label
cexit = S "exit@plt"

cmalloc :: Label
cmalloc = S "malloc@plt"

cfree :: Label
cfree = S "free@plt"

cscanf :: Label
cscanf = S "scanf@plt"

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
printi :: Program
printi =
  [ Dir DirSection
  , Dir $ DirInt 2
  , Lab (S ".L._printi_str0")
  , Dir $ DirAsciz "%d"
  , Dir DirText
  , Lab (R PrintI)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Movl (Reg Edi) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._printi_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cprintf
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
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
prints :: Program
prints =
  [ Dir DirSection
  , Dir $ DirInt 4
  , Lab (S ".L._prints_str0")
  , Dir $ DirAsciz "%.*s"
  , Dir DirText
  , Lab (R PrintS)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Movq (Reg Rdi) (Reg Rdx)
  , Movl (Mem (MRegI (-4) Rdi)) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._prints_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cprintf
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
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
printb :: Program
printb =
  [ Dir DirSection
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
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Cmpb (Imm (IntLitB 0)) (Reg Dil)
  , Jne (S ".L_printb0")
  , Leaq (Mem (MRegL (S ".L._printb_str0") Rip)) (Reg Rdx)
  , Jmp (S ".L_printb1")
  , Lab (S ".L_printb0")
  , Leaq (Mem (MRegL (S ".L._printb_str1") Rip)) (Reg Rdx)
  , Lab (S ".L_printb1")
  , Movl (Mem (MRegI (-4) Rdx)) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._printb_str2") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cprintf
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
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
printc :: Program
printc =
  [ Dir DirSection
  , Dir $ DirInt 2
  , Lab (S ".L._printc_str0")
  , Dir $ DirAsciz "%c"
  , Dir DirText
  , Lab (R PrintC)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Movb (Reg Dil) (Reg Sil)
  , Leaq (Mem (MRegL (S ".L._printc_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cprintf
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
.section .rodata
# length of .L._printp_str0
	.int 2
.L._printp_str0:
	.asciz "%p"
.text
_printp:
	pushq %rbp
	movq %rsp, %rbp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	movq %rdi, %rsi
	leaq .L._printp_str0(%rip), %rdi
	# on x86, al represents the number of SIMD registers used as variadic arguments
	movb $0, %al
	call printf@plt
	movq $0, %rdi
	call fflush@plt
	movq %rbp, %rsp
	popq %rbp
	ret
-}

printp :: Program
printp =
  [ Dir DirSection
  , Dir $ DirInt 2
  , Lab (S ".L._printp_str0")
  , Dir $ DirAsciz "%p"
  , Dir DirText
  , Lab (R PrintP)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Movq (Reg Rdi) (Reg Rsi)
  , Leaq (Mem (MRegL (S ".L._printp_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cprintf
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
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
println :: Program
println =
  [ Dir DirSection
  , Dir $ DirInt 0
  , Lab (S ".L._println_str0")
  , Dir $ DirAsciz ""
  , Dir DirText
  , Lab (R PrintLn)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._println_str0") Rip)) (Reg Rdi)
  , Call cputs
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
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
free :: Program
free =
  [ Lab (R Free)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Call cfree
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
malloc :: Program
malloc =
  [ Lab (R Malloc)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Call cmalloc
  , Cmpq (Imm (IntLitQ 0)) (Reg Rax)
  , Je (R ErrOutOfMemory)
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
errOutOfMemory :: Program
errOutOfMemory =
  [ Dir DirSection
  , Dir $ DirInt 27
  , Lab (S ".L._errOutOfMemory_str0")
  , Dir $ DirAsciz "fatal error: out of memory\n"
  , Dir DirText
  , Lab (R ErrOutOfMemory)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOutOfMemory_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (IntLitB (-1))) (Reg Dil)
  , Call cexit
  ]

{-
.section .rodata
# length of .L._errOutOfBounds_str0
	.int 42
.L._errOutOfBounds_str0:
	.asciz "fatal error: array index %d out of bounds\n"
.text
_errOutOfBounds:
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	leaq .L._errOutOfBounds_str0(%rip), %rdi
	# on x86, al represents the number of SIMD registers used as variadic arguments
	movb $0, %al
	call printf@plt
	movq $0, %rdi
	call fflush@plt
	movb $-1, %dil
	call exit@plt
-}
errOutOfBounds :: Program
errOutOfBounds =
  [ Dir DirSection
  , Dir $ DirInt 42
  , Lab (S ".L._errOutOfBounds_str0")
  , Dir $
      DirAsciz
        "fatal error: variable failed boundary check. Expected: 0 <= x < %d, while x = %d\n"
  , Dir DirText
  , Lab (R ErrOutOfBounds)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOutOfBounds_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Movq (Reg Rsi) (Reg Rdx)
  , Movq (Reg Rdi) (Reg Rsi)
  , Call cprintf
  , Movq (Imm (IntLitQ 0)) (Reg Rdi)
  , Call cfflush
  , Movb (Imm (IntLitB (-1))) (Reg Dil)
  , Call cexit
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
errOverflow :: Program
errOverflow =
  [ Dir DirSection
  , Dir $ DirInt 52
  , Lab (S ".L._errOverflow_str0")
  , Dir $ DirAsciz "fatal error: integer overflow or underflow occurred\n"
  , Dir DirText
  , Lab (R ErrOverflow)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOverflow_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (IntLitB (-1))) (Reg Dil)
  , Call cexit
  ]

{-
.section .rodata
# length of .L._errDivZero_str0
	.int 40
.L._errDivZero_str0:
	.asciz "fatal error: division or modulo by zero\n"
.text
_errDivZero:
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	leaq .L._errDivZero_str0(%rip), %rdi
	call _prints
	movb $-1, %dil
	call exit@plt
-}

errDivByZero :: Program
errDivByZero =
  [ Dir DirSection
  , Dir $ DirInt 40
  , Lab (S ".L._errDivZero_str0")
  , Dir $ DirAsciz "fatal error: division or modulo by zero\n"
  , Dir DirText
  , Lab (R ErrDivByZero)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errDivZero_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (IntLitB (-1))) (Reg Dil)
  , Call cexit
  ]

{-
.section .rodata
# length of .L._readi_str0
	.int 2
.L._readi_str0:
	.asciz "%d"
.text
_readi:
	pushq %rbp
	movq %rsp, %rbp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	# RDI contains the "original" value of the destination of the read
	# allocate space on the stack to store the read: preserve alignment!
	# the passed default argument should be stored in case of EOF
	subq $16, %rsp
	movl %edi, (%rsp)
	leaq (%rsp), %rsi
	leaq .L._readi_str0(%rip), %rdi
	# on x86, al represents the number of SIMD registers used as variadic arguments
	movb $0, %al
	call scanf@plt
	movslq (%rsp), %rax
	addq $16, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
-}

readi :: Program
readi =
  [ Dir DirSection
  , Dir $ DirInt 2
  , Lab (S ".L._readi_str0")
  , Dir $ DirAsciz "%d"
  , Dir DirText
  , Lab (R ReadI)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Subq (Imm (IntLitQ 16)) (Reg Rsp)
  , Movl (Reg Edi) (Mem (MRegI 0 Rsp))
  , Leaq (Mem (MRegI 0 Rsp)) (Reg Rsi)
  , Leaq (Mem (MRegL (S ".L._readi_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cscanf
  , Movslq (Mem (MRegI 0 Rsp)) (Reg Rax)
  , Addq (Imm (IntLitQ 16)) (Reg Rsp)
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
section .rodata
 length of .L._readc_str0
.int 3
L._readc_str0:
.asciz " %c"
text
readc:
pushq %rbp
movq %rsp, %rbp
# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
andq $-16, %rsp
# RDI contains the "original" value of the destination of the read
# allocate space on the stack to store the read: preserve alignment!
# the passed default argument should be stored in case of EOF
subq $16, %rsp
movb %dil, (%rsp)
leaq (%rsp), %rsi
leaq .L._readc_str0(%rip), %rdi
# on x86, al represents the number of SIMD registers used as variadic arguments
movb $0, %al
call scanf@plt
movsbq (%rsp), %rax
addq $16, %rsp
movq %rbp, %rsp
popq %rbp
ret
-}

readc :: Program
readc =
  [ Dir DirSection
  , Dir $ DirInt 3
  , Lab (S ".L._readc_str0")
  , Dir $ DirAsciz " %c"
  , Dir DirText
  , Lab (R ReadC)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Subq (Imm (IntLitQ 16)) (Reg Rsp)
  , Movb (Reg Dil) (Mem (MRegI 0 Rsp))
  , Leaq (Mem (MRegI 0 Rsp)) (Reg Rsi)
  , Leaq (Mem (MRegL (S ".L._readc_str0") Rip)) (Reg Rdi)
  , Movb (Imm (IntLitB 0)) (Reg Al)
  , Call cscanf
  , Movsbq (Mem (MRegI 0 Rsp)) (Reg Rax)
  , Addq (Imm (IntLitQ 16)) (Reg Rsp)
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
_exit:
	pushq %rbp
	movq %rsp, %rbp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	call exit@plt
	movq %rbp, %rsp
	popq %rbp
	ret
-}
exit :: Program
exit =
  [ Lab (R Exit)
  , Pushq (Reg Rbp)
  , Movq (Reg Rsp) (Reg Rbp)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Call cexit
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
.section .rodata
# length of .L._errNull_str0
	.int 45
.L._errNull_str0:
	.asciz "fatal error: null pair dereferenced or freed\n"
.text
_errNull:
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	andq $-16, %rsp
	leaq .L._errNull_str0(%rip), %rdi
	call _prints
	movb $-1, %dil
	call exit@plt
-}

errNull :: Program
errNull =
  [ Dir DirSection
  , Dir $ DirInt 45
  , Lab (S ".L._errNull_str0")
  , Dir $ DirAsciz "fatal error: null pair dereferenced or freed\n"
  , Dir DirText
  , Lab (R ErrNull)
  , Andq (Imm (IntLitQ (-16))) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errNull_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (IntLitB (-1))) (Reg Dil)
  , Call cexit
  ]

-- | Print a program, useful for debugging in GHCi
printProgram :: Program -> IO ()
printProgram = putStrLn . formatA
