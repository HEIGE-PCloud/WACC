module Language.WACC.X86.Runtime where

import qualified Data.DList as D
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import Language.WACC.X86.ATNT (formatA)
import Language.WACC.X86.X86

x86Examples :: [(Program, String)]
x86Examples =
  [ (arrLoad1, "arrLoad1")
  , (arrLoad4, "arrLoad4")
  , (arrLoad8, "arrLoad8")
  , (arrStore1, "arrStore1")
  , (arrStore4, "arrStore4")
  , (arrStore8, "arrStore8")
  , (printi, "printi")
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
    [ (ArrLoad1, D.fromList arrLoad1)
    , (ArrLoad4, D.fromList arrLoad4)
    , (ArrLoad8, D.fromList arrLoad8)
    , (ArrStore1, D.fromList arrStore1)
    , (ArrStore4, D.fromList arrStore4)
    , (ArrStore8, D.fromList arrStore8)
    , (PrintI, D.fromList printi)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Movl (Reg Edi) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._printi_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cprintf
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Movq (Reg Rdi) (Reg Rdx)
  , Movl (Mem (MRegI (-4) Rdi)) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._prints_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cprintf
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
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
  , Andq (Imm ((-16) :: Int64)) (Reg Rsp)
  , Cmpb (Imm (0 :: Int8)) (Reg Dl)
  , Jne (S ".L_printb0")
  , Leaq (Mem (MRegL (S ".L._printb_str0") Rip)) (Reg Rdx)
  , Jmp (S ".L_printb1")
  , Lab (S ".L_printb0")
  , Leaq (Mem (MRegL (S ".L._printb_str1") Rip)) (Reg Rdx)
  , Lab (S ".L_printb1")
  , Movl (Mem (MRegI (-4) Rdx)) (Reg Esi)
  , Leaq (Mem (MRegL (S ".L._printb_str2") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cprintf
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Movb (Reg Dl) (Reg Sil)
  , Leaq (Mem (MRegL (S ".L._printc_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cprintf
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Movq (Reg Rdi) (Reg Rsi)
  , Leaq (Mem (MRegL (S ".L._printp_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cprintf
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._println_str0") Rip)) (Reg Rdi)
  , Call cputs
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Call cmalloc
  , Cmpq (Imm (0 :: Int64)) (Reg Rax)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOutOfMemory_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (-1 :: Int8)) (Reg Dil)
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
  , Dir $ DirAsciz "fatal error: array index %d out of bounds\n"
  , Dir DirText
  , Lab (R ErrOutOfBounds)
  , Andq (Imm ((-16) :: Int64)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOutOfBounds_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cprintf
  , Movq (Imm (0 :: Int64)) (Reg Rdi)
  , Call cfflush
  , Movb (Imm (-1 :: Int8)) (Reg Dil)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errOverflow_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (-1 :: Int8)) (Reg Dil)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Leaq (Mem (MRegL (S ".L._errDivZero_str0") Rip)) (Reg Rdi)
  , Call (R PrintS)
  , Movb (Imm (-1 :: Int8)) (Reg Dil)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Subq (Imm (16 :: Int64)) (Reg Rsp)
  , Movl (Reg Edi) (Mem (MRegI 0 Rsp))
  , Leaq (Mem (MRegI 0 Rsp)) (Reg Rsi)
  , Leaq (Mem (MRegL (S ".L._readi_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cscanf
  , Movslq (Mem (MRegI 0 Rsp)) (Reg Rax)
  , Addq (Imm (16 :: Int64)) (Reg Rsp)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Subq (Imm (16 :: Int64)) (Reg Rsp)
  , Movb (Reg Dil) (Mem (MRegI 0 Rsp))
  , Leaq (Mem (MRegI 0 Rsp)) (Reg Rsi)
  , Leaq (Mem (MRegL (S ".L._readc_str0") Rip)) (Reg Rdi)
  , Movb (Imm (0 :: Int8)) (Reg Al)
  , Call cscanf
  , Movsbq (Mem (MRegI 0 Rsp)) (Reg Rax)
  , Addq (Imm (16 :: Int64)) (Reg Rsp)
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
  , Andq (Imm (-16 :: Int64)) (Reg Rsp)
  , Call cexit
  , Movq (Reg Rbp) (Reg Rsp)
  , Popq (Reg Rbp)
  , Ret
  ]

{-
_arrLoad1:
	# Special calling convention: array ptr passed in R9, index in R10, and return into R9
	pushq %rbx
	cmpl $0, %r10d
	cmovl %r10, %rsi
	jl _errOutOfBounds
	movl -4(%r9), %ebx
	cmpl %ebx, %r10d
  cmovge %r10, %rsi
  jge _errOutOfBounds
  movsbq (%r9,%r10), %r9
  popq %rbx
  ret
-}
arrLoad1 :: Program
arrLoad1 =
  [ Lab (R ArrLoad1)
  , Pushq (Reg Rbx)
  , Cmpl (Imm (0 :: Int32)) (Reg R10d)
  , Cmovl (Reg R10) (Reg Rsi)
  , Jl (R ErrOutOfBounds)
  , Movl (Mem (MRegI (-4) R9)) (Reg Ebx)
  , Cmpl (Reg Ebx) (Reg R10d)
  , Cmovge (Reg R10) (Reg Rsi)
  , Jge (R ErrOutOfBounds)
  , Movsbq (Mem (MScale R9 R10 1)) (Reg R9)
  , Popq (Reg Rbx)
  , Ret
  ]

{-
_arrStore1:
	# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
	pushq %rbx
	cmpl $0, %r10d
	cmovl %r10, %rsi
	jl _errOutOfBounds
	movl -4(%r9), %ebx
	cmpl %ebx, %r10d
	cmovge %r10, %rsi
	jge _errOutOfBounds
	movb %al, (%r9,%r10)
	popq %rbx
	ret
-}
arrStore1 :: Program
arrStore1 =
  [ Lab (R ArrStore1)
  , Pushq (Reg Rbx)
  , Cmpl (Imm (0 :: Int32)) (Reg R10d)
  , Cmovl (Reg R10) (Reg Rsi)
  , Jl (R ErrOutOfBounds)
  , Movl (Mem (MRegI (-4) R9)) (Reg Ebx)
  , Cmpl (Reg Ebx) (Reg R10d)
  , Cmovge (Reg R10) (Reg Rsi)
  , Jge (R ErrOutOfBounds)
  , Movb (Reg Al) (Mem (MScale R9 R10 1))
  , Popq (Reg Rbx)
  , Ret
  ]

{-
_arrStore4:
	# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
	pushq %rbx
	cmpl $0, %r10d
	cmovl %r10, %rsi
	jl _errOutOfBounds
	movl -4(%r9), %ebx
	cmpl %ebx, %r10d
	cmovge %r10, %rsi
	jge _errOutOfBounds
	movl %eax, (%r9,%r10,4)
	popq %rbx
	ret
-}
arrStore4 :: Program
arrStore4 =
  [ Lab (R ArrStore4)
  , Pushq (Reg Rbx)
  , Cmpl (Imm (0 :: Int32)) (Reg R10d)
  , Cmovl (Reg R10) (Reg Rsi)
  , Jl (R ErrOutOfBounds)
  , Movl (Mem (MRegI (-4) R9)) (Reg Ebx)
  , Cmpl (Reg Ebx) (Reg R10d)
  , Cmovge (Reg R10) (Reg Rsi)
  , Jge (R ErrOutOfBounds)
  , Movl (Reg Eax) (Mem (MScale R9 R10 4))
  , Popq (Reg Rbx)
  , Ret
  ]

{-
_arrLoad4:
	# Special calling convention: array ptr passed in R9, index in R10, and return into R9
  pushq %rbx
  cmpl $0, %r10d
  cmovl %r10, %rsi
  jl _errOutOfBounds
  movl -4(%r9), %ebx
  cmpl %ebx, %r10d
  cmovge %r10, %rsi
  jge _errOutOfBounds
  movslq (%r9,%r10,4), %r9
  popq %rbx
  ret
-}

arrLoad4 :: Program
arrLoad4 =
  [ Lab (R ArrLoad4)
  , Pushq (Reg Rbx)
  , Cmpl (Imm (0 :: Int32)) (Reg R10d)
  , Cmovl (Reg R10) (Reg Rsi)
  , Jl (R ErrOutOfBounds)
  , Movl (Mem (MRegI (-4) R9)) (Reg Ebx)
  , Cmpl (Reg Ebx) (Reg R10d)
  , Cmovge (Reg R10) (Reg Rsi)
  , Jge (R ErrOutOfBounds)
  , Movslq (Mem (MScale R9 R10 4)) (Reg R9)
  , Popq (Reg Rbx)
  , Ret
  ]

{-
_arrLoad8:
	# Special calling convention: array ptr passed in R9, index in R10, and return into R9
	pushq %rbx
	cmpl $0, %r10d
	cmovl %r10, %rsi
	jl _errOutOfBounds
	movl -4(%r9), %ebx
	cmpl %ebx, %r10d
	cmovge %r10, %rsi
	jge _errOutOfBounds
	movq (%r9,%r10,8), %r9
	popq %rbx
	ret
-}
arrLoad8 :: Program
arrLoad8 =
  [ Lab (R ArrLoad8)
  , Pushq (Reg Rbx)
  , Cmpl (Imm (0 :: Int32)) (Reg R10d)
  , Cmovl (Reg R10) (Reg Rsi)
  , Jl (R ErrOutOfBounds)
  , Movl (Mem (MRegI (-4) R9)) (Reg Ebx)
  , Cmpl (Reg Ebx) (Reg R10d)
  , Cmovge (Reg R10) (Reg Rsi)
  , Jge (R ErrOutOfBounds)
  , Movq (Mem (MScale R9 R10 8)) (Reg R9)
  , Popq (Reg Rbx)
  , Ret
  ]

{-
_arrStore8:
	# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
	pushq %rbx
	cmpl $0, %r10d
	cmovl %r10, %rsi
	jl _errOutOfBounds
	movl -4(%r9), %ebx
	cmpl %ebx, %r10d
  cmovge %r10, %rsi
  jge _errOutOfBounds
  movq %rax, (%r9,%r10,8)
  popq %rbx
  ret
-}
arrStore8 :: Program
arrStore8 =
  [ Lab (R ArrStore8)
  , Pushq (Reg Rbx)
  , Cmpl (Imm (0 :: Int32)) (Reg R10d)
  , Cmovl (Reg R10) (Reg Rsi)
  , Jl (R ErrOutOfBounds)
  , Movl (Mem (MRegI (-4) R9)) (Reg Ebx)
  , Cmpl (Reg Ebx) (Reg R10d)
  , Cmovge (Reg R10) (Reg Rsi)
  , Jge (R ErrOutOfBounds)
  , Movq (Reg Rax) (Mem (MScale R9 R10 8))
  , Popq (Reg Rbx)
  , Ret
  ]

-- | Print a program, useful for debugging in GHCi
printProgram :: Program -> IO ()
printProgram = putStrLn . formatA
