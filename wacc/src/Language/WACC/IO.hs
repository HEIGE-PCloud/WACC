{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
The entrypoint for the compiler executable.
-}
module Language.WACC.IO (main, readProgramFile) where

import Control.Exception (handle)
import Data.List.Extra (replace)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStrLn)
import Language.WACC.AST.Prog (Prog)
import Language.WACC.AST.WType (WType)
import Language.WACC.Error (Error, printError, semanticError)
import Language.WACC.Parser.Stmt (parseWithError, program)
import Language.WACC.Parser.Token (fully)
import Language.WACC.Semantic.Scope (Fnident, VarST, Vident, scopeAnalysis)
import Language.WACC.TypeChecking (checkTypes)
import System.Console.CmdArgs
  ( Data
  , Typeable
  , argPos
  , cmdArgs
  , def
  , help
  , name
  , summary
  , typ
  , (&=)
  )
import System.Exit
  ( ExitCode (ExitFailure)
  , exitSuccess
  , exitWith
  )
import System.FilePath.Posix (takeBaseName)
import System.IO.Error
  ( isDoesNotExistError
  , isFullError
  , isPermissionError
  )
import Text.Gigaparsec (Result (..))
import Text.Gigaparsec.Position (Pos)
import Text.RawString.QQ

ioErrorCode :: ExitCode
ioErrorCode = ExitFailure 255

syntaxErrorCode :: ExitCode
syntaxErrorCode = ExitFailure 100

semanticErrorCode :: ExitCode
semanticErrorCode = ExitFailure 200

handleIOExceptions :: IO a -> IO a
handleIOExceptions =
  handle
    ( (*> exitWith ioErrorCode)
        . hPutStrLn stderr
        . ("File I/O error: " ++)
        . getReason
    )
  where
    getReason err
      | isDoesNotExistError err = "input file does not exist"
      | isFullError err = "disk is full"
      | isPermissionError err = "input file cannot be read (permission error)"
      | otherwise = show err

{- |
Read a WACC source file, replacing each tab character with two spaces.
-}
readProgramFile :: FilePath -> IO String
readProgramFile = fmap (replace "\t" "  ") . readFile

type Result' = Result ([Error], ExitCode) (Prog WType Fnident Vident Pos, VarST)

data Compile = Compile {file :: FilePath, parseOnly :: Bool}
  deriving (Show, Data, Typeable)

compileArgs :: Compile
compileArgs =
  Compile
    { file = def &= argPos 0 &= typ "FILE"
    , parseOnly = def &= help "Run in parse-only mode" &= name "parseOnly"
    }
    &= summary "WACC Compiler - Group 19"

{- |
The entrypoint.
-}
main :: IO ()
main = handleIOExceptions $ do
  args <- cmdArgs compileArgs
  let
    filename = file args
  let
    codeGen = not $ parseOnly args
  sourceCode <- readProgramFile filename
  let
    printError' = printError filename (lines sourceCode)
  case runParse sourceCode of
    Success ast -> if codeGen then runCodeGen filename ast else exitSuccess
    Failure (errs, exitCode) ->
      mapM_ (putStrLn . printError' semanticError) errs >> exitWith exitCode

runParse
  :: String -> Result'
runParse sourceCode = case parseWithError (fully program) sourceCode of
  Failure err -> Failure ([err], syntaxErrorCode)
  Success ast -> runScopeAnalysis ast

runScopeAnalysis
  :: Prog WType String String Pos -> Result'
runScopeAnalysis ast = case scopeAnalysis ast of
  Failure errs -> Failure (errs, semanticErrorCode)
  Success res -> runTypeCheck res

runTypeCheck
  :: (Prog WType Fnident Vident Pos, VarST) -> Result'
runTypeCheck ast = case uncurry checkTypes ast of
  [] -> Success ast
  errs -> Failure (errs, semanticErrorCode)

runCodeGen :: String -> (Prog WType Fnident Vident Pos, VarST) -> IO ()
runCodeGen path _ = writeFile filename testCode >>= const exitSuccess
  where
    filename = takeBaseName path ++ ".s"

testCode :: String
testCode =
  [r|
.globl main
.section .rodata
# length of .L.str0
    .int 24
.L.str0:
    .asciz "enter an integer to echo"
.text
main:
    pushq %rbp
    # pushq {%rbx, %r12}
    subq $16, %rsp
    movq %rbx, (%rsp)
    movq %r12, 8(%rsp)
    movq %rsp, %rbp
    # Stack pointer unchanged, no stack allocated variables
    movq $1, %rax
    movq %rax, %r12
    # Stack pointer unchanged, no stack allocated arguments
    leaq .L.str0(%rip), %rax
    pushq %rax
    popq %rax
    movq %rax, %rax
    movq %rax, %rdi
    # statement primitives do not return results (but will clobber r0/rax)
    call _prints
    call _println
    # Stack pointer unchanged, no stack allocated arguments
    # load the current value in the destination of the read so it supports defaults
    movq %r12, %rax
    movq %rax, %rdi
    call _readi
    movq %rax, %r11
    movq %r11, %rax
    movq %rax, %r12
    # Stack pointer unchanged, no stack allocated arguments
    movq %r12, %rax
    movq %rax, %rdi
    # statement primitives do not return results (but will clobber r0/rax)
    call _printi
    call _println
    # Stack pointer unchanged, no stack allocated variables
    movq $0, %rax
    # popq {%rbx, %r12}
    movq (%rsp), %rbx
    movq 8(%rsp), %r12
    addq $16, %rsp
    popq %rbp
    ret

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

.section .rodata
# length of .L._printi_str0
    .int 2
.L._printi_str0:
    .asciz "%d"
.text
_printi:
    pushq %rbp
    movq %rsp, %rbp
    # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
    andq $-16, %rsp
    movl %edi, %esi
    leaq .L._printi_str0(%rip), %rdi
    # on x86, al represents the number of SIMD registers used as variadic arguments
    movb $0, %al
    call printf@plt
    movq $0, %rdi
    call fflush@plt
    movq %rbp, %rsp
    popq %rbp
    ret

.section .rodata
# length of .L._println_str0
    .int 0
.L._println_str0:
    .asciz ""
.text
_println:
    pushq %rbp
    movq %rsp, %rbp
    # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
    andq $-16, %rsp
    leaq .L._println_str0(%rip), %rdi
    call puts@plt
    movq $0, %rdi
    call fflush@plt
    movq %rbp, %rsp
    popq %rbp
    ret


|]
