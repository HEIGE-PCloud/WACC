module Test.Backend.TAC.TranslationTest where

import Control.DeepSeq (deepseq)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Map
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Language.WACC.TAC.FType
import Language.WACC.TAC.TAC
import Language.WACC.X86.Runtime (printProg)
import Language.WACC.X86.Translate (translateProg)
import Language.WACC.X86.X86 (Prog, formatA)
import System.IO
import System.Process

exitBlock :: BasicBlock Integer Integer
exitBlock =
  BasicBlock
    [ LoadCI (Var 0) 0
    ]
    (Ret (Var 0))

helloWorldBlock :: BasicBlock Integer Integer
helloWorldBlock =
  BasicBlock
    [ LoadCI (Var 0) 0
    , LoadCS (Var 1) "Hello, world!\n"
    , PrintLn (Var 1) FString
    ]
    (Ret (Var 0))

mainFuntion :: TACFunc Integer Integer
mainFuntion = TACFunc 0 [] (fromList [(0, helloWorldBlock)])

mainProgram :: TACProgram Integer Integer
mainProgram = fromList [(0, mainFuntion)]

asm :: Prog
asm = translateProg mainProgram

asm' :: IO ()
asm' = printProg asm

writeAsm :: IO ()
writeAsm = writeFile "../test.s" (formatA asm)

compileAsm :: IO ()
compileAsm = do
  writeAsm
  (_, stdoutH, stderrH, pid) <-
    runInteractiveProcess
      "gcc"
      ["-o", "a.out", "-z", "noexecstack", "test.s"]
      (Just "..")
      Nothing
  stderrStr <- hGetContents stderrH
  stdoutStr <- hGetContents stdoutH
  ecode <- stderrStr `deepseq` stdoutStr `deepseq` waitForProcess pid
  when (ecode /= ExitSuccess) $ do
    putStrLn $ "Receiving stderr:\n" ++ stderrStr
    putStrLn $ "Receiving stdout:\n" ++ stdoutStr

runAsm' :: String -> IO ()
runAsm' str = do
  (stdinH, stdoutH, stderrH, _) <-
    runInteractiveProcess "./a.out" [] (Just "..") Nothing
  for_ ((++ "\n") <$> Just str) (hPutStr stdinH)
  stderrStr <- hGetContents stderrH
  stdoutStr <- hGetContents stdoutH
  putStrLn $ "Stderr\n" ++ stderrStr
  putStrLn $ "Stdout\n" ++ stdoutStr
  return ()

runAsm :: IO ()
runAsm = runAsm' ""

runrunrun :: IO ()
runrunrun = do
  writeAsm
  compileAsm
  runAsm
  return ()
