{-# LANGUAGE PatternSynonyms #-}

module Test.Backend.TAC.TranslationTest where

import Control.DeepSeq (deepseq)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Map (fromList)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Language.WACC.AST (Prog, WType)
import Language.WACC.IO (runParse)
import Language.WACC.TAC.Class (FnToTAC (..))
import Language.WACC.TAC.FType (pattern FString)
import Language.WACC.TAC.State (evalTACM)
import Language.WACC.TAC.TAC
  ( BasicBlock (BasicBlock)
  , Jump (Exit, Ret)
  , TAC (LoadCI, LoadCS, PrintLn)
  , TACFunc (..)
  , TACProgram
  , Var (Var)
  )
import Language.WACC.TypeChecking (BType)
import Language.WACC.X86.ATNT (formatA)
import Language.WACC.X86.Translate (translateProg)
import Language.WACC.X86.X86 (Program)
import System.IO (hGetContents, hPutStr)
import System.Process (runInteractiveProcess, waitForProcess)
import Text.Gigaparsec (Result (..))

exitBlock :: BasicBlock Integer Integer
exitBlock =
  BasicBlock
    [ LoadCI (Var 0) 0
    ]
    (Exit (Var 0))

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

runFrontendSource :: IO (Prog WType Integer Integer BType)
runFrontendSource = do
  sourceCode <- readFile "test/wacc_examples/valid/sequence/intLeadingZeros.wacc"
  return $ case runParse sourceCode of
    Success ast -> fst ast
    _ -> error "Failed to parse the source code."

astToTAC :: Prog WType Integer Integer BType -> TACProgram Integer Integer
astToTAC ast = evalTACM 1 (fnToTAC ast)

mainProgram :: IO (TACProgram Integer Integer)
mainProgram = do astToTAC <$> runFrontendSource

asm :: IO Program
asm = do translateProg <$> mainProgram

writeAsm :: IO ()
writeAsm = do
  prog <- asm
  putStrLn (formatA prog)
  writeFile "../test.s" (formatA prog)

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
