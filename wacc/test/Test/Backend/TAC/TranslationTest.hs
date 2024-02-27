module Test.Backend.TAC.TranslationTest where

import Data.Map
import Language.WACC.TAC.FType
import Language.WACC.TAC.TAC
import Language.WACC.X86.Runtime (printProg)
import Language.WACC.X86.Translate (translateProg)
import Language.WACC.X86.X86 (Prog)

exitBlock :: BasicBlock Integer Integer
exitBlock =
  BasicBlock
    [ LoadCI (Var 0) 0
    , Exit (Var 0)
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

mainFuntion :: Func Integer Integer
mainFuntion = Func 0 [] (fromList [(0, helloWorldBlock)])

mainProgram :: TACProgram Integer Integer
mainProgram = fromList [(0, mainFuntion)]

asm :: Prog
asm = translateProg mainProgram

asm' :: IO ()
asm' = printProg asm
