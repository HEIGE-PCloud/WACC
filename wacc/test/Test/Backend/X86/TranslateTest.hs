-- {-# LANGUAGE GADTs #-}
module Test.Backend.X86.TranslateTest where

-- ( translateTestGroup
-- )

-- Test translation of TAC to X86

-- import Data.List (isInfixOf, isPrefixOf, isSubsequenceOf)
-- import Data.Map as M
-- import Language.WACC.TAC.FType
-- import Language.WACC.TAC.TAC hiding (Call, Label)
-- import qualified Language.WACC.TAC.TAC as TAC
-- import Language.WACC.X86.Runtime
-- import Language.WACC.X86.Translate (translateProg)
-- import Language.WACC.X86.X86
-- import Test

-- runTranslationTest
--   :: Program -> TACProgram Integer Integer -> (Program -> Program -> Bool) -> Bool
-- runTranslationTest x86Prog tacProg cmp = cmp x86Prog (translateProg tacProg)

-- preamble :: Program
-- preamble = [Dir $ DirGlobl (S "main"), Dir DirSection, Dir DirText, Lab $ S "main"]

-- -- T for TAC and X for X86

-- singleFunctionT
--   :: Map Integer (BasicBlock Integer Integer) -> TACProgram Integer Integer
-- singleFunctionT bbs = M.fromList [(0, Func 0 [] bbs)]

-- singleBlockT :: [TAC Integer Integer] -> TACProgram Integer Integer
-- singleBlockT ts =
--   singleFunctionT
--     ( M.fromList
--         [(0, BasicBlock {block = ts, nextBlock = Jump (TAC.Label 0)})]
--     )

-- singleBlockX :: Program
-- singleBlockX = [Lab (I 0), Jmp (I 0)]

-- translateTestGroup :: TestTree
-- translateTestGroup =
--   testGroup
--     "translateTest"
--     [ testGroup
--         "controlFlow"
--         [ testProperty "Boilerplate directives start every program" $
--             runTranslationTest preamble M.empty isPrefixOf
--         , testProperty "Single Block" $
--             runTranslationTest singleBlockX (singleBlockT []) isSubsequenceOf
--         , testProperty "Runtime library loads as necessary" $
--             runTranslationTest printi (singleBlockT [Print temp1 FInt]) isInfixOf
--         , testProperty "Overflow checking after addition" $
--             runTranslationTest
--               [Addl (Reg Rax) (Reg Rax), Jo (R ErrOverflow)]
--               (singleBlockT [BinInstr temp0 temp1 TAC.Add temp2])
--               wEq
--         , testProperty "Division by zero checking" $
--             runTranslationTest
--               [Cmpl (Imm 0) rax, Je (R ErrDivByZero)]
--               (singleBlockT [BinInstr temp0 temp1 TAC.Div temp2])
--               wEq
--               -- , testProperty "Translation of Store TAC" $
--               --    --runTranslationTest
--               --      [Movq (Reg Rax) (Reg Rax)] ===
--               --      (translateProg $ singleBlockT [Store temp0 temp1 temp2 FBool])
--               --      --wEq
--         ]
--     ]

-- rax = Reg Rax

-- temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Integer
-- temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _ = Temp <$> [0 ..]

-- temp7, temp8, temp9, temp10, temp11 :: Var Integer
-- temp7 : temp8 : temp9 : temp10 : temp11 : _ = Temp <$> [7 ..]

-- class WeakEq a where
--   wEq :: a -> a -> Bool

-- instance (WeakEq a) => WeakEq [a] where
--   wEq [] _ = True
--   wEq (x : xs) [] = False
--   wEq (x : xs) (y : ys) = wEq x y && wEq xs ys || wEq (x : xs) ys

-- instance WeakEq (Operand s t) where
--   wEq (Imm i1) (Imm i2) = i1 == i2
--   wEq (Imm _) _ = False
--   wEq _ (Imm _) = False
--   wEq _ _ = True

-- instance WeakEq Instruction where
--   wEq (Idivl op1) (Idivl op2) = op1 `wEq` op2
--   wEq (Sete op1) (Sete op2) = op1 `wEq` op2
--   wEq (Setne op1) (Setne op2) = op1 `wEq` op2
--   wEq (Setl op1) (Setl op2) = op1 `wEq` op2
--   wEq (Setle op1) (Setle op2) = op1 `wEq` op2
--   wEq (Setg op1) (Setg op2) = op1 `wEq` op2
--   wEq (Setge op1) (Setge op2) = op1 `wEq` op2
--   wEq (Negl op1) (Negl op2) = op1 `wEq` op2
--   wEq (Pushq op1) (Pushq op2) = op1 `wEq` op2
--   wEq (Popq op1) (Popq op2) = op1 `wEq` op2
--   wEq (Cmovl op1 op2) (Cmovl op3 op4) = op1 `wEq` op3 && op2 `wEq` op4
--   wEq (Cmovge op1 op2) (Cmovge op3 op4) = op1 `wEq` op3 && op2 `wEq` op4
--   wEq (Movslq op1 op2) (Movslq op3 op4) = op1 `wEq` op3 && op2 `wEq` op4
--   wEq (Movsbq op1 op2) (Movsbq op3 op4) = op1 `wEq` op3 && op2 `wEq` op4
--   wEq (Movzbl op1 op2) (Movzbl op3 op4) = op1 `wEq` op3 && op2 `wEq` op4
--   wEq x y = x == y
