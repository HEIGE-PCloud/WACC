{-# LANGUAGE OverloadedLists #-}

module Test.Backend.TAC.StmtTest (stmtTestGroup, stmtsTestGroup) where

import Data.DList
import Data.Map
import Language.WACC.AST hiding (Stmt (..))
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.Stmt
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking
import Test

testTACM :: TACM Int Int a -> a
testTACM = fst . runTACM 0

toTAC'
  :: AST.Stmt Int Int BType
  -> (DList (TAC Int Int), Map Int (BasicBlock Int Int))
toTAC' = testTACM . (*> ((,) <$> collectTACs <*> collectBlocks)) . fnToTAC

intLit :: (Integral a) => a -> Expr Int BType
intLit x = WAtom (IntLit (toInteger x) BInt) BInt

temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Int
temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _ = Temp <$> [0 ..]

stmtTestGroup :: TestTree
stmtTestGroup =
  testGroup
    "statements"
    [ testCase "skip generates no instructions" $
        toTAC' (AST.Skip BAny) @?= ([], [])
    , testProperty "read uses Read" $ \v ->
        toTAC' (AST.Read (LVIdent v BInt) BAny) === ([Read (Var v) FInt], [])
    , testProperty "free uses Free" $ \v ->
        toTAC' (AST.Free (WAtom (Ident v BErasedPair) BErasedPair) BAny)
          === ([Move temp1 (Var v), Free temp1], [])
    , testProperty "print uses Print" $ \i ->
        toTAC' (AST.Print (intLit i) BAny)
          === ([LoadCI temp1 i, Print temp1 FInt], [])
    , testProperty "println uses PrintLn" $ \i ->
        toTAC' (AST.PrintLn (intLit i) BAny)
          === ([LoadCI temp1 i, PrintLn temp1 FInt], [])
    ]

jump0 :: Jump Int Int
jump0 = Jump $ Label 0

stmtsToTAC'
  :: Stmts Int Int BType -> Int -> Jump Int Int -> Map Int (BasicBlock Int Int)
stmtsToTAC' stmts l j =
  testTACM $ (stmtsToTAC stmts l >>= ($ j)) *> collectBlocks

stmtsTestGroup :: TestTree
stmtsTestGroup = testGroup "statement groups" []
