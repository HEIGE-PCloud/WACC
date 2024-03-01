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

temp0, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8 :: Var Int
temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : temp7 : temp8 : _ = Temp <$> [0 ..]

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
    , testProperty "Decl uses Decl" $ \v i1 i2 ->
        toTAC'
          ( AST.Decl
              WInt
              v
              ( AST.RVExpr
                  ( AST.Add
                      (intLit i1)
                      (intLit i2)
                      BInt
                  )
                  BInt
              )
              BAny
          )
          === (
                [ LoadCI temp1 i1
                , LoadCI temp2 i2
                , BinInstr (Var v) temp1 Language.WACC.TAC.TAC.Add temp2
                ]
              , []
              )
    , testProperty "Asgn uses Asgn with LVIdent" $ \v i1 i2 ->
        toTAC'
          ( AST.Asgn
              (AST.LVIdent v BInt)
              ( AST.RVExpr
                  ( AST.Add
                      (intLit i1)
                      (intLit i2)
                      BInt
                  )
                  BInt
              )
              BAny
          )
          === (
                [ LoadCI temp1 i1
                , LoadCI temp2 i2
                , BinInstr (Var v) temp1 Language.WACC.TAC.TAC.Add temp2
                ]
              , []
              )
    , testProperty "Asgn uses Asgn with LVArrayElem" $ \v e i1 i2 ->
        toTAC'
          ( AST.Asgn
              ( AST.LVArrayElem
                  ( AST.ArrayIndex v [intLit e] (BArray BInt)
                  )
                  BInt
              )
              ( AST.RVExpr
                  ( AST.Add
                      (intLit i1)
                      (intLit i2)
                      BInt
                  )
                  BInt
              )
              BAny
          )
          === (
                [ LoadCI temp1 4
                , LoadCI temp2 4
                , LoadCI temp3 e
                , BinInstr temp4 temp3 Language.WACC.TAC.TAC.Mul temp2
                , BinInstr temp5 temp4 Language.WACC.TAC.TAC.Add temp1
                , LoadCI temp7 i1
                , LoadCI temp8 i2
                , BinInstr temp6 temp7 Language.WACC.TAC.TAC.Add temp8
                , Store (Var v) temp5 temp6 WIntF
                ]
              , []
              )
    , testProperty "Asgn uses Asgn with LVPairElem" $ \v i1 i2 ->
        toTAC'
          ( AST.Asgn
              ( AST.LVPairElem
                  ( AST.FstElem (LVIdent v BErasedPair) BInt
                  )
                  BInt
              )
              ( AST.RVExpr
                  ( AST.Add
                      (intLit i1)
                      (intLit i2)
                      BInt
                  )
                  BInt
              )
              BAny
          )
          === (
                [ LoadCI temp1 v
                , LoadCI temp3 i1
                , LoadCI temp4 i2
                , BinInstr temp2 temp3 Language.WACC.TAC.TAC.Add temp4
                , Store temp0 temp1 temp2 WIntF
                ]
              , []
              )
    ]

jump0 :: Jump Int Int
jump0 = Jump $ Label 0

stmtsToTAC'
  :: Stmts Int Int BType -> Int -> Jump Int Int -> Map Int (BasicBlock Int Int)
stmtsToTAC' stmts l j =
  testTACM $ (stmtsToTAC stmts l >>= ($ j)) *> collectBlocks

stmtsTestGroup :: TestTree
stmtsTestGroup = testGroup "statement groups" []
