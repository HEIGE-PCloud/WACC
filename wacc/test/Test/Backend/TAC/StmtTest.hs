{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-type-defaults #-}

module Test.Backend.TAC.StmtTest (stmtTestGroup, stmtsTestGroup) where

import Data.DList
import Data.List.NonEmpty
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
testTACM = fst . runTACM 1

toTAC'
  :: AST.Stmt Int Int BType
  -> (DList (TAC Int Int), Map Int (BasicBlock Int Int))
toTAC' = testTACM . (*> ((,) <$> collectTACs <*> collectBlocks)) . fnToTAC

intLit :: (Integral a) => a -> Expr Int BType
intLit x = WAtom (IntLit (toInteger x) BInt) BInt

temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Int
temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _ = Temp <$> [0 ..]

temp7, temp8, temp9, temp10 :: Var Int
temp7 : temp8 : temp9 : temp10 : _ = Temp <$> [7 ..]

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
                [ LoadCI temp1 0
                , LoadCI temp2 4
                , LoadCI temp3 4
                , LoadCI temp4 e
                , LoadM temp5 (Var v) temp1 FInt
                , CheckBounds temp4 temp5
                , BinInstr temp6 temp4 Language.WACC.TAC.TAC.Mul temp3
                , BinInstr temp7 temp6 Language.WACC.TAC.TAC.Add temp2
                , LoadCI temp9 i1
                , LoadCI temp10 i2
                , BinInstr temp8 temp9 Language.WACC.TAC.TAC.Add temp10
                , Store (Var v) temp7 temp8 WIntF
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
                [ Move temp1 (Var v)
                , LoadCI temp2 0
                , LoadCI temp4 i1
                , LoadCI temp5 i2
                , BinInstr temp3 temp4 Language.WACC.TAC.TAC.Add temp5
                , Store temp1 temp2 temp3 WIntF
                ]
              , []
              )
    , testProperty "Asgn uses Asgn with RVArrayLit" $ \v ->
        toTAC'
          ( AST.Asgn
              (AST.LVIdent v BInt)
              (AST.RVArrayLit [intLit 5, intLit 6, intLit 7] (BArray BInt))
              BAny
          )
          === (
                [ LoadCI temp1 16
                , LoadCI temp2 3
                , LoadCI temp3 0
                , Malloc (Var v) temp1
                , Store (Var v) temp3 temp2 WIntF
                , LoadCI temp4 5
                , LoadCI temp5 4
                , Store (Var v) temp5 temp4 WIntF
                , LoadCI temp6 6
                , LoadCI temp7 8
                , Store (Var v) temp7 temp6 WIntF
                , LoadCI temp8 7
                , LoadCI temp9 12
                , Store (Var v) temp9 temp8 WIntF
                ]
              , []
              )
    , testProperty "Asgn Uses Asgn with RVNewPair" $ \v i1 i2 ->
        toTAC'
          ( AST.Asgn
              (AST.LVIdent v (BKnownPair BInt BInt))
              (RVNewPair (intLit i1) (intLit i2) (BKnownPair BInt BInt))
              BAny
          )
          === (
                [ LoadCI temp1 i1
                , LoadCI temp2 i2
                , LoadCI temp3 0
                , LoadCI temp4 8
                , LoadCI temp5 16
                , Malloc (Var v) temp5
                , Store (Var v) temp3 temp1 WIntF
                , Store (Var v) temp4 temp2 WIntF
                ]
              , []
              )
    , testProperty "Return Creates instructions for var" $ \x ->
        toTAC' (AST.Return (intLit x) BAny) === ([LoadCI temp1 x], [])
    , testProperty "Return Creates instructions for simple expression" $ \i1 i2 ->
        toTAC' (AST.Return (AST.Add (intLit i1) (intLit i2) BInt) BAny)
          === (
                [ LoadCI temp2 i1
                , LoadCI temp3 i2
                , BinInstr temp1 temp2 Language.WACC.TAC.TAC.Add temp3
                ]
              , []
              )
    , testProperty "Return Creates BasicBlock for var" $ \x l ->
        stmtsToTAC'
          (AST.Stmts [(AST.Return (intLit x) BAny)])
          0
          (Jump l)
          === [(0, BasicBlock {block = [LoadCI temp1 x], nextBlock = (Ret temp1)})]
    , testProperty "Exit Creates instructions for var" $ \x ->
        toTAC' (AST.Exit (intLit x) BAny) === ([LoadCI temp1 x], [])
    , testProperty "Exit Creates instructions for simple expression" $ \i1 i2 ->
        toTAC' (AST.Exit (AST.Add (intLit i1) (intLit i2) BInt) BAny)
          === (
                [ LoadCI temp2 i1
                , LoadCI temp3 i2
                , BinInstr temp1 temp2 Language.WACC.TAC.TAC.Add temp3
                ]
              , []
              )
    , testProperty "Exit Creates BasicBlock for var" $ \x l ->
        stmtsToTAC'
          (AST.Stmts [(AST.Exit (intLit x) BAny)])
          0
          (Jump l)
          === [(0, BasicBlock {block = [LoadCI temp1 x], nextBlock = (Exit temp1)})]
    , testProperty "If Else creates If Else Block with Return" $ \b x l ->
        stmtsToTAC'
          ( AST.Stmts
              [ AST.IfElse
                  (WAtom (BoolLit b BBool) BBool)
                  (AST.Stmts [(AST.Return (intLit x) BAny)])
                  (AST.Stmts [(AST.Return (intLit x) BAny)])
                  BAny
              ]
          )
          0
          (Jump l)
          === [
                ( 0
                , BasicBlock
                    { block = [LoadCI temp1 (if b then 1 else 0)]
                    , nextBlock = CJump temp1 1 2
                    }
                )
              , (1, BasicBlock {block = [LoadCI temp2 x], nextBlock = Ret temp2})
              , (2, BasicBlock {block = [LoadCI temp3 x], nextBlock = Ret temp3})
              , (3, BasicBlock {block = [], nextBlock = Jump l})
              ]
    , testProperty "If Else creates If Else Block" $ \b l ->
        stmtsToTAC'
          ( AST.Stmts
              [ AST.IfElse
                  (WAtom (BoolLit b BBool) BBool)
                  (AST.Stmts [(AST.Skip BAny)])
                  (AST.Stmts [(AST.Skip BAny)])
                  BAny
              ]
          )
          0
          (Jump l)
          === [
                ( 0
                , BasicBlock
                    { block = [LoadCI temp1 (if b then 1 else 0)]
                    , nextBlock = CJump temp1 1 2
                    }
                )
              , (1, BasicBlock {block = [], nextBlock = Jump 3})
              , (2, BasicBlock {block = [], nextBlock = Jump 3})
              , (3, BasicBlock {block = [], nextBlock = Jump l})
              ]
    , testProperty "While creates While Block With Return" $ \b x y ->
        stmtsToTAC'
          ( AST.Stmts
              [ AST.While
                  (WAtom (BoolLit b BBool) BBool)
                  (AST.Stmts [(AST.Return (intLit y) BAny)])
                  BAny
              ]
          )
          0
          (Jump x)
          === [ (0, BasicBlock {block = [], nextBlock = Jump 1})
              ,
                ( 1
                , BasicBlock
                    { block = [LoadCI temp1 (if b then 1 else 0)]
                    , nextBlock = CJump temp1 2 3
                    }
                )
              , (2, BasicBlock {block = [LoadCI temp2 y], nextBlock = Ret temp2})
              , (3, BasicBlock {block = [], nextBlock = Jump x})
              ]
    , testProperty "While creates While Block" $ \b x ->
        stmtsToTAC'
          ( AST.Stmts
              [ AST.While
                  (WAtom (BoolLit b BBool) BBool)
                  (AST.Stmts [(AST.Skip BAny)])
                  BAny
              ]
          )
          0
          (Jump x)
          === [ (0, BasicBlock {block = [], nextBlock = Jump 1})
              ,
                ( 1
                , BasicBlock
                    { block = [LoadCI temp1 (if b then 1 else 0)]
                    , nextBlock = CJump temp1 2 3
                    }
                )
              , (2, BasicBlock {block = [], nextBlock = Jump 1})
              , (3, BasicBlock {block = [], nextBlock = Jump x})
              ]
    , testProperty "BeginEnd creates BeginEnd Block With Return" $ \x y ->
        stmtsToTAC'
          (AST.Stmts [(AST.BeginEnd (AST.Stmts [(AST.Return (intLit y) BAny)]) BAny)])
          0
          (Jump $ x)
          === [ (0, BasicBlock {block = [LoadCI temp1 y], nextBlock = Jump 1})
              , (1, BasicBlock {block = [], nextBlock = Ret temp1})
              , (2, BasicBlock {block = [], nextBlock = Jump x})
              ]
    , testProperty "BeginEnd creates BeginEnd Block" $ \x ->
        stmtsToTAC'
          (AST.Stmts [(AST.BeginEnd (AST.Stmts [(AST.Skip BAny)]) BAny)])
          0
          (Jump x)
          === [ (0, BasicBlock {block = [], nextBlock = Jump 1})
              , (1, BasicBlock {block = [], nextBlock = Jump 2})
              , (2, BasicBlock {block = [], nextBlock = Jump x})
              ]
    ]

jump0 :: Jump Int Int
jump0 = Jump 0

stmtsToTAC'
  :: Stmts Int Int BType -> Int -> Jump Int Int -> Map Int (BasicBlock Int Int)
stmtsToTAC' stmts l j =
  testTACM $ (stmtsToTAC stmts l >>= ($ j)) *> collectBlocks

stmtsTestGroup :: TestTree
stmtsTestGroup =
  testGroup
    "statement groups"
    [ testProperty "Singleton Skip" $ \l ->
        stmtsToTAC' (AST.Stmts [AST.Skip BAny]) 0 (Jump l)
          === [(0, BasicBlock {block = [], nextBlock = Jump l})]
    , testProperty "Singleton Read" $ \v l ->
        stmtsToTAC' (AST.Stmts [AST.Read (LVIdent v BInt) BAny]) 0 (Jump l)
          === [(0, BasicBlock {block = [Read (Var v) FInt], nextBlock = Jump l})]
    , testProperty "Singleton Free" $ \v l ->
        stmtsToTAC'
          (AST.Stmts [AST.Free (WAtom (Ident v BErasedPair) BErasedPair) BAny])
          0
          (Jump l)
          === [
                ( 0
                , BasicBlock
                    { block = [Move temp1 (Var v), Free temp1]
                    , nextBlock = Jump l
                    }
                )
              ]
    , testProperty "Singleton Print" $ \i l ->
        stmtsToTAC' (AST.Stmts [AST.Print (intLit i) BAny]) 0 (Jump l)
          === [
                ( 0
                , BasicBlock
                    { block = [LoadCI temp1 i, Print temp1 FInt]
                    , nextBlock = Jump l
                    }
                )
              ]
    , testProperty "Singleton PrintLn" $ \i l ->
        stmtsToTAC' (AST.Stmts [AST.PrintLn (intLit i) BAny]) 0 (Jump l)
          === [
                ( 0
                , BasicBlock
                    { block = [LoadCI temp1 i, PrintLn temp1 FInt]
                    , nextBlock = Jump l
                    }
                )
              ]
    , testProperty "Singleton Decl" $ \v i1 i2 l ->
        stmtsToTAC'
          ( AST.Stmts
              [ AST.Decl
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
              ]
          )
          0
          (Jump l)
          === [
                ( 0
                , BasicBlock
                    { block =
                        [ LoadCI temp1 i1
                        , LoadCI temp2 i2
                        , BinInstr (Var v) temp1 Language.WACC.TAC.TAC.Add temp2
                        ]
                    , nextBlock = Jump l
                    }
                )
              ]
    ]
