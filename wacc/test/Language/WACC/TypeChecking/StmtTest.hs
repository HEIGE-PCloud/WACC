{-# LANGUAGE OverloadedLists #-}

{- AUTOCOLLECT.TEST -}

module Language.WACC.TypeChecking.StmtTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.Map (singleton)
import Language.WACC.AST.Expr
import Language.WACC.AST.Stmt
import Language.WACC.AST.WType
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.State
import Language.WACC.TypeChecking.Stmt
import Test

testTypingM :: TypingM () BType a -> Either Int a
testTypingM action = case runTypingM action id fnMap of
  (Just x, _, []) -> Right x
  (_, _, es) -> Left $ length es
  where
    fnMap = singleton () (FnType [BInt, BBool] BBool)

checkStmt' :: Stmt () BType -> Either Int BType
checkStmt' = testTypingM . checkStmt

checkLValue' :: LValue BType -> Either Int BType
checkLValue' = testTypingM . checkLValue

checkRValue' :: RValue () BType -> Either Int BType
checkRValue' = testTypingM . checkRValue

checkPairElem' :: PairElem BType -> Either Int BType
checkPairElem' = testTypingM . checkPairElem

intExpr :: Expr BType
intExpr = WAtom (IntLit 0) undefined

boolExpr :: Expr BType
boolExpr = WAtom (BoolLit False) undefined

nullExpr :: Expr BType
nullExpr = WAtom Null undefined

varExpr :: BType -> Expr BType
varExpr = flip WAtom undefined . Ident

bIntPair :: BType
bIntPair = BKnownPair BInt BInt

wIntPair :: WType
wIntPair = WKnownPair WInt WInt

test :: TestTree
test =
  testGroup
    "unitTests"
    [ testGroup
        "checkStmt"
        [ testCase "ignores skip" $ checkStmt' Skip @?= pure BAny
        , testGroup
            "declaration"
            [ testCase "accepts identical type" $
                checkStmt' (Decl WInt BInt (RVExpr intExpr)) @?= pure BAny
            , testCase "accepts compatible type" $
                checkStmt' (Decl wIntPair bIntPair (RVExpr nullExpr))
                  @?= pure BAny
            , testCase "rejects incompatible type" $
                checkStmt' (Decl WInt BInt (RVExpr nullExpr)) @?= Left 1
            ]
        , testGroup
            "assignment"
            [ testCase "accepts identical type" $
                checkStmt' (Asgn (LVIdent BInt) (RVExpr intExpr)) @?= pure BAny
            , testCase "accepts compatible type" $
                checkStmt' (Asgn (LVIdent bIntPair) (RVExpr nullExpr))
                  @?= pure BAny
            , testCase "rejects incompatible type" $
                checkStmt' (Asgn (LVIdent BInt) (RVExpr nullExpr)) @?= Left 1
            ]
        , testProperty "ignores read" $
            \t -> checkStmt' (Read (LVIdent t)) == pure BAny
        , testGroup
            "free"
            [ testProperty "accepts arrays" $
                \t -> checkStmt' (Free (varExpr $ BArray t)) == pure BAny
            , testCase "accepts erased pairs" $
                checkStmt' (Free (varExpr BErasedPair)) @?= pure BAny
            , testProperty "accepts known pairs" $
                \t1 t2 ->
                  checkStmt' (Free (varExpr $ BKnownPair t1 t2)) == pure BAny
            ]
        , testProperty "return type is propagated" $
            \t -> checkStmt' (Return (varExpr t)) == pure t
        , testGroup
            "exit"
            [ testCase "accepts ints" $ checkStmt' (Exit intExpr) @?= pure BAny
            , testCase "rejects bools" $ checkStmt' (Exit boolExpr) @?= Left 1
            ]
        , testProperty "ignores print" $
            \t -> checkStmt' (Print (varExpr t)) == pure BAny
        , testProperty "ignores println" $
            \t -> checkStmt' (PrintLn (varExpr t)) == pure BAny
        , testGroup
            "if then else fi"
            [ testCase "accepts skips" $
                checkStmt' (IfElse boolExpr [Skip] [Skip]) @?= pure BAny
            , testProperty "accepts identical return types" $
                \t ->
                  let
                    body = pure . Return $ varExpr t
                  in
                    checkStmt' (IfElse boolExpr body body) == pure t
            , testCase "rejects incompatible return types" $
                checkStmt' (IfElse boolExpr [Return intExpr] [Return boolExpr])
                  @?= Left 1
            ]
        , testProperty "while do done propagates return types" $
            \t -> checkStmt' (While boolExpr [Return $ varExpr t]) == pure t
        , testProperty "begin end propagates return types" $
            \t -> checkStmt' (BeginEnd [Return $ varExpr t]) == pure t
        ]
    , testGroup
        "checkLValue"
        [ testProperty "LVIdent looks up variable types" $
            \t -> checkLValue' (LVIdent t) == pure t
        ]
    , testGroup
        "checkRValue"
        [ testProperty "RVExpr propagates expression types" $
            \t -> checkRValue' (RVExpr $ varExpr t) == pure t
        , testGroup
            "RVArrayLit"
            [ testCase "accepts empty arrays" $
                checkRValue' (RVArrayLit []) @?= pure (BArray BAny)
            , testProperty "accepts arrays of homogeneous expressions" $
                \n t ->
                  let
                    n' = abs n + 1
                    xs = replicate n' $ varExpr t
                  in
                    checkRValue' (RVArrayLit xs) == pure (BArray t)
            , testProperty "rejects arrays of heterogeneous expressions" $
                \n ->
                  let
                    n' = abs n + 1
                    xs = replicate n' intExpr ++ [boolExpr]
                  in
                    checkRValue' (RVArrayLit xs) == Left 1
            ]
        , testProperty "RVNewPair creates correct pair types" $
            \t1 t2 ->
              checkRValue' (RVNewPair (varExpr t1) (varExpr t2))
                == pure (BKnownPair t1 t2)
        , testGroup
            "RVCall"
            [ testCase "accepts calls with parameters of correct types" $
                checkRValue' (RVCall () [intExpr, boolExpr]) @?= pure BBool
            , testCase "rejects calls with parameters of incorrect types" $
                checkRValue' (RVCall () [boolExpr, intExpr]) @?= Left 1
            , testCase "rejects calls with fewer parameters than signature" $
                checkRValue' (RVCall () [intExpr]) @?= Left 1
            , testCase "rejects calls with more parameters than signature" $
                checkRValue' (RVCall () [intExpr, boolExpr, intExpr])
                  @?= Left 1
            ]
        ]
    , testGroup
        "checkPairElem"
        [ testProperty "extracts correct fst type" $
            \t1 t2 ->
              checkPairElem' (FstElem (LVIdent $ BKnownPair t1 t2)) == pure t1
        , testProperty "extracts correct snd type" $
            \t1 t2 ->
              checkPairElem' (SndElem (LVIdent $ BKnownPair t1 t2)) == pure t2
        ]
    ]
