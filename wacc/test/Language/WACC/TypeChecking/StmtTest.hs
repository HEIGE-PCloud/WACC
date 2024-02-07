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

testTypingM :: TypingM () BType a -> Maybe a
testTypingM action = case runTypingM action id fnMap of
  (mx, _, _) -> mx
  where
    fnMap = singleton () (FnType [BInt, BBool] BBool)

checkStmt' :: Stmt () BType -> Maybe BType
checkStmt' = testTypingM . checkStmt

checkLValue' :: LValue BType -> Maybe BType
checkLValue' = testTypingM . checkLValue

checkRValue' :: RValue () BType -> Maybe BType
checkRValue' = testTypingM . checkRValue

intExpr :: Expr BType
intExpr = WAtom $ IntLit 0 undefined

boolExpr :: Expr BType
boolExpr = WAtom $ BoolLit False undefined

varExpr :: BType -> Expr BType
varExpr = WAtom . Ident

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
        [ testCase "ignores skip" $ checkStmt' Skip @?= Just BAny
        , testGroup
            "declaration"
            [ testCase "accepts identical type" $
                checkStmt' (Decl WInt BInt (RVExpr intExpr)) @?= Just BAny
            , testCase "accepts compatible type" $
                checkStmt' (Decl wIntPair bIntPair (RVExpr $ WAtom Null))
                  @?= Just BAny
            , testCase "rejects incompatible type" $
                checkStmt' (Decl WInt BInt (RVExpr $ WAtom Null)) @?= Nothing
            ]
        , testGroup
            "assignment"
            [ testCase "accepts identical type" $
                checkStmt' (Asgn (LVIdent BInt) (RVExpr intExpr)) @?= Just BAny
            , testCase "accepts compatible type" $
                checkStmt' (Asgn (LVIdent bIntPair) (RVExpr $ WAtom Null))
                  @?= Just BAny
            , testCase "rejects incompatible type" $
                checkStmt' (Asgn (LVIdent BInt) (RVExpr $ WAtom Null))
                  @?= Nothing
            ]
        , testProperty "ignores read" $
            \t -> checkStmt' (Read (LVIdent t)) == Just BAny
        , testGroup
            "free"
            [ testProperty "accepts arrays" $
                \t -> checkStmt' (Free (varExpr $ BArray t)) == Just BAny
            , testCase "accepts erased pairs" $
                checkStmt' (Free (varExpr BErasedPair)) @?= Just BAny
            , testProperty "accepts known pairs" $
                \t1 t2 ->
                  checkStmt' (Free (varExpr $ BKnownPair t1 t2)) == Just BAny
            ]
        , testProperty "return type is propagated" $
            \t -> checkStmt' (Return (varExpr t)) == Just t
        , testGroup
            "exit"
            [ testCase "accepts ints" $ checkStmt' (Exit intExpr) @?= Just BAny
            , testCase "rejects bools" $ checkStmt' (Exit boolExpr) @?= Nothing
            ]
        , testProperty "ignores print" $
            \t -> checkStmt' (Print (varExpr t)) == Just BAny
        , testProperty "ignores println" $
            \t -> checkStmt' (PrintLn (varExpr t)) == Just BAny
        , testGroup
            "if then else fi"
            [ testCase "accepts skips" $
                checkStmt' (IfElse boolExpr [Skip] [Skip]) @?= Just BAny
            , testProperty "accepts identical return types" $
                \t ->
                  let
                    body = pure . Return $ varExpr t
                  in
                    checkStmt' (IfElse boolExpr body body) == Just t
            , testCase "rejects incompatible return types" $
                checkStmt' (IfElse boolExpr [Return intExpr] [Return boolExpr])
                  @?= Nothing
            ]
        , testProperty "while do done propagates return types" $
            \t -> checkStmt' (While boolExpr [Return $ varExpr t]) == Just t
        , testProperty "begin end propagates return types" $
            \t -> checkStmt' (BeginEnd [Return $ varExpr t]) == Just t
        ]
    , testGroup
        "checkLValue"
        [ testProperty "LVIdent looks up variable types" $
            \t -> checkLValue' (LVIdent t) == Just t
        ]
    , testGroup
        "checkRValue"
        [ testProperty "RVExpr propagates expression types" $
            \t -> checkRValue' (RVExpr $ varExpr t) == Just t
        , testGroup
            "RVArrayLit"
            [ testCase "accepts empty arrays" $
                checkRValue' (RVArrayLit []) @?= Just (BArray BAny)
            , testProperty "accepts arrays of homogeneous expressions" $
                \n t ->
                  let
                    n' = abs n + 1
                    xs = replicate n' $ varExpr t
                  in
                    checkRValue' (RVArrayLit xs) == Just (BArray t)
            , testProperty "rejects arrays of heterogeneous expressions" $
                \n ->
                  let
                    n' = abs n + 1
                    xs = replicate n' intExpr ++ [boolExpr]
                  in
                    checkRValue' (RVArrayLit xs) == Nothing
            ]
        , testProperty "RVNewPair creates correct pair types" $
            \t1 t2 ->
              checkRValue' (RVNewPair (varExpr t1) (varExpr t2))
                == Just (BKnownPair t1 t2)
        , testGroup
            "RVCall"
            [ testCase "accepts calls with parameters of correct types" $
                checkRValue' (RVCall () [intExpr, boolExpr]) @?= Just BBool
            , testCase "rejects calls with parameters of incorrect types" $
                checkRValue' (RVCall () [boolExpr, intExpr]) @?= Nothing
            , testCase "rejects calls with fewer parameters than signature" $
                checkRValue' (RVCall () [intExpr]) @?= Nothing
            , testCase "rejects calls with more parameters than signature" $
                checkRValue' (RVCall () [intExpr, boolExpr, intExpr])
                  @?= Nothing
            ]
        ]
    ]
