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
import Language.WACC.TypeChecking.Class
import Language.WACC.TypeChecking.State
import Language.WACC.TypeChecking.Stmt ()
import Test

testTypingM :: TypingM () BType a -> Either Int a
testTypingM action = case runTypingM action id fnMap of
  (Just x, _, []) -> Right x
  (_, _, es) -> Left $ length es
  where
    fnMap = singleton () (FnType [BInt, BBool] BBool)

checkStmt' :: Stmt Pos () BType -> Either Int BType
checkStmt' = testTypingM . fnCheck

checkLValue' :: LValue Pos BType -> Either Int BType
checkLValue' = testTypingM . check

checkRValue' :: RValue Pos () BType -> Either Int BType
checkRValue' = testTypingM . fnCheck

checkPairElem' :: PairElem Pos BType -> Either Int BType
checkPairElem' = testTypingM . check

intExpr :: Expr Pos BType
intExpr = WAtom (IntLit 0 undefined) undefined

boolExpr :: Expr Pos BType
boolExpr = WAtom (BoolLit False undefined) undefined

nullExpr :: Expr Pos BType
nullExpr = WAtom (Null undefined) undefined

varExpr :: BType -> Expr Pos BType
varExpr = flip WAtom undefined . (`Ident` undefined)

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
        [ testCase "ignores skip" $ checkStmt' (Skip undefined) @?= pure BAny
        , testGroup
            "declaration"
            [ testCase "accepts identical type" $
                checkStmt' (Decl WInt BInt (RVExpr intExpr undefined) undefined)
                  @?= pure BAny
            , testCase "accepts compatible type" $
                checkStmt'
                  ( Decl
                      wIntPair
                      bIntPair
                      (RVExpr nullExpr undefined)
                      undefined
                  )
                  @?= pure BAny
            , testCase "rejects incompatible type" $
                checkStmt'
                  (Decl WInt BInt (RVExpr nullExpr undefined) undefined)
                  @?= Left 1
            ]
        , testGroup
            "assignment"
            [ testCase "accepts identical type" $
                checkStmt'
                  ( Asgn
                      (LVIdent BInt undefined)
                      (RVExpr intExpr undefined)
                      undefined
                  )
                  @?= pure BAny
            , testCase "accepts compatible type" $
                checkStmt'
                  ( Asgn
                      (LVIdent bIntPair undefined)
                      (RVExpr nullExpr undefined)
                      undefined
                  )
                  @?= pure BAny
            , testCase "rejects incompatible type" $
                checkStmt'
                  ( Asgn
                      (LVIdent BInt undefined)
                      (RVExpr nullExpr undefined)
                      undefined
                  )
                  @?= Left 1
            ]
        , testGroup
            "read"
            [ testCase "accepts ints" $
                checkStmt' (Read (LVIdent BInt undefined) undefined)
                  @?= pure BAny
            , testCase "accepts chars" $
                checkStmt' (Read (LVIdent BChar undefined) undefined)
                  @?= pure BAny
            , testCase "rejects bools" $
                checkStmt' (Read (LVIdent BBool undefined) undefined)
                  @?= Left 1
            ]
        , testGroup
            "free"
            [ testProperty "accepts arrays" $
                \t ->
                  checkStmt' (Free (varExpr $ BArray t) undefined) == pure BAny
            , testCase "accepts erased pairs" $
                checkStmt' (Free (varExpr BErasedPair) undefined) @?= pure BAny
            , testProperty "accepts known pairs" $
                \t1 t2 ->
                  checkStmt' (Free (varExpr $ BKnownPair t1 t2) undefined)
                    == pure BAny
            ]
        , testProperty "return type is propagated" $
            \t -> checkStmt' (Return (varExpr t) undefined) == pure t
        , testGroup
            "exit"
            [ testCase "accepts ints" $
                checkStmt' (Exit intExpr undefined) @?= pure BAny
            , testCase "rejects bools" $
                checkStmt' (Exit boolExpr undefined) @?= Left 1
            ]
        , testProperty "ignores print" $
            \t -> checkStmt' (Print (varExpr t) undefined) == pure BAny
        , testProperty "ignores println" $
            \t -> checkStmt' (PrintLn (varExpr t) undefined) == pure BAny
        , testGroup
            "if then else fi"
            [ testCase "accepts skips" $
                checkStmt'
                  (IfElse boolExpr [Skip undefined] [Skip undefined] undefined)
                  @?= pure BAny
            , testProperty "accepts identical return types" $
                \t ->
                  let
                    body = [Return (varExpr t) undefined]
                  in
                    checkStmt' (IfElse boolExpr body body undefined) == pure t
            , testCase "rejects incompatible return types" $
                checkStmt'
                  ( IfElse
                      boolExpr
                      [Return intExpr undefined]
                      [Return boolExpr undefined]
                      undefined
                  )
                  @?= Left 1
            ]
        , testProperty "while do done propagates return types" $
            \t ->
              checkStmt'
                (While boolExpr [Return (varExpr t) undefined] undefined)
                == pure t
        , testProperty "begin end propagates return types" $
            \t ->
              checkStmt' (BeginEnd [Return (varExpr t) undefined] undefined)
                == pure t
        ]
    , testGroup
        "checkLValue"
        [ testProperty "LVIdent looks up variable types" $
            \t -> checkLValue' (LVIdent t undefined) == pure t
        ]
    , testGroup
        "checkRValue"
        [ testProperty "RVExpr propagates expression types" $
            \t -> checkRValue' (RVExpr (varExpr t) undefined) == pure t
        , testGroup
            "RVArrayLit"
            [ testCase "accepts empty arrays" $
                checkRValue' (RVArrayLit [] undefined) @?= pure (BArray BAny)
            , testProperty "accepts arrays of homogeneous expressions" $
                \n wt ->
                  let
                    n' = abs n + 1
                    t = fix wt
                    xs = replicate n' $ varExpr t
                  in
                    checkRValue' (RVArrayLit xs undefined) == pure (BArray t)
            , testProperty "rejects arrays of heterogeneous expressions" $
                \n ->
                  let
                    n' = abs n + 1
                    xs = replicate n' intExpr ++ [boolExpr]
                  in
                    checkRValue' (RVArrayLit xs undefined) == Left 1
            ]
        , testProperty "RVNewPair creates correct pair types" $
            \t1 t2 ->
              checkRValue' (RVNewPair (varExpr t1) (varExpr t2) undefined)
                == pure (BKnownPair t1 t2)
        , testGroup
            "RVCall"
            [ testCase "accepts calls with parameters of correct types" $
                checkRValue' (RVCall () [intExpr, boolExpr] undefined)
                  @?= pure BBool
            , testCase "rejects calls with parameters of incorrect types" $
                checkRValue' (RVCall () [boolExpr, intExpr] undefined)
                  @?= Left 1
            , testCase "rejects calls with fewer parameters than signature" $
                checkRValue' (RVCall () [intExpr] undefined) @?= Left 1
            , testCase "rejects calls with more parameters than signature" $
                checkRValue' (RVCall () [intExpr, boolExpr, intExpr] undefined)
                  @?= Left 1
            ]
        ]
    , testGroup
        "checkPairElem"
        [ testProperty "extracts correct fst type" $
            \t1 t2 ->
              checkPairElem'
                (FstElem (LVIdent (BKnownPair t1 t2) undefined) undefined)
                == pure t1
        , testProperty "extracts correct snd type" $
            \t1 t2 ->
              checkPairElem'
                (SndElem (LVIdent (BKnownPair t1 t2) undefined) undefined)
                == pure t2
        ]
    ]
