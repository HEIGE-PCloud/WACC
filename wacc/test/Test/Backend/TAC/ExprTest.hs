{-# LANGUAGE OverloadedLists #-}

module Test.Backend.TAC.ExprTest (exprTestGroup) where

import Data.Char (ord)
import Language.WACC.AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking
import Test

testTACM :: TACM Int Int a -> a
testTACM = runTACM 0

toTAC' :: Expr Int BType -> ExprTACs Int Int
toTAC' = testTACM . toTAC

temp0 :: Var Int
temp0 = Temp 0

exprTestGroup :: TestTree
exprTestGroup =
  testGroup
    "expressions"
    [ testGroup
        "atomic expressions"
        [ testGroup
            "literals"
            [ testProperty "int literals are loaded using LoadCI" $ \i ->
                toTAC' (WAtom (IntLit (toInteger i) BInt) BInt)
                  == [LoadCI temp0 i] temp0
            , testCase "false is 0" $
                toTAC' (WAtom (BoolLit False BBool) BBool)
                  @?= [LoadCI temp0 0] temp0
            , testCase "true is 1" $
                toTAC' (WAtom (BoolLit True BBool) BBool)
                  @?= [LoadCI temp0 1] temp0
            , testProperty "char literals are loaded using LoadCI" $ \c ->
                toTAC' (WAtom (CharLit c BChar) BChar)
                  == [LoadCI temp0 $ ord c] temp0
            , testProperty "string literals are loaded using LoadCS" $ \s ->
                toTAC' (WAtom (StringLit s BString) BString)
                  == [LoadCS temp0 s] temp0
            , testCase "null is 0" $
                toTAC' (WAtom (Null BErasedPair) BErasedPair)
                  @?= [LoadCI temp0 0] temp0
            ]
        , testProperty "identifiers generate no instructions" $ \i ->
            toTAC' (WAtom (Ident i BInt) BInt) == [] (Var i)
        ]
    ]
