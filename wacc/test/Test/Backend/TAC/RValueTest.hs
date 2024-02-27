{-# LANGUAGE OverloadedLists #-}

module Test.Backend.TAC.RValueTest (rvalueTestGroup) where

import Data.DList (DList)
import Language.WACC.AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.RValue ()
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking
import Test

testTACM :: TACM Int Int a -> a
testTACM = fst . runTACM 0

toTAC' :: RValue Int Int BType -> DList (TAC Int Int)
toTAC' = testTACM . (*> collectTACs) . fnToTAC

temp0 :: Var Int
temp0 = Temp 0

rvalueTestGroup :: TestTree
rvalueTestGroup =
  testGroup
    "rvalues"
    [ testGroup
        "expressions"
        [ testProperty "int literals are loaded using LoadCI" $ \i ->
            toTAC' (RVExpr (WAtom (IntLit (toInteger i) BInt) BInt) BInt)
              == [LoadCI temp0 i]
        , testProperty "string literals are loaded using LoadCS" $ \s ->
            toTAC' (RVExpr (WAtom (StringLit s BString) BString) BString)
              == [LoadCS temp0 s]
        , testProperty "identifiers generate no instructions" $ \v ->
            toTAC' (RVExpr (WAtom (Ident v BInt) BInt) BInt) == []
        ]
    ]
