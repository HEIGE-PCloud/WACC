{-# LANGUAGE OverloadedLists #-}

module Test.Backend.TAC.ExprTest (exprTestGroup) where

import Data.Char (ord)
import Language.WACC.AST (Expr (WAtom), WAtom (..), WType (..))
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking
import Test
import Prelude hiding (GT, LT)

testTACM :: TACM Int Int a -> a
testTACM = runTACM 0

toTAC' :: Expr Int BType -> ExprTACs Int Int
toTAC' = testTACM . toTAC

varExpr :: Int -> BType -> Expr Int BType
varExpr v t = WAtom (Ident v t) t

temp0 :: Var Int
temp0 = Temp 0

temp1 :: Var Int
temp1 = Temp 1

testBinOp
  :: String
  -> (Expr Int BType -> Expr Int BType -> BType -> Expr Int BType)
  -> BinOp
  -> BType
  -> BType
  -> BType
  -> TestTree
testBinOp name astOp tacOp t1 t2 t3 =
  testProperty (unwords [name, "generates", show tacOp]) $ \v1 v2 ->
    toTAC' (astOp (varExpr v1 t1) (varExpr v2 t2) t3)
      == [BinInstr temp0 (Var v1) tacOp (Var v2)] temp0

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
        , testProperty "identifiers generate no instructions" $ \v ->
            toTAC' (varExpr v BInt) == [] (Var v)
        ]
    , testGroup
        "unary expressions"
        [ testProperty "!_ generates Not" $ \v ->
            toTAC' (AST.Not (varExpr v BBool) BBool)
              == [UnInstr temp0 Not (Var v)] temp0
        , testProperty "-_ generates Negate" $ \v ->
            toTAC' (AST.Negate (varExpr v BInt) BInt)
              == [UnInstr temp0 Negate (Var v)] temp0
        , testProperty "len loads length stored at base address" $ \v ->
            toTAC' (AST.Len (varExpr v $ BArray BInt) BInt)
              == [LoadCI temp0 0, LoadM temp1 (Var v) temp0 WInt] temp1
        , testProperty "ord generates no instructions" $ \v ->
            toTAC' (AST.Ord (varExpr v BChar) BInt) == [] (Var v)
        , testProperty "chr generates a bounds check" $ \v ->
            toTAC' (AST.Chr (varExpr v BInt) BChar)
              == [CheckBounds 0 (Var v) 127] (Var v)
        ]
    , testGroup
        "binary expressions"
        [ testBinOp "_*_" AST.Mul Mul BInt BInt BInt
        , testBinOp "_/_" AST.Div Div BInt BInt BInt
        , testBinOp "_%_" AST.Mod Mod BInt BInt BInt
        , testBinOp "_+_" AST.Add Add BInt BInt BInt
        , testBinOp "_-_" AST.Sub Sub BInt BInt BInt
        , testBinOp "_>_" AST.GT GT BInt BInt BBool
        , testBinOp "_>=_" AST.GTE GTE BInt BInt BBool
        , testBinOp "_<_" AST.LT LT BInt BInt BBool
        , testBinOp "_<=_" AST.LTE LTE BInt BInt BBool
        , testBinOp "_&&_" AST.And And BBool BBool BBool
        , testBinOp "_||_" AST.Or Or BBool BBool BBool
        , testBinOp "_==_" AST.Eq Eq BInt BInt BBool
        , testBinOp "_!=_" AST.Ineq Ineq BInt BInt BBool
        ]
    ]
