{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-type-defaults #-}

module Test.Backend.TAC.ExprTest (exprTestGroup) where

import Data.Char (ord)
import Data.DList (DList)
import Language.WACC.AST (ArrayIndex (..), Expr (WAtom), WAtom (..))
import qualified Language.WACC.AST as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.Expr ()
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TypeChecking
import Test
import Prelude hiding (GT, LT)

testTACM :: TACM Int Int a -> a
testTACM = fst . runTACM 0

toTAC' :: Expr Int BType -> DList (TAC Int Int)
toTAC' = testTACM . (*> collectTACs) . toTAC

intLit :: (Integral a) => a -> Expr Int BType
intLit x = WAtom (IntLit (toInteger x) BInt) BInt

varExpr :: Int -> BType -> Expr Int BType
varExpr v t = WAtom (Ident v t) t

temp1, temp2, temp3, temp4, temp5, temp6, temp7 :: Var Int
temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : temp7 : _ = Temp <$> [1 ..]

temp8, temp9, temp10, temp11, temp12, temp13 :: Var Int
temp8 : temp9 : temp10 : temp11 : temp12 : temp13 : _ = Temp <$> [8 ..]

testIndexScaling :: String -> BType -> Int -> TestTree
testIndexScaling tName bt tSize =
  testProperty (unwords [tName, "array indices are scaled by", show tSize]) $
    \v i ->
      toTAC' (WAtom (ArrayElem (ArrayIndex v [intLit i] (BArray bt)) bt) bt)
        === [ LoadCI temp1 0
            , LoadCI temp2 4
            , LoadCI temp3 tSize
            , LoadCI temp4 i
            , LoadM temp5 (Var v) temp1 FInt
            , CheckBounds temp4 temp5 ArrayIndexCheck
            , BinInstr temp6 temp4 Mul temp3
            , BinInstr temp7 temp6 Add temp2
            , LoadM defaultTarget (Var v) temp7 (flatten bt)
            ]

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
      === [ Move temp1 (Var v1)
          , Move temp2 (Var v2)
          , BinInstr defaultTarget temp1 tacOp temp2
          ]

exprTestGroup :: TestTree
exprTestGroup =
  testGroup
    "expressions"
    [ testGroup
        "atomic expressions"
        [ testGroup
            "literals"
            [ testProperty "int literals are loaded using LoadCI" $ \i ->
                toTAC' (intLit i) === [LoadCI defaultTarget i]
            , testCase "false is 0" $
                toTAC' (WAtom (BoolLit False BBool) BBool) @?= [LoadCI defaultTarget 0]
            , testCase "true is 1" $
                toTAC' (WAtom (BoolLit True BBool) BBool) @?= [LoadCI defaultTarget 1]
            , testProperty "char literals are loaded using LoadCI" $ \c ->
                toTAC' (WAtom (CharLit c BChar) BChar)
                  === [LoadCI defaultTarget $ ord c]
            , testProperty "string literals are loaded using LoadCS" $ \s ->
                toTAC' (WAtom (StringLit s BString) BString)
                  === [LoadCS defaultTarget s]
            , testCase "null is 0" $
                toTAC' (WAtom (Null BErasedPair) BErasedPair)
                  @?= [LoadCI defaultTarget 0]
            ]
        , testGroup
            "identifiers"
            [ testProperty "same identifier generates no instructions" $ \v ->
                testTACM (toTAC (varExpr v BInt) `into` Var v *> collectTACs)
                  === []
            , testProperty "distinct identifiers are copied using Move" $ \v ->
                toTAC' (varExpr v BInt) === [Move defaultTarget (Var v)]
            ]
        , testGroup
            "array indexing"
            [ testIndexScaling "int" BInt 4
            , testIndexScaling "bool" BBool 1
            , testIndexScaling "char" BChar 1
            , testIndexScaling "string" BString 8
            , testIndexScaling "pair" BErasedPair 8
            , testIndexScaling "array" (BArray BInt) 8
            , testProperty "indices are applied from left to right" $
                \v i1 i2 ->
                  toTAC'
                    ( WAtom
                        ( ArrayElem
                            ( ArrayIndex
                                v
                                [intLit i1, intLit i2]
                                (BArray $ BArray BInt)
                            )
                            BInt
                        )
                        BInt
                    )
                    === [ LoadCI temp1 0
                        , LoadCI temp2 4
                        , LoadCI temp3 8
                        , LoadCI temp4 i1
                        , LoadM temp5 (Var v) temp1 FInt
                        , CheckBounds temp4 temp5 ArrayIndexCheck
                        , BinInstr temp6 temp4 Mul temp3
                        , BinInstr temp7 temp6 Add temp2
                        , LoadM temp8 (Var v) temp7 FPtr
                        , LoadCI temp9 4
                        , LoadCI temp10 i2
                        , LoadM temp11 temp8 temp1 FInt
                        , CheckBounds temp10 temp11 ArrayIndexCheck
                        , BinInstr temp12 temp10 Mul temp9
                        , BinInstr temp13 temp12 Add temp2
                        , LoadM defaultTarget temp8 temp13 FInt
                        ]
            ]
        ]
    , testGroup
        "unary expressions"
        [ testProperty "!_ generates Not" $ \v ->
            toTAC' (AST.Not (varExpr v BBool) BBool)
              === [Move temp1 (Var v), UnInstr defaultTarget Not temp1]
        , testProperty "-_ generates Negate" $ \v ->
            toTAC' (AST.Negate (varExpr v BInt) BInt)
              === [Move temp1 (Var v), UnInstr defaultTarget Negate temp1]
        , testProperty "len loads length stored at base address" $ \v ->
            toTAC' (AST.Len (varExpr v $ BArray BInt) BInt)
              === [ Move temp1 (Var v)
                  , LoadCI temp2 0
                  , LoadM defaultTarget temp1 temp2 FInt
                  ]
        , testProperty "ord generates no instructions" $ \v ->
            toTAC' (AST.Ord (varExpr v BChar) BInt) === [Move defaultTarget (Var v)]
        , testProperty "chr generates a bounds check" $ \v ->
            toTAC' (AST.Chr (varExpr v BInt) BChar)
              === [ Move defaultTarget (Var v)
                  , LoadCI temp1 128
                  , CheckBounds defaultTarget temp1 ChrCheck
                  ]
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
