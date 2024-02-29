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

temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Int
temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _ = Temp <$> [0 ..]

temp7, temp8, temp9, temp10 :: Var Int
temp7 : temp8 : temp9 : temp10 : _ = Temp <$> [7 ..]

testIndexScaling :: String -> BType -> Int -> TestTree
testIndexScaling tName bt tSize =
  testProperty (unwords [tName, "array indices are scaled by", show tSize]) $
    \v i ->
      toTAC' (WAtom (ArrayElem (ArrayIndex v [intLit i] (BArray bt)) bt) bt)
        === [ LoadCI temp1 4
            , LoadCI temp2 tSize
            , LoadCI temp3 i
            , BinInstr temp4 temp3 Mul temp2
            , BinInstr temp5 temp4 Add temp1
            , LoadM temp0 (Var v) temp5 (flatten bt)
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
          , BinInstr temp0 temp1 tacOp temp2
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
                toTAC' (intLit i) === [LoadCI temp0 i]
            , testCase "false is 0" $
                toTAC' (WAtom (BoolLit False BBool) BBool) @?= [LoadCI temp0 0]
            , testCase "true is 1" $
                toTAC' (WAtom (BoolLit True BBool) BBool) @?= [LoadCI temp0 1]
            , testProperty "char literals are loaded using LoadCI" $ \c ->
                toTAC' (WAtom (CharLit c BChar) BChar)
                  === [LoadCI temp0 $ ord c]
            , testProperty "string literals are loaded using LoadCS" $ \s ->
                toTAC' (WAtom (StringLit s BString) BString)
                  === [LoadCS temp0 s]
            , testCase "null is 0" $
                toTAC' (WAtom (Null BErasedPair) BErasedPair)
                  @?= [LoadCI temp0 0]
            ]
        , testProperty "identifiers are copied using Move" $ \v ->
            toTAC' (varExpr v BInt) === [Move temp0 (Var v)]
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
                    === [ LoadCI temp1 4
                        , LoadCI temp2 8
                        , LoadCI temp3 i1
                        , BinInstr temp4 temp3 Mul temp2
                        , BinInstr temp5 temp4 Add temp1
                        , LoadM temp6 (Var v) temp5 FPtr
                        , LoadCI temp7 4
                        , LoadCI temp8 i2
                        , BinInstr temp9 temp8 Mul temp7
                        , BinInstr temp10 temp9 Add temp1
                        , LoadM temp0 temp6 temp10 FInt
                        ]
            ]
        ]
    , testGroup
        "unary expressions"
        [ testProperty "!_ generates Not" $ \v ->
            toTAC' (AST.Not (varExpr v BBool) BBool)
              === [Move temp1 (Var v), UnInstr temp0 Not temp1]
        , testProperty "-_ generates Negate" $ \v ->
            toTAC' (AST.Negate (varExpr v BInt) BInt)
              === [Move temp1 (Var v), UnInstr temp0 Negate temp1]
        , testProperty "len loads length stored at base address" $ \v ->
            toTAC' (AST.Len (varExpr v $ BArray BInt) BInt)
              === [ Move temp1 (Var v)
                  , LoadCI temp2 0
                  , LoadM temp0 temp1 temp2 FInt
                  ]
        , testProperty "ord generates no instructions" $ \v ->
            toTAC' (AST.Ord (varExpr v BChar) BInt) === [Move temp0 (Var v)]
        , testProperty "chr generates a bounds check" $ \v ->
            toTAC' (AST.Chr (varExpr v BInt) BChar)
              === [Move temp0 (Var v), CheckBounds 0 temp0 127]
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
