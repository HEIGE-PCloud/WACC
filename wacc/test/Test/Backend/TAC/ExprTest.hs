{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-type-defaults #-}

module Test.Backend.TAC.ExprTest (exprTestGroup) where

import Data.Char (ord)
import Data.Functor.Foldable (embed, project)
import Language.WACC.AST (ArrayIndex (..), Expr (WAtom), WAtom (..), WType (..))
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

intLit :: (Integral a) => a -> Expr Int BType
intLit x = WAtom (IntLit (toInteger x) BInt) BInt

varExpr :: Int -> BType -> Expr Int BType
varExpr v t = WAtom (Ident v t) t

temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Int
(temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _) = Temp <$> [0 ..]

temp7, temp8, temp9, temp10, temp11 :: Var Int
(temp7 : temp8 : temp9 : temp10 : temp11 : _) = Temp <$> [7 ..]

testIndexScaling :: String -> WType -> Int -> TestTree
testIndexScaling tName wt tSize =
  testProperty (unwords [tName, "array indices are scaled by", show tSize]) $
    \v i ->
      toTAC' (WAtom (ArrayElem (ArrayIndex v [intLit i] (BArray bt)) bt) bt)
        == [ LoadCI temp0 4
           , LoadCI temp1 tSize
           , LoadCI temp2 i
           , BinInstr temp3 temp2 Mul temp1
           , BinInstr temp4 temp3 Add temp0
           , LoadM temp5 (Var v) temp4 (embed $ WErasedPair <$ project wt)
           ]
          temp5
  where
    bt = fix wt

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
                toTAC' (intLit i) == [LoadCI temp0 i] temp0
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
        , testGroup
            "array indexing"
            [ testIndexScaling "int" WInt 4
            , testIndexScaling "bool" WChar 1
            , testIndexScaling "char" WChar 1
            , testIndexScaling "string" WString 8
            , testIndexScaling "pair" WErasedPair 8
            , testIndexScaling "array" (WArray WInt) 8
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
                    == [ LoadCI temp0 4
                       , LoadCI temp1 8
                       , LoadCI temp2 i1
                       , BinInstr temp3 temp2 Mul temp1
                       , BinInstr temp4 temp3 Add temp0
                       , LoadM temp5 (Var v) temp4 (WArray WErasedPair)
                       , LoadCI temp6 4
                       , LoadCI temp7 4
                       , LoadCI temp8 i2
                       , BinInstr temp9 temp8 Mul temp7
                       , BinInstr temp10 temp9 Add temp6
                       , LoadM temp11 temp5 temp10 WInt
                       ]
                      temp11
            ]
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
