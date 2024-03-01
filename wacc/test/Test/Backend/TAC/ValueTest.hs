{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Backend.TAC.ValueTest (lvalueTestGroup, rvalueTestGroup) where

import Data.Char (ord)
import Data.DList (DList)
import Language.WACC.AST
  ( ArrayIndex (..)
  , Expr (WAtom)
  , LValue (..)
  , PairElem (..)
  , RValue (..)
  , WAtom (..)
  )
import Language.WACC.TAC.Class
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TAC.Value
import Language.WACC.TypeChecking
import Test

testTACM :: TACM Int Int a -> a
testTACM = fst . runTACM 0

toTAC' :: RValue Int Int BType -> DList (TAC Int Int)
toTAC' = testTACM . (*> collectTACs) . fnToTAC

lvToTAC' :: LValue Int BType -> LVMode Int Int -> DList (TAC Int Int)
lvToTAC' lv mode = testTACM $ lvToTAC lv mode *> collectTACs

intLit :: (Integral a) => a -> Expr Int BType
intLit x = WAtom (IntLit (toInteger x) BInt) BInt

lvStore :: Int -> LVMode Int Int
lvStore = LVStore . flip RVExpr BInt . intLit

temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Int
temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _ = Temp <$> [0 ..]

temp7, temp8, temp9, temp10, temp11 :: Var Int
temp7 : temp8 : temp9 : temp10 : temp11 : _ = Temp <$> [7 ..]

testPairProperty
  :: (Testable prop)
  => String
  -> ((LValue ident ann -> ann -> PairElem ident ann) -> Int -> prop)
  -> TestTree
testPairProperty propName mkProp =
  testGroup
    propName
    [ testProperty "FstElem" (mkProp FstElem 0)
    , testProperty "SndElem" (mkProp SndElem 8)
    ]

lvalueTestGroup :: TestTree
lvalueTestGroup =
  testGroup
    "lvalues"
    [ testGroup
        "identifiers"
        [ testProperty "loading the same identifier generates no instructions" $
            \v ->
              testTACM
                (lvToTAC (LVIdent v BInt) LVLoad `into` Var v *> collectTACs)
                === []
        , testProperty "loading a different identifier uses Move" $ \v ->
            lvToTAC' (LVIdent v BInt) LVLoad === [Move temp0 (Var v)]
        , testProperty "reading into an identifier uses Read" $ \v ->
            lvToTAC' (LVIdent v BInt) LVRead === [Read (Var v) FInt]
        , testProperty "storing into an identifier generates no instructions" $
            \v i ->
              lvToTAC' (LVIdent v BInt) (lvStore i) === [LoadCI (Var v) i]
        ]
    , testGroup
        "array elements"
        [ testProperty "loading an array element uses LoadM" $ \v i1 i2 ->
            lvToTAC'
              ( LVArrayElem
                  (ArrayIndex v [intLit i1, intLit i2] (BArray $ BArray BInt))
                  BInt
              )
              LVLoad
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
        , testProperty "reading into an array element uses Read" $ \v i1 i2 ->
            lvToTAC'
              ( LVArrayElem
                  (ArrayIndex v [intLit i1, intLit i2] (BArray $ BArray BInt))
                  BInt
              )
              LVRead
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
                  , Read temp11 FInt
                  , Store temp6 temp10 temp11 FInt
                  ]
        , testProperty "storing into an array element uses Store" $
            \v i1 i2 x ->
              lvToTAC'
                ( LVArrayElem
                    (ArrayIndex v [intLit i1, intLit i2] (BArray $ BArray BInt))
                    BInt
                )
                (lvStore x)
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
                    , LoadCI temp11 x
                    , Store temp6 temp10 temp11 FInt
                    ]
        ]
    , testGroup
        "pair elements"
        [ testPairProperty "loading a pair element uses LoadM" $
            \mkPairElem offset v ->
              lvToTAC'
                ( LVPairElem
                    (mkPairElem (LVIdent v $ BKnownPair BInt BInt) BInt)
                    BInt
                )
                LVLoad
                === [ Move temp1 (Var v)
                    , LoadCI temp2 offset
                    , LoadM temp0 temp1 temp2 FInt
                    ]
        , testPairProperty "reading into a pair element uses Read" $
            \mkPairElem offset v ->
              lvToTAC'
                ( LVPairElem
                    (mkPairElem (LVIdent v $ BKnownPair BInt BInt) BInt)
                    BInt
                )
                LVRead
                === [ Move temp1 (Var v)
                    , LoadCI temp2 offset
                    , Read temp3 FInt
                    , Store temp1 temp2 temp3 FInt
                    ]
        , testPairProperty "storing into a pair element uses Store" $
            \mkPairElem offset v x ->
              lvToTAC'
                ( LVPairElem
                    (mkPairElem (LVIdent v $ BKnownPair BInt BInt) BInt)
                    BInt
                )
                (lvStore x)
                === [ Move temp1 (Var v)
                    , LoadCI temp2 offset
                    , LoadCI temp3 x
                    , Store temp1 temp2 temp3 FInt
                    ]
        ]
    ]

testArrayElemSizes
  :: (Arbitrary a, Show a)
  => String
  -> BType
  -> (a -> BType -> WAtom Int BType)
  -> (Var Int -> a -> TAC Int Int)
  -> Int
  -> TestTree
testArrayElemSizes tName bt mkAtom mkTAC tSize =
  testProperty (unwords [tName, "array elements use", show tSize ++ "B"]) $
    \e1 e2 ->
      toTAC'
        (RVArrayLit (flip WAtom bt . flip mkAtom bt <$> [e1, e2]) (BArray bt))
        === [ LoadCI temp1 (4 + 2 * tSize)
            , LoadCI temp2 2
            , LoadCI temp3 0
            , Malloc temp0 temp1
            , Store temp0 temp3 temp2 FInt
            , mkTAC temp4 e1
            , LoadCI temp5 4
            , Store temp0 temp5 temp4 ft
            , mkTAC temp6 e2
            , LoadCI temp7 (4 + tSize)
            , Store temp0 temp7 temp6 ft
            ]
  where
    ft = flatten bt

rvalueTestGroup :: TestTree
rvalueTestGroup =
  testGroup
    "rvalues"
    [ testGroup
        "expressions"
        [ testProperty "int literals are loaded using LoadCI" $ \i ->
            toTAC' (RVExpr (intLit i) BInt) === [LoadCI temp0 i]
        , testProperty "string literals are loaded using LoadCS" $ \s ->
            toTAC' (RVExpr (WAtom (StringLit s BString) BString) BString)
              === [LoadCS temp0 s]
        , testProperty "identifiers generate no instructions" $ \v ->
            toTAC' (RVExpr (WAtom (Ident v BInt) BInt) BInt)
              === [Move temp0 (Var v)]
        ]
    , testGroup
        "array literals"
        [ testCase "empty arrays are heap-allocated zeroes" $
            toTAC' (RVArrayLit [] (BArray BInt))
              @?= [ LoadCI temp1 4
                  , LoadCI temp2 0
                  , LoadCI temp3 0
                  , Malloc temp0 temp1
                  , Store temp0 temp3 temp2 FInt
                  ]
        , testArrayElemSizes "int" BInt (IntLit . toInteger) LoadCI 4
        , testArrayElemSizes
            @Bool
            "bool"
            BBool
            (IntLit . toInteger . fromEnum)
            (\v b -> LoadCI v $ fromEnum b)
            1
        , testArrayElemSizes
            "char"
            BChar
            (IntLit . toInteger . ord)
            (\v c -> LoadCI v $ ord c)
            1
        , testArrayElemSizes "string" BString StringLit LoadCS 8
        , testArrayElemSizes
            "pair"
            BErasedPair
            (const Null)
            (\v () -> LoadCI v 0)
            8
        ]
    , testProperty "new pairs allocate 16 bytes" $ \i1 i2 ->
        toTAC' (RVNewPair (intLit i1) (intLit i2) (BKnownPair BInt BInt))
          === [ LoadCI temp1 i1
              , LoadCI temp2 i2
              , LoadCI temp3 0
              , LoadCI temp4 8
              , LoadCI temp5 16
              , Malloc temp0 temp5
              , Store temp0 temp3 temp1 FInt
              , Store temp0 temp4 temp2 FInt
              ]
    , testPairProperty "pair elements are loaded with correct offsets" $
        \mkPairElem offset v ->
          toTAC'
            ( RVPairElem
                (mkPairElem (LVIdent v (BKnownPair BInt BInt)) BInt)
                BInt
            )
            === [ Move temp1 (Var v)
                , LoadCI temp2 offset
                , LoadM temp0 temp1 temp2 FInt
                ]
    , testGroup
        "function calls"
        [ testProperty "nullary function calls are executed using Call" $ \f ->
            toTAC' (RVCall f [] BInt) === [Call temp0 f []]
        , testProperty "function arguments are evaluated before the Call" $
            \f i1 i2 ->
              toTAC' (RVCall f [intLit i1, intLit i2] BInt)
                === [ LoadCI temp1 i1
                    , LoadCI temp2 i2
                    , Call temp0 f [temp1, temp2]
                    ]
        ]
    ]
