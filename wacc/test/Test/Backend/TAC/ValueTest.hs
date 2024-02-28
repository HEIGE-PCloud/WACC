{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Backend.TAC.ValueTest (rvalueTestGroup) where

import Data.Char (ord)
import Data.DList (DList)
import Language.WACC.AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.FType
import Language.WACC.TAC.State
import Language.WACC.TAC.TAC
import Language.WACC.TAC.Value ()
import Language.WACC.TypeChecking
import Test

testTACM :: TACM Int Int a -> a
testTACM = fst . runTACM 0

toTAC' :: RValue Int Int BType -> DList (TAC Int Int)
toTAC' = testTACM . (*> collectTACs) . fnToTAC

intLit :: (Integral a) => a -> Expr Int BType
intLit x = WAtom (IntLit (toInteger x) BInt) BInt

temp0, temp1, temp2, temp3, temp4, temp5, temp6 :: Var Int
temp0 : temp1 : temp2 : temp3 : temp4 : temp5 : temp6 : _ = Temp <$> [0 ..]

temp7 :: Var Int
temp7 = Temp 7

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
        == [ LoadCI temp1 (4 + 2 * tSize)
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
            toTAC' (RVExpr (intLit i) BInt) == [LoadCI temp0 i]
        , testProperty "string literals are loaded using LoadCS" $ \s ->
            toTAC' (RVExpr (WAtom (StringLit s BString) BString) BString)
              == [LoadCS temp0 s]
        , testProperty "identifiers generate no instructions" $ \v ->
            toTAC' (RVExpr (WAtom (Ident v BInt) BInt) BInt) == []
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
          == [ LoadCI temp1 i1
             , LoadCI temp2 i2
             , LoadCI temp3 0
             , LoadCI temp4 8
             , LoadCI temp5 16
             , Malloc temp0 temp5
             , Store temp0 temp3 temp1 FInt
             , Store temp0 temp4 temp2 FInt
             ]
    , testGroup
        "pair elements"
        [ testProperty "first element is loaded with offset 0" $ \v ->
            toTAC'
              ( RVPairElem
                  (FstElem (LVIdent v (BKnownPair BInt BInt)) BInt)
                  BInt
              )
              == [LoadCI temp2 0, LoadM temp0 temp1 temp2 FInt]
        , testProperty "second element is loaded with offset 8" $ \v ->
            toTAC'
              ( RVPairElem
                  (SndElem (LVIdent v (BKnownPair BInt BInt)) BInt)
                  BInt
              )
              == [LoadCI temp2 8, LoadM temp0 temp1 temp2 FInt]
        ]
    , testGroup
        "function calls"
        [ testProperty "nullary function calls are executed using Call" $ \f ->
            toTAC' (RVCall f [] BInt) == [Call temp0 (Label f) []]
        , testProperty "function arguments are evaluated before the Call" $
            \f i1 i2 ->
              toTAC' (RVCall f [intLit i1, intLit i2] BInt)
                == [ LoadCI temp1 i1
                   , LoadCI temp2 i2
                   , Call temp0 (Label f) [temp1, temp2]
                   ]
        ]
    ]
