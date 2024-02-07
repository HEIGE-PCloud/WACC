{- AUTOCOLLECT.TEST -}

module Language.WACC.TypeChecking.ExprTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.Foldable (traverse_)
import Language.WACC.AST.Expr
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Expr
import Language.WACC.TypeChecking.State
import Test
import Prelude hiding (GT, LT)

btypes :: [BType]
btypes =
  [ BAny
  , BInt
  , BBool
  , BChar
  , BString
  , BErasedPair
  , BKnownPair BAny BAny
  , BKnownPair BAny BInt
  , BKnownPair BInt BAny
  , BKnownPair BInt BInt
  , BArray BAny
  , BArray BInt
  , BArray (BArray BInt)
  ]

forEachBType :: (BType -> Assertion) -> Assertion
forEachBType = flip traverse_ btypes

checkAtom' :: WAtom BType -> Maybe BType
checkAtom' atom = case runTypingM (checkAtom'' atom) id mempty of
  (mt, _, _) -> mt
  where
    checkAtom'' :: WAtom BType -> TypingM () BType BType
    checkAtom'' = checkAtom

checkExpr' :: Expr BType -> Maybe BType
checkExpr' expr = case runTypingM (checkExpr'' expr) id mempty of
  (mt, _, _) -> mt
  where
    checkExpr'' :: Expr BType -> TypingM () BType BType
    checkExpr'' = checkExpr

intExpr :: Expr BType
intExpr = WAtom $ IntLit 0 undefined

boolExpr :: Expr BType
boolExpr = WAtom $ BoolLit False undefined

charExpr :: Expr BType
charExpr = WAtom $ CharLit 'C'

stringExpr :: Expr BType
stringExpr = WAtom $ StringLit "String"

varExpr :: BType -> Expr BType
varExpr = WAtom . Ident

showType :: BType -> String
showType BAny = "T"
showType BUnknown = "unk"
showType BInt = "int"
showType BBool = "bool"
showType BChar = "char"
showType BString = "string"
showType BErasedPair = "pair"
showType (BKnownPair t1 t2) =
  concat ["pair(", showType t1, ", ", showType t2, ")"]
showType (BArray t) = showType t ++ "[]"

mkBad :: BType -> BType
mkBad BInt = BBool
mkBad _ = BInt

testUnOp :: String -> (Expr BType -> Expr BType) -> BType -> BType -> TestTree
testUnOp name op t ret =
  testGroup
    name
    [ testCase
        (concat ["accepts ", showType t, "s and returns ", showType ret, "s"])
        $ checkExpr' (op $ varExpr t) @?= Just ret
    , testCase ("rejects " ++ showType bad ++ "s") $
        checkExpr' (op $ varExpr bad) @?= Nothing
    ]
  where
    bad = mkBad t

testBinOp
  :: String
  -> (Expr BType -> Expr BType -> Expr BType)
  -> [(BType, BType, BType)]
  -> TestTree
testBinOp name op ts = testGroup name $ ts >>= mkCases
  where
    mkCases (t1, t2, ret) =
      [ testCase
          ( concat
              [ accepts
              , " and returns "
              , showType ret
              , "s"
              ]
          )
          $ checkExpr' (op (varExpr t1) (varExpr t2)) @?= Just ret
      , testCase
          ( concat
              [ "rejects "
              , showType bad1
              , "s (left only)"
              ]
          )
          $ checkExpr' (op (varExpr bad1) (varExpr t2)) @?= Nothing
      , testCase
          ( concat
              [ "rejects "
              , showType bad2
              , "s (right only)"
              ]
          )
          $ checkExpr' (op (varExpr t1) (varExpr bad2)) @?= Nothing
      ]
      where
        accepts
          | t1 == t2 = "accepts " ++ showType t1 ++ "s"
          | otherwise =
              "accepts " ++ showType t1 ++ "s and " ++ showType t2 ++ "s"
        bad1 = mkBad t1
        bad2 = mkBad t2

test :: TestTree
test =
  testGroup
    "unitTests"
    [ testGroup
        "checkAtom"
        [ testGroup
            "literals"
            [ testProperty "int literals are ints" $
                \i -> checkAtom' (IntLit i undefined) == Just BInt
            , testProperty "bool literals are bools" $
                \b -> checkAtom' (BoolLit b undefined) == Just BBool
            , testProperty "char literals are chars" $
                \c -> checkAtom' (CharLit c) == Just BChar
            , testProperty "string literals are strings" $
                \s -> checkAtom' (StringLit s) == Just BString
            , testCase "null is a literal for any pair type" $
                checkAtom' Null @?= Just (BKnownPair BAny BAny)
            ]
        , testCase "looks up variable types" $
            forEachBType (\t -> checkAtom' (Ident t) @?= Just t)
        , testGroup
            "array elements"
            [ testCase "elements of an int[] are ints" $
                checkAtom' (ArrayElem (ArrayIndex (BArray BInt) [intExpr]))
                  @?= Just BInt
            , testCase "elements of an int[][] are int[]s" $
                checkAtom'
                  (ArrayElem (ArrayIndex (BArray (BArray BInt)) [intExpr]))
                  @?= Just (BArray BInt)
            , testCase "elements of elements of an int[][] are ints" $
                checkAtom'
                  ( ArrayElem
                      (ArrayIndex (BArray (BArray BInt)) [intExpr, intExpr])
                  )
                  @?= Just BInt
            , testCase "providing too many indices fails the check" $
                checkAtom'
                  (ArrayElem (ArrayIndex (BArray BInt) [intExpr, intExpr]))
                  @?= Nothing
            , testCase "string indexing is not supported" $
                checkAtom'
                  (ArrayElem (ArrayIndex BString [intExpr]))
                  @?= Nothing
            ]
        ]
    , testGroup
        "checkExpr"
        [ testGroup
            "atoms"
            [ testCase "int atoms are ints" $ checkExpr' intExpr @?= Just BInt
            , testCase "bool atoms are bools" $
                checkExpr' boolExpr @?= Just BBool
            , testCase "char atoms are chars" $
                checkExpr' charExpr @?= Just BChar
            , testCase "string atoms are strings" $
                checkExpr' stringExpr @?= Just BString
            ]
        , testGroup
            "unary operators"
            [ testUnOp "not" Not BBool BBool
            , testUnOp "-_ (negate)" Negate BInt BInt
            , testUnOp "len" Len (BArray BAny) BInt
            , testUnOp "ord" Ord BChar BInt
            , testUnOp "chr" Chr BInt BChar
            ]
        , testGroup
            "binary operators"
            [ testBinOp "_*_ (mul)" Mul [(BInt, BInt, BInt)]
            , testBinOp "_/_ (div)" Div [(BInt, BInt, BInt)]
            , testBinOp "_%_ (mod)" Mod [(BInt, BInt, BInt)]
            , testBinOp "_+_ (add)" Add [(BInt, BInt, BInt)]
            , testBinOp "_-_ (sub)" Sub [(BInt, BInt, BInt)]
            , testBinOp
                "_>_ (gt)"
                GT
                [(BInt, BInt, BBool), (BChar, BChar, BBool)]
            , testBinOp
                "_>=_ (gte)"
                GTE
                [(BInt, BInt, BBool), (BChar, BChar, BBool)]
            , testBinOp
                "_<_ (lt)"
                LT
                [(t, t, BBool) | t <- orderedTypes]
            , testBinOp
                "_<=_ (lte)"
                LTE
                [(t, t, BBool) | t <- orderedTypes]
            , testBinOp
                "_==_ (eq)"
                Eq
                [(t, t, BBool) | t <- btypes, t /= BAny]
            , testBinOp
                "_!=_ (ineq)"
                Eq
                [(t, t, BBool) | t <- btypes, t /= BAny]
            , testBinOp "_&&_ (and)" And [(BBool, BBool, BBool)]
            , testBinOp "_||_ (or)" Or [(BBool, BBool, BBool)]
            ]
        ]
    ]
