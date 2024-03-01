module Test.Frontend.TypeChecker.ExprTest
  ( exprTestGroup
  )
where

import Data.Foldable (traverse_)
import Language.WACC.AST
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Class
import Language.WACC.TypeChecking.Expr ()
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

testTypingM' :: (a -> b) -> TypingM () BType a -> Either Int b
testTypingM' f action = case runTypingM action id mempty of
  (Just x, _, es) | null es -> Right (f x)
  (_, _, es) -> Left $ length es

testTypingM :: (Annotated a) => TypingM () BType a -> Either Int (Ann a)
testTypingM = testTypingM' getAnn

checkAtom' :: WAtom BType Pos -> Either Int BType
checkAtom' = testTypingM . check

checkExpr' :: Expr BType Pos -> Either Int BType
checkExpr' = testTypingM . check

checkArrayIndex' :: ArrayIndex BType Pos -> Either Int BType
checkArrayIndex' = testTypingM' fst . check

intExpr :: Expr BType Pos
intExpr = WAtom (IntLit 0 undefined) undefined

boolExpr :: Expr BType Pos
boolExpr = WAtom (BoolLit False undefined) undefined

charExpr :: Expr BType Pos
charExpr = WAtom (CharLit 'C' undefined) undefined

stringExpr :: Expr BType Pos
stringExpr = WAtom (StringLit "String" undefined) undefined

varExpr :: BType -> Expr BType Pos
varExpr = flip WAtom undefined . (`Ident` undefined)

showType :: BType -> String
showType BAny = "T"
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

testUnOp
  :: String
  -> (Expr BType Pos -> Pos -> Expr BType Pos)
  -> BType
  -> BType
  -> TestTree
testUnOp name op t ret =
  testGroup
    name
    [ testCase
        (concat ["accepts ", showType t, "s and returns ", showType ret, "s"])
        $ checkExpr' (op (varExpr t) undefined) @?= pure ret
    , testCase ("rejects " ++ showType bad ++ "s") $
        checkExpr' (op (varExpr bad) undefined) @?= Left 1
    ]
  where
    bad = mkBad t

testBinOp
  :: String
  -> (Expr BType Pos -> Expr BType Pos -> a -> Expr BType Pos)
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
          $ checkExpr' (op (varExpr t1) (varExpr t2) undefined) @?= pure ret
      , testCase
          ( concat
              [ "rejects "
              , showType bad1
              , "s (left only)"
              ]
          )
          $ checkExpr' (op (varExpr bad1) (varExpr t2) undefined) @?= Left 1
      , testCase
          ( concat
              [ "rejects "
              , showType bad2
              , "s (right only)"
              ]
          )
          $ checkExpr' (op (varExpr t1) (varExpr bad2) undefined) @?= Left 1
      ]
      where
        accepts
          | t1 == t2 = "accepts " ++ showType t1 ++ "s"
          | otherwise =
              "accepts " ++ showType t1 ++ "s and " ++ showType t2 ++ "s"
        bad1 = mkBad t1
        bad2 = mkBad t2

exprTestGroup :: TestTree
exprTestGroup =
  testGroup
    "unitTest"
    [ testGroup
        "checkAtom"
        [ testGroup
            "literals"
            [ testProperty "int literals are ints" $
                \i -> checkAtom' (IntLit i undefined) == pure BInt
            , testProperty "bool literals are bools" $
                \b -> checkAtom' (BoolLit b undefined) == pure BBool
            , testProperty "char literals are chars" $
                \c -> checkAtom' (CharLit c undefined) == pure BChar
            , testProperty "string literals are strings" $
                \s -> checkAtom' (StringLit s undefined) == pure BString
            , testCase "null is a literal for any pair type" $
                checkAtom' (Null undefined) @?= pure (BKnownPair BAny BAny)
            ]
        , testCase "looks up variable types" $
            forEachBType (\t -> checkAtom' (Ident t undefined) @?= pure t)
        ]
    , testGroup
        "checkExpr"
        [ testGroup
            "atoms"
            [ testCase "int atoms are ints" $ checkExpr' intExpr @?= pure BInt
            , testCase "bool atoms are bools" $
                checkExpr' boolExpr @?= pure BBool
            , testCase "char atoms are chars" $
                checkExpr' charExpr @?= pure BChar
            , testCase "string atoms are strings" $
                checkExpr' stringExpr @?= pure BString
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
    , testGroup
        "checkArrayIndex"
        [ testCase "elements of an int[] are ints" $
            checkArrayIndex' (ArrayIndex (BArray BInt) [intExpr] undefined)
              @?= pure BInt
        , testCase "elements of an int[][] are int[]s" $
            checkArrayIndex'
              (ArrayIndex (BArray (BArray BInt)) [intExpr] undefined)
              @?= pure (BArray BInt)
        , testCase "elements of elements of an int[][] are ints" $
            checkArrayIndex'
              (ArrayIndex (BArray (BArray BInt)) [intExpr, intExpr] undefined)
              @?= pure BInt
        , testCase "providing too many indices fails the check" $
            checkArrayIndex'
              (ArrayIndex (BArray BInt) [intExpr, intExpr] undefined)
              @?= Left 1
        , testCase "string indexing is not supported" $
            checkArrayIndex' (ArrayIndex BString [intExpr] undefined) @?= Left 1
        ]
    ]
