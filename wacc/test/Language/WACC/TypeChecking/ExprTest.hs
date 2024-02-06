{- AUTOCOLLECT.TEST -}

module Language.WACC.TypeChecking.ExprTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Control.Monad.Trans.State (evalStateT)
import Data.Foldable (traverse_)
import Data.Map (Map, fromList)
import Language.WACC.AST.Expr
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Expr
import Test

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

vars :: Map BType BType
vars = fromList $ fmap (\x -> (x, x)) btypes

checkAtom' :: WAtom BType -> Maybe BType
checkAtom' atom = evalStateT (checkAtom atom) vars

checkExpr' :: Expr BType -> Maybe BType
checkExpr' x = evalStateT (checkExpr x) vars

intExpr :: Expr BType
intExpr = WAtom $ IntLit 0 undefined

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
    ]
