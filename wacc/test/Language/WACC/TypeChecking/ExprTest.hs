{- AUTOCOLLECT.TEST -}

module Language.WACC.TypeChecking.ExprTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Control.Monad.Trans.State (evalStateT)
import Data.Map (Map, empty)
import Language.WACC.AST.Expr
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Expr
import Test

vars :: Map () BType
vars = empty

checkAtom' :: WAtom () -> Maybe BType
checkAtom' atom = evalStateT (checkAtom atom) vars

checkExpr' :: Expr () -> Maybe BType
checkExpr' x = evalStateT (checkExpr x) vars

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
        ]
    ]
