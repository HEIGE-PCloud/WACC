{-# LANGUAGE OverloadedLists #-}

{- AUTOCOLLECT.TEST -}

module Language.WACC.TypeChecking.ProgTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.Map ((!?))
import Language.WACC.AST
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Prog
import Language.WACC.TypeChecking.State
import Test

mkParam :: WType -> (WType, BType)
mkParam wt = (wt, fix wt)

testTypingM :: TypingM Int BType () -> Int -> Maybe FnType
testTypingM action = case runTypingM action id mempty of
  (Nothing, _, _) -> const Nothing
  (_, fs, _) -> (fs !?)

checkFunc' :: Func Int BType -> Int -> Maybe FnType
checkFunc' = testTypingM . checkFunc

boolExpr :: Expr BType
boolExpr = WAtom $ BoolLit False undefined

varExpr :: BType -> Expr BType
varExpr = WAtom . Ident

test :: TestTree
test =
  testGroup
    "unitTests"
    [ testGroup
        "checkFunc"
        [ testProperty "records correct parameter and return types" $
            \i pwts rwt ->
              let
                pts = fix <$> pwts
                rt = fix rwt
              in
                checkFunc'
                  (Func rwt i (mkParam <$> pwts) [Return $ varExpr rt])
                  i
                  == Just (FnType pts rt)
        , testCase "rejects incompatible return statements in body" $
            checkFunc' (Func WInt 0 [] [Return boolExpr]) 0 @?= Nothing
        ]
    ]
