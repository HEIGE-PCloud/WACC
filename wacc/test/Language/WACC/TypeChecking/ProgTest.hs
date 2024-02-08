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

checkProg' :: Prog Int BType -> Int -> Maybe FnType
checkProg' = testTypingM . checkProg

intExpr :: Expr BType
intExpr = WAtom (IntLit 0) undefined

boolExpr :: Expr BType
boolExpr = WAtom (BoolLit False) undefined

varExpr :: BType -> Expr BType
varExpr = flip WAtom undefined . Ident

func :: Func Int BType
func = Func WInt 0 [(WInt, BInt)] [Return intExpr]

funcType :: FnType
funcType = FnType [BInt] BInt

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
    , testGroup
        "checkProg"
        [ testCase "accepts valid function calls" $
            checkProg'
              (Main [func] [Asgn (LVIdent BInt) (RVCall 0 [intExpr])])
              0
              @?= Just funcType
        , testCase "accepts invalid function calls" $
            checkProg'
              (Main [func] [Asgn (LVIdent BInt) (RVCall 0 [boolExpr])])
              0
              @?= Nothing
        , testCase "rejects unknown function calls" $
            checkProg'
              (Main [func] [Asgn (LVIdent BInt) (RVCall 1 [intExpr])])
              0
              @?= Nothing
        , testProperty "rejects return statements in main program" $
            \wt ->
              checkProg' (Main [func] [Return (varExpr $ fix wt)]) 0
                == Nothing
        ]
    ]
