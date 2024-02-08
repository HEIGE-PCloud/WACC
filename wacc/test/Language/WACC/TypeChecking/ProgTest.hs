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

testTypingM :: TypingM Int BType () -> Int -> Either Int FnType
testTypingM action = case runTypingM action id mempty of
  (Just _, fs, es) | null es -> maybe (Left 0) pure . (fs !?)
  (_, _, es) -> const . Left $ length es

checkFunc' :: Func Int BType -> Int -> Either Int FnType
checkFunc' = testTypingM . checkFunc

checkProg' :: Prog Int BType -> Int -> Either Int FnType
checkProg' = testTypingM . checkProg

intExpr :: Expr BType
intExpr = WAtom (IntLit 0 undefined) undefined

boolExpr :: Expr BType
boolExpr = WAtom (BoolLit False undefined) undefined

varExpr :: BType -> Expr BType
varExpr = flip WAtom undefined . (`Ident` undefined)

func :: Func Int BType
func = Func WInt 0 [(WInt, BInt)] [Return intExpr undefined] undefined

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
                  ( Func
                      rwt
                      i
                      (mkParam <$> pwts)
                      [Return (varExpr rt) undefined]
                      undefined
                  )
                  i
                  == pure (FnType pts rt)
        , testCase "rejects incompatible return statements in body" $
            checkFunc' (Func WInt 0 [] [Return boolExpr undefined] undefined) 0
              @?= Left 1
        ]
    , testGroup
        "checkProg"
        [ testCase "accepts valid function calls" $
            checkProg'
              ( Main
                  [func]
                  [ Asgn
                      (LVIdent BInt undefined)
                      (RVCall 0 [intExpr] undefined)
                      undefined
                  ]
                  undefined
              )
              0
              @?= pure funcType
        , testCase "accepts invalid function calls" $
            checkProg'
              ( Main
                  [func]
                  [ Asgn
                      (LVIdent BInt undefined)
                      (RVCall 0 [boolExpr] undefined)
                      undefined
                  ]
                  undefined
              )
              0
              @?= Left 1
        , testCase "rejects unknown function calls" $
            checkProg'
              ( Main
                  [func]
                  [ Asgn
                      (LVIdent BInt undefined)
                      (RVCall 1 [intExpr] undefined)
                      undefined
                  ]
                  undefined
              )
              0
              @?= Left 0
        , testProperty "rejects return statements in main program" $
            \wt ->
              checkProg'
                (Main [func] [Return (varExpr $ fix wt) undefined] undefined)
                0
                == Left 1
        ]
    ]
