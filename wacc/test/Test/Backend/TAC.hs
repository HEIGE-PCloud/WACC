module Test.Backend.TAC (tacTestGroup) where

import Test
import Test.Backend.TAC.ExprTest (exprTestGroup)
import Test.Backend.TAC.ValueTest (rvalueTestGroup)

tacTestGroup :: TestTree
tacTestGroup =
  testGroup "TAC" [testGroup "unitTest" [exprTestGroup, rvalueTestGroup]]
