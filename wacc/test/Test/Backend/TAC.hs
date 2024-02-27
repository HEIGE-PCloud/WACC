module Test.Backend.TAC (tacTestGroup) where

import Test
import Test.Backend.TAC.ExprTest (exprTestGroup)

tacTestGroup :: TestTree
tacTestGroup = testGroup "TAC" [testGroup "unitTest" [exprTestGroup]]
