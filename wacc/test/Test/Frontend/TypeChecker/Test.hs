module Test.Frontend.TypeChecker.Test
  ( typeCheckerTestGroup
  )
where

import Test.Frontend.TypeChecker.BTypeTest (bTypeTestGroup)
import Test.Frontend.TypeChecker.ExprTest (exprTestGroup)
import Test.Frontend.TypeChecker.ProgTest (progTestGroup)
import Test.Frontend.TypeChecker.StmtTest (stmtTestGroup)
import Test.Tasty (TestTree, testGroup)

typeCheckerTestGroup :: TestTree
typeCheckerTestGroup = testGroup "typeChecker" [bTypeTestGroup, exprTestGroup, progTestGroup, stmtTestGroup]
