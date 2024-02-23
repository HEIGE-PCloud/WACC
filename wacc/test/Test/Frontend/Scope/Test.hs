module Test.Frontend.Scope.Test
  ( scopeTestGroup
  )
where

import Test (testGroup)
import Test.Frontend.Scope.UnitTest (unitTestGroup)
import Test.Tasty (TestTree)

scopeTestGroup :: TestTree
scopeTestGroup = testGroup "scope" [unitTestGroup]
