module Test.Frontend.Scope.Test
  ( scopeTestGroup
  )
where

import Test (testGroup)
import Test.Frontend.Scope.UnitTest (unitTestGroup)

scopeTestGroup = testGroup "scope" [unitTestGroup]
