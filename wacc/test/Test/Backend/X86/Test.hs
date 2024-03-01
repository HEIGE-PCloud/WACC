module Test.Backend.X86.Test
  ( x86TestGroup
  )
where

import Test (testGroup)
import Test.Backend.X86.GoldenTest (goldenTestGroup)
-- import Test.Backend.X86.TranslateTest (translateTestGroup)
import Test.Tasty (TestTree)

x86TestGroup :: TestTree
-- x86TestGroup = testGroup "x86" [goldenTestGroup, translateTestGroup]
x86TestGroup = testGroup "x86" [goldenTestGroup]
