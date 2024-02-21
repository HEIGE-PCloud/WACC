module Test.Backend.X86.Test
  ( x86TestGroup
  )
where

import Test (testGroup)
import Test.Backend.X86.GoldenTest (goldenTestGroup)

x86TestGroup = testGroup "x86" [goldenTestGroup]
