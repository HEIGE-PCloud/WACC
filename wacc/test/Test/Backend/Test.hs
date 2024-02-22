{- AUTOCOLLECT.TEST -}
module Test.Backend.Test
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test (testGroup)
import Test.Backend.IntegrationTest (integrationTestGroup)
import Test.Backend.X86.Test (x86TestGroup)

test = testGroup "backend" [x86TestGroup, integrationTestGroup]
