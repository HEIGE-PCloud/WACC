{- AUTOCOLLECT.TEST -}
module Test.Frontend.Test
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test (testGroup)
import Test.Frontend.IntegrationTest (integrationTestGroup)

test = testGroup "frontend" [integrationTestGroup]
