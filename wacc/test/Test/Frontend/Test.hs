{- AUTOCOLLECT.TEST -}
module Test.Frontend.Test
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test (testGroup)
import Test.Frontend.IntegrationTest (integrationTestGroup)
import Test.Frontend.Parser.Test (parserTestGroup)

test = testGroup "frontend" [integrationTestGroup, parserTestGroup]
