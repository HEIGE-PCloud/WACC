{- AUTOCOLLECT.TEST -}
module Test.Frontend.Test
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test (testGroup)
import Test.Frontend.IntegrationTest (integrationTestGroup)
import Test.Frontend.Parser.Test (parserTestGroup)
import Test.Frontend.Scope.Test (scopeTestGroup)
import Test.Frontend.TypeChecker.Test (typeCheckerTestGroup)

test = testGroup "frontend" [integrationTestGroup, parserTestGroup, scopeTestGroup, typeCheckerTestGroup]
