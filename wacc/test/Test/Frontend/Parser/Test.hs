
module Test.Frontend.Parser.Test
  ( parserTestGroup
  )
where

import Test (testGroup)
import Test.Frontend.Parser.UnitTest (unitTestGroup)
import Test.Frontend.Parser.GoldenTest (goldenTestGroup)

parserTestGroup = testGroup "parser" [unitTestGroup, goldenTestGroup]
