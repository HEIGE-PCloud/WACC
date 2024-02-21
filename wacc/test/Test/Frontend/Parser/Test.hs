module Test.Frontend.Parser.Test
  ( parserTestGroup
  )
where

import Test (testGroup)
import Test.Frontend.Parser.GoldenTest (goldenTestGroup)
import Test.Frontend.Parser.UnitTest (unitTestGroup)

parserTestGroup = testGroup "parser" [unitTestGroup, goldenTestGroup]
