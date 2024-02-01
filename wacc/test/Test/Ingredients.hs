module Test.Ingredients
  ( testReporter,
  )
where

import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)
import Test.Tasty.Runners.AntXML (antXMLRunner)

testReporter = composeReporters consoleTestReporter antXMLRunner
