module Test.Ingredients
  ( testReporter
  )
where

import Test.Tasty.Ingredients (composeReporters, Ingredient)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)
import Test.Tasty.Runners.AntXML (antXMLRunner)

testReporter :: Ingredient
testReporter = composeReporters consoleTestReporter antXMLRunner
