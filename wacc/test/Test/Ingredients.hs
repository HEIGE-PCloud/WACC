module Test.Ingredients
  ( testReporter
  )
where

import Test.Tasty.Ingredients (Ingredient, composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)
import Test.Tasty.Runners.AntXML (antXMLRunner)

testReporter :: Ingredient
testReporter = composeReporters consoleTestReporter antXMLRunner