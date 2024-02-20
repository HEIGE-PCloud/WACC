module Test.Ingredients
  ( testReporter
  )
where

import Test.AntXML (antXMLRunner)
import Test.Tasty.Ingredients (Ingredient, composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)

testReporter :: Ingredient
testReporter = composeReporters consoleTestReporter antXMLRunner
