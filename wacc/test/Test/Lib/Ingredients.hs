module Test.Lib.Ingredients
  ( testReporter
  )
where

import Test.Lib.AntXML (antXMLRunner)
import Test.Tasty.Ingredients (Ingredient, composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)

testReporter :: Ingredient
testReporter = composeReporters consoleTestReporter antXMLRunner
