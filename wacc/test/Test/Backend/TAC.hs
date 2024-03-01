module Test.Backend.TAC (tacTestGroup) where

import Test
import Test.Backend.TAC.ExprTest (exprTestGroup)
import Test.Backend.TAC.StmtTest (stmtTestGroup, stmtsTestGroup)
import Test.Backend.TAC.ValueTest (lvalueTestGroup, rvalueTestGroup)

tacTestGroup :: TestTree
tacTestGroup =
  testGroup
    "TAC"
    [ testGroup
        "unitTest"
        [ exprTestGroup
        , lvalueTestGroup
        , rvalueTestGroup
        , stmtTestGroup
        , stmtsTestGroup
        ]
    ]
