module Test.Backend.X86.GoldenTest
  ( goldenTestGroup
  )
where

import Data.ByteString.Lazy.UTF8 (fromString)
import Language.WACC.X86.Runtime (x86Examples)
import Language.WACC.X86.ATNT (formatA)
import qualified Language.WACC.X86.X86 as X86
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

goldenBasePath :: FilePath
goldenBasePath = "test/golden"

runX86CheckATNT :: X86.Program -> String -> TestTree
runX86CheckATNT p name = goldenVsStringDiff testname diff goldenPath testAction
  where
    testname = "X86Example." ++ name
    diff ref new = ["diff", "-u", ref, new]
    goldenPath = goldenBasePath ++ "/" ++ testname
    testAction = return (fromString (formatA p))

goldenTestGroup :: TestTree
goldenTestGroup =
  testGroup
    "goldenTest"
    [runX86CheckATNT prog name | (prog, name) <- x86Examples]
