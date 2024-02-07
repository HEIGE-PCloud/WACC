{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TypeApplications #-}

module Test.Semantic.ScopeUnitTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Control.Exception
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Except
import qualified Data.DList as DL
import Data.Either
import qualified Data.Map as Map
import Language.WACC.Parser.Prog
import Language.WACC.Semantic.Scope
import Test.Tasty
import qualified Test.Tasty.HUnit as T
import Text.Gigaparsec

test_ignoreTest = testGroup "Scope Analysis Tests" [parent_tests, naming_fail_tests]

hasErrored :: String -> Bool
hasErrored code = (w :: DL.DList String) /= DL.empty
  where
    (Success t) = parse @String prog code
    (a, s, w) = runRWS (runExceptT (renameProgram t)) (Map.empty, Map.empty) (DL.empty)

badParentScope = "begin\nint x = 5 ;\nbegin\nbegin\nfree x\nend ;\nint[] x = [1]\nend\nend\n"

parent_tests =
  testGroup
    "Parent Scope Tests"
    [ T.testCase "Bad Parent Scope" $
        T.assertBool "Bad Parent Scope nested begins" (hasErrored badParentScope)
    ]

badPairElemFst = "begin\nint x = 5;\nbegin\nfst y = null\nend\nend\n"

badPairElemSnd = "begin\nint x = 5;\nbegin\nsnd y = null\nend\nend\n"

pair_naming_fail_tests =
  testGroup
    "Naming Fail Tests - Pair Elem"
    [ T.testCase "Bad Pair elem fst" $
        T.assertBool "Bad Pair elem fst" (hasErrored badPairElemFst)
    , T.testCase "Bad Pair elem snd" $
        T.assertBool "Bad Pair elem snd" (hasErrored badPairElemSnd)
    ]

badRvalueExpr = "begin\nint x = 5;\nbegin\nx = 5 + y\nend\nend\n"

badRvalueArrayLit = "begin\nint x = 5;\nbegin\nx = [1, 2, y]\nend\nend\n"

badRValueNewPair = "begin\nint x = 5;\nbegin\nx = newpair(1, y)\nend\nend\n"

badRValuePairElem = "begin\nint x = 5;\nbegin\nx = fst y\nend\nend\n"

badRValueCall = "begin\nint x = 5;\nbegin\nx = call f(y)\nend\nend\n"

rValue_naming_fail_tests =
  testGroup
    "Naming Fail Tests - RValue"
    [ T.testCase "Bad RValue Expr" $
        T.assertBool "Bad RValue Expr" (hasErrored badRvalueExpr)
    , T.testCase "Bad RValue ArrayLit" $
        T.assertBool "Bad RValue ArrayLit" (hasErrored badRvalueArrayLit)
    , T.testCase "Bad RValue NewPair" $
        T.assertBool "Bad RValue NewPair" (hasErrored badRValueNewPair)
    , T.testCase "Bad RValue PairElem" $
        T.assertBool "Bad RValue PairElem" (hasErrored badRValuePairElem)
    , T.testCase "Bad RValue Call" $
        T.assertBool "Bad RValue Call" (hasErrored badRValueCall)
    ]

badLValueIdent = "begin\nint x = 5;\nbegin\ny = 5\nend\nend\n"

badLValueArrayElem = "begin\nint x = 5;\nbegin\ny[5] = 5\nend\nend\n"

badLValuePairElem = "begin\nint x = 5;\nbegin\nfst y = 5\nend\nend\n"

lValue_naming_fail_tests =
  testGroup
    "Naming Fail Tests - LValue"
    [ T.testCase "Bad LValue Ident" $
        T.assertBool "Bad LValue Ident" (hasErrored badLValueIdent)
    , T.testCase "Bad LValue ArrayElem" $
        T.assertBool "Bad LValue ArrayElem" (hasErrored badLValueArrayElem)
    , T.testCase "Bad LValue PairElem" $
        T.assertBool "Bad LValue PairElem" (hasErrored badLValuePairElem)
    ]

badRenameArrayIndex = "begin\nint[] x = [1, 2];\nbegin\nx[y + 1] = 5\nend\nend\n"

renameArrayIndex_fail_tests =
  testGroup
    "Naming Fail Tests - Array Index"
    [ T.testCase "Bad Rename Array Index" $
        T.assertBool "Bad Rename Array Index" (hasErrored badRenameArrayIndex)
    ]

badIdentAtom = "begin\nx = 5\nend\n"

badArrayElemAtom = "begin\nx[5] = 5\nend\n"

intLitAtom = "begin\nint x = 5\nend\n"

boolLitAtom = "begin\nbool x = true\nend\n"

charLitAtom = "begin\nchar x = 'a'\nend\n"

stringLitAtom = "begin\nstring x = \"hello\"\nend\n"

naming_fail_atom_tests =
  testGroup
    "Naming Fail Tests - Atom"
    [ T.testCase "Bad Ident Atom" $
        T.assertBool "Bad Ident Atom" (hasErrored badIdentAtom)
    , T.testCase "Bad ArrayElem Atom" $
        T.assertBool "Bad ArrayElem Atom" (hasErrored badArrayElemAtom)
    , T.testCase "Int Lit Atom" $
        T.assertBool "Int Lit Atom" (not $ hasErrored intLitAtom)
    , T.testCase "Bool Lit Atom" $
        T.assertBool "Bool Lit Atom" (not $ hasErrored boolLitAtom)
    , T.testCase "Char Lit Atom" $
        T.assertBool "Char Lit Atom" (not $ hasErrored charLitAtom)
    , T.testCase "String Lit Atom" $
        T.assertBool "String Lit Atom" (not $ hasErrored stringLitAtom)
    ]

badAtomExpr = "begin\nint x = 5;\nbegin\nx = y\nend\nend\n"

badNotExpr = "begin\nbool x = false;\nbegin\nx = !y\nend\nend\n"

badNegateExpr = "begin\nint x = 5;\nbegin\nx = -y\nend\nend\n"

badLenExpr = "begin\nint x = 2;\nbegin\nx = len y\nend\nend\n"

badOrdExpr = "begin\nint x = 2;\nbegin\nx = ord y\nend\nend\n"

badChrExpr = "begin\nchar x = 'a';\nbegin\nx = chr y\nend\nend\n"

badMulExpr = "begin\nint x = 2;\nbegin\nx = y * z\nend\nend\n"

badDivExpr = "begin\nint x = 2;\nbegin\nx = y / z\nend\nend\n"

badModExpr = "begin\nint x = 2;\nbegin\nx = y % z\nend\nend\n"

badAddExpr = "begin\nint x = 2;\nbegin\nx = y + z\nend\nend\n"

badSubExpr = "begin\nint x = 2;\nbegin\nx = y - z\nend\nend\n"

badGTExpr = "begin\nbool x = false;\nbegin\nx = y > z\nend\nend\n"

badGTEExpr = "begin\nbool x = false;\nbegin\nx = y >= z\nend\nend\n"

badLTExpr = "begin\nbool x = false;\nbegin\nx = y < z\nend\nend\n"

badLTEExpr = "begin\nbool x = false;\nbegin\nx = y <= z\nend\nend\n"

badEqExpr = "begin\nbool x = false;\nbegin\nx = y == z\nend\nend\n"

badIneqExpr = "begin\nbool x = false;\nbegin\nx = y != z\nend\nend\n"

badAndExpr = "begin\nbool x = false;\nbegin\nx = y && z\nend\nend\n"

badOrExpr = "begin\nbool x = false;\nbegin\nx = y || z\nend\nend\n"

naming_fail_expr_tests =
  testGroup
    "Naming Fail Tests - Expr"
    [ T.testCase "Bad Atom Expr" $
        T.assertBool "Bad Atom Expr" (hasErrored badAtomExpr)
    , T.testCase "Bad Not Expr" $
        T.assertBool "Bad Not Expr" (hasErrored badNotExpr)
    , T.testCase "Bad Negate Expr" $
        T.assertBool "Bad Negate Expr" (hasErrored badNegateExpr)
    , T.testCase "Bad Len Expr" $ T.assertBool "Bad Len Expr" (hasErrored badLenExpr)
    , T.testCase "Bad Ord Expr" $ T.assertBool "Bad Ord Expr" (hasErrored badOrdExpr)
    , T.testCase "Bad Chr Expr" $ T.assertBool "Bad Chr Expr" (hasErrored badChrExpr)
    , T.testCase "Bad Mul Expr" $ T.assertBool "Bad Mul Expr" (hasErrored badMulExpr)
    , T.testCase "Bad Div Expr" $ T.assertBool "Bad Div Expr" (hasErrored badDivExpr)
    , T.testCase "Bad Mod Expr" $ T.assertBool "Bad Mod Expr" (hasErrored badModExpr)
    , T.testCase "Bad Add Expr" $ T.assertBool "Bad Add Expr" (hasErrored badAddExpr)
    , T.testCase "Bad Sub Expr" $ T.assertBool "Bad Sub Expr" (hasErrored badSubExpr)
    , T.testCase "Bad GT Expr" $ T.assertBool "Bad GT Expr" (hasErrored badGTExpr)
    , T.testCase "Bad GTE Expr" $ T.assertBool "Bad GTE Expr" (hasErrored badGTEExpr)
    , T.testCase "Bad LT Expr" $ T.assertBool "Bad LT Expr" (hasErrored badLTExpr)
    , T.testCase "Bad LTE Expr" $ T.assertBool "Bad LTE Expr" (hasErrored badLTEExpr)
    , T.testCase "Bad Eq Expr" $ T.assertBool "Bad Eq Expr" (hasErrored badEqExpr)
    , T.testCase "Bad Ineq Expr" $
        T.assertBool "Bad Ineq Expr" (hasErrored badIneqExpr)
    , T.testCase "Bad And Expr" $ T.assertBool "Bad And Expr" (hasErrored badAndExpr)
    , T.testCase "Bad Or Expr" $ T.assertBool "Bad Or Expr" (hasErrored badOrExpr)
    ]

skipStatement = "begin\nskip\nend\n"

declareStatement = "begin\nint x = 5\nend\n"

badAsgnStatement = "begin\nint x = 5;\nbegin\ny = x\nend\nend\n"

badReadStatement = "begin\nint x = 5;\nbegin\nread y\nend\nend\n"

badFreeStatement = "begin\nint x = 5;\nbegin\nfree y\nend\nend\n"

badReturnStatement = "begin\nint x = 5;\nbegin\nreturn y\nend\nend\n"

badExitStatement = "begin\nint x = 5;\nbegin\nexit y\nend\nend\n"

badPrintStatement = "begin\nint x = 5;\nbegin\nprint y\nend\nend\n"

badPrintLnStatement = "begin\nint x = 5;\nbegin\nprintln y\nend\nend\n"

badIfElseExprstatement = "begin\nint x = 5;\nbegin\nif y then x = 5 else x = 6\nend\nend\n"

badIfElseStatementOneStatement = "begin\nint x = 5;\nbegin\nif true then y = 5 else skip\nend\nend\n"

badIfElseStatementTwoStatement = "begin\nint x = 5;\nbegin\nif true then x = 5 else y = 6\nend\nend\n"

badWhileExprStatement = "begin\nint x = 5;\nbegin\nwhile y do x = 5\nend\nend\n"

badWhileStatementOneStatement = "begin\nint x = 5;\nbegin\nwhile true do y = 5\nend\nend\n"

rename_fail_statement_tests =
  testGroup
    "Naming Fail Tests - Statement"
    [ T.testCase "Skip Statement" $
        T.assertBool "Skip Statement" (not $ hasErrored skipStatement)
    , T.testCase "Declare Statement" $
        T.assertBool "Declare Statement" (not $ hasErrored declareStatement)
    , T.testCase "Bad Asgn Statement" $
        T.assertBool "Bad Asgn Statement" (hasErrored badAsgnStatement)
    , T.testCase "Bad Read Statement" $
        T.assertBool "Bad Read Statement" (hasErrored badReadStatement)
    , T.testCase "Bad Free Statement" $
        T.assertBool "Bad Free Statement" (hasErrored badFreeStatement)
    , T.testCase "Bad Return Statement" $
        T.assertBool "Bad Return Statement" (hasErrored badReturnStatement)
    , T.testCase "Bad Exit Statement" $
        T.assertBool "Bad Exit Statement" (hasErrored badExitStatement)
    , T.testCase "Bad Print Statement" $
        T.assertBool "Bad Print Statement" (hasErrored badPrintStatement)
    , T.testCase "Bad PrintLn Statement" $
        T.assertBool "Bad PrintLn Statement" (hasErrored badPrintLnStatement)
    , T.testCase "Bad IfElse Expr Statement" $
        T.assertBool "Bad IfElse Expr Statement" (hasErrored badIfElseExprstatement)
    , T.testCase "Bad IfElse Statement One Statement" $
        T.assertBool
          "Bad IfElse Statement One Statement"
          (hasErrored badIfElseStatementOneStatement)
    , T.testCase "Bad IfElse Statement Two Statement" $
        T.assertBool
          "Bad IfElse Statement Two Statement"
          (hasErrored badIfElseStatementTwoStatement)
    , T.testCase "Bad While Expr Statement" $
        T.assertBool "Bad While Expr Statement" (hasErrored badWhileExprStatement)
    , T.testCase "Bad While Statement One Statement" $
        T.assertBool
          "Bad While Statement One Statement"
          (hasErrored badWhileStatementOneStatement)
    ]

badStatements = "begin\nint x = 5;\nbegin\nint y = 5;\nz = 5\nend\nend\n"

rename_fail_statements_tests =
  testGroup
    "Naming Fail Tests - Statements"
    [ T.testCase "Bad Statements" $
        T.assertBool "Bad Statements" (hasErrored badStatements)
    ]

naming_fail_tests =
  testGroup
    "Naming Fail Tests"
    [ pair_naming_fail_tests
    , rValue_naming_fail_tests
    , lValue_naming_fail_tests
    , renameArrayIndex_fail_tests
    , naming_fail_atom_tests
    , naming_fail_expr_tests
    , rename_fail_statement_tests
    , rename_fail_statements_tests
    ]
