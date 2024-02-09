{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Semantic.ScopeUnitTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Language.WACC.Error
import Language.WACC.Parser.Stmt
import Language.WACC.Parser.Token
import Language.WACC.Semantic.Scope
import Test.Tasty
import qualified Test.Tasty.HUnit as T
import Text.Gigaparsec
import Language.WACC.AST.Prog (Prog)

test =
  testGroup "unitTests" [testGroup "scopeTests" [naming_fail_tests]]

hasErrored :: String -> Bool
hasErrored code = isFailure (scopeAnalysis t)
  where
    (Success t) = parse @String (fully program) code

badPairElemFst :: String
badPairElemFst = "begin\nint x = 5;\nbegin\nfst y = null\nend\nend\n"

badPairElemSnd :: String
badPairElemSnd = "begin\nint x = 5;\nbegin\nsnd y = null\nend\nend\n"

pair_naming_fail_tests :: TestTree
pair_naming_fail_tests =
  testGroup
    "Naming Fail Tests - Pair Elem"
    [ T.testCase "Bad Pair elem fst" $
        T.assertBool "Bad Pair elem fst" (hasErrored badPairElemFst)
    , T.testCase "Bad Pair elem snd" $
        T.assertBool "Bad Pair elem snd" (hasErrored badPairElemSnd)
    ]

badRvalueExpr :: String
badRvalueExpr = "begin\nint x = 5;\nbegin\nx = 5 + y\nend\nend\n"

badRvalueArrayLit :: String
badRvalueArrayLit = "begin\nint x = 5;\nbegin\nx = [1, 2, y]\nend\nend\n"

badRValueNewPair :: String
badRValueNewPair = "begin\nint x = 5;\nbegin\nx = newpair(1, y)\nend\nend\n"

badRValuePairElem :: String
badRValuePairElem = "begin\nint x = 5;\nbegin\nx = fst y\nend\nend\n"

badRValueCall :: String
badRValueCall = "begin\nint x = 5;\nbegin\nx = call f(y)\nend\nend\n"

rValue_naming_fail_tests :: TestTree
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

badLValueIdent :: String
badLValueIdent = "begin\nint x = 5;\nbegin\ny = 5\nend\nend\n"

badLValueArrayElem :: String
badLValueArrayElem = "begin\nint x = 5;\nbegin\ny[5] = 5\nend\nend\n"

badLValuePairElem :: String
badLValuePairElem = "begin\nint x = 5;\nbegin\nfst y = 5\nend\nend\n"

lValue_naming_fail_tests :: TestTree
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

badRenameArrayIndex :: String
badRenameArrayIndex = "begin\nint[] x = [1, 2];\nbegin\nx[y + 1] = 5\nend\nend\n"

renameArrayIndex_fail_tests :: TestTree
renameArrayIndex_fail_tests =
  testGroup
    "Naming Fail Tests - Array Index"
    [ T.testCase "Bad Rename Array Index" $
        T.assertBool "Bad Rename Array Index" (hasErrored badRenameArrayIndex)
    ]

badIdentAtom :: String
badIdentAtom = "begin\nx = 5\nend\n"

badArrayElemAtom :: String
badArrayElemAtom = "begin\nx[5] = 5\nend\n"

intLitAtom :: String
intLitAtom = "begin\nint x = 5\nend\n"

boolLitAtom :: String
boolLitAtom = "begin\nbool x = true\nend\n"

charLitAtom :: String
charLitAtom = "begin\nchar x = 'a'\nend\n"

stringLitAtom :: String
stringLitAtom = "begin\nstring x = \"hello\"\nend\n"

naming_fail_atom_tests :: TestTree
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

badAtomExpr :: String
badAtomExpr = "begin\nint x = 5;\nbegin\nx = y\nend\nend\n"

badNotExpr :: String
badNotExpr = "begin\nbool x = false;\nbegin\nx = !y\nend\nend\n"

badNegateExpr :: String
badNegateExpr = "begin\nint x = 5;\nbegin\nx = -y\nend\nend\n"

badLenExpr :: String
badLenExpr = "begin\nint x = 2;\nbegin\nx = len y\nend\nend\n"

badOrdExpr :: String
badOrdExpr = "begin\nint x = 2;\nbegin\nx = ord y\nend\nend\n"

badChrExpr :: String
badChrExpr = "begin\nchar x = 'a';\nbegin\nx = chr y\nend\nend\n"

badMulExpr :: String
badMulExpr = "begin\nint x = 2;\nbegin\nx = y * z\nend\nend\n"

badDivExpr :: String
badDivExpr = "begin\nint x = 2;\nbegin\nx = y / z\nend\nend\n"

badModExpr :: String
badModExpr = "begin\nint x = 2;\nbegin\nx = y % z\nend\nend\n"

badAddExpr :: String
badAddExpr = "begin\nint x = 2;\nbegin\nx = y + z\nend\nend\n"

badSubExpr :: String
badSubExpr = "begin\nint x = 2;\nbegin\nx = y - z\nend\nend\n"
badGTExpr :: String

badGTExpr = "begin\nbool x = false;\nbegin\nx = y > z\nend\nend\n"

badGTEExpr :: String
badGTEExpr = "begin\nbool x = false;\nbegin\nx = y >= z\nend\nend\n"

badLTExpr :: String
badLTExpr = "begin\nbool x = false;\nbegin\nx = y < z\nend\nend\n"

badLTEExpr :: String
badLTEExpr = "begin\nbool x = false;\nbegin\nx = y <= z\nend\nend\n"

badEqExpr :: String
badEqExpr = "begin\nbool x = false;\nbegin\nx = y == z\nend\nend\n"

badIneqExpr :: String
badIneqExpr = "begin\nbool x = false;\nbegin\nx = y != z\nend\nend\n"

badAndExpr :: String
badAndExpr = "begin\nbool x = false;\nbegin\nx = y && z\nend\nend\n"

badOrExpr :: String
badOrExpr = "begin\nbool x = false;\nbegin\nx = y || z\nend\nend\n"

naming_fail_expr_tests :: TestTree
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

skipStatement :: String
skipStatement = "begin\nskip\nend\n"

declareStatement :: String
declareStatement = "begin\nint x = 5\nend\n"

badAsgnStatement :: String
badAsgnStatement = "begin\nint x = 5;\nbegin\ny = x\nend\nend\n"

badReadStatement :: String
badReadStatement = "begin\nint x = 5;\nbegin\nread y\nend\nend\n"

badFreeStatement :: String
badFreeStatement = "begin\nint x = 5;\nbegin\nfree y\nend\nend\n"

badReturnStatement :: String
badReturnStatement = "begin\nint x = 5;\nbegin\nreturn y\nend\nend\n"

badExitStatement :: String
badExitStatement = "begin\nint x = 5;\nbegin\nexit y\nend\nend\n"

badPrintStatement :: String
badPrintStatement = "begin\nint x = 5;\nbegin\nprint y\nend\nend\n"

badPrintLnStatement :: String
badPrintLnStatement = "begin\nint x = 5;\nbegin\nprintln y\nend\nend\n"

badIfElseExprstatement :: String
badIfElseExprstatement = "begin\nint x = 5;\nbegin\nif y then x = 5 else x = 6 fi\nend\nend\n"

badIfElseStatementOneStatement :: String
badIfElseStatementOneStatement = "begin\nint x = 5;\nbegin\nif true then y = 5 else skip fi\nend\nend\n"

badIfElseStatementTwoStatement :: String
badIfElseStatementTwoStatement = "begin\nint x = 5;\nbegin\nif true then x = 5 else y = 6 fi\nend\nend\n"
badWhileExprStatement :: String

badWhileExprStatement = "begin\nint x = 5;\nbegin\nwhile y do x = 5 done\nend\nend\n"

badWhileStatementOneStatement :: String
badWhileStatementOneStatement = "begin\nint x = 5;\nbegin\nwhile true do y = 5 done\nend\nend\n"

rename_fail_statement_tests :: TestTree
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

badStatements :: String
badStatements = "begin\nint x = 5;\nbegin\nint y = 5;\nz = 5\nend\nend\n"

rename_fail_statements_tests :: TestTree
rename_fail_statements_tests =
  testGroup
    "Naming Fail Tests - Statements"
    [ T.testCase "Bad Statements" $
        T.assertBool "Bad Statements" (hasErrored badStatements)
    ]

naming_fail_tests :: TestTree
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
