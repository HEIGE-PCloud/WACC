{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Scope.UnitTest
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

test =
  testGroup "unitTest" [testGroup "scope" [naming_fail_tests]]

hasErrored :: String -> Bool
hasErrored code = isFailure (scopeAnalysis t)
  where
    (Success t) = parse @String (fully program) code

badPairElemFst :: String
badPairElemFst =
  "begin \
  \int x = 5; \
  \begin \
  \fst y = null \
  \end \
  \end \
  \"

badPairElemSnd :: String
badPairElemSnd =
  "begin \
  \int x = 5; \
  \begin \
  \snd y = null \
  \end \
  \end \
  \"

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
badRvalueExpr =
  "begin \
  \int x = 5; \
  \begin \
  \x = 5 + y \
  \end \
  \end \
  \"

badRvalueArrayLit :: String
badRvalueArrayLit =
  "begin \
  \int x = 5; \
  \begin \
  \x = [1, 2, y] \
  \end \
  \end \
  \"

badRValueNewPair :: String
badRValueNewPair =
  "begin \
  \int x = 5; \
  \begin \
  \x = newpair(1, y) \
  \end \
  \end \
  \"

badRValuePairElem :: String
badRValuePairElem =
  "begin \
  \int x = 5; \
  \begin \
  \x = fst y \
  \end \
  \end \
  \"

badRValueCall :: String
badRValueCall =
  "begin \
  \int x = 5; \
  \begin \
  \x = call f(y) \
  \end \
  \end \
  \"

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
badLValueIdent =
  "begin \
  \int x = 5; \
  \begin \
  \y = 5 \
  \end \
  \end \
  \"

badLValueArrayElem :: String
badLValueArrayElem =
  "begin \
  \int x = 5; \
  \begin \
  \y[5] = 5 \
  \end \
  \end \
  \"

badLValuePairElem :: String
badLValuePairElem =
  "begin \
  \int x = 5; \
  \begin \
  \fst y = 5 \
  \end \
  \end \
  \"

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
badRenameArrayIndex =
  "begin \
  \int[] x = [1, 2]; \
  \begin \
  \x[y + 1] = 5 \
  \end \
  \end \
  \"

renameArrayIndex_fail_tests :: TestTree
renameArrayIndex_fail_tests =
  testGroup
    "Naming Fail Tests - Array Index"
    [ T.testCase "Bad Rename Array Index" $
        T.assertBool "Bad Rename Array Index" (hasErrored badRenameArrayIndex)
    ]

badIdentAtom :: String
badIdentAtom =
  "begin \
  \x = 5 \
  \end \
  \"

badArrayElemAtom :: String
badArrayElemAtom =
  "begin \
  \x[5] = 5 \
  \end \
  \"

intLitAtom :: String
intLitAtom =
  "begin \
  \int x = 5 \
  \end \
  \"

boolLitAtom :: String
boolLitAtom =
  "begin \
  \bool x = true \
  \end \
  \"

charLitAtom :: String
charLitAtom =
  "begin \
  \char x = 'a' \
  \end \
  \"

stringLitAtom :: String
stringLitAtom =
  "begin \
  \string x = \"hello\" \
  \end \
  \"

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
badAtomExpr =
  "begin \
  \int x = 5; \
  \begin \
  \x = y \
  \end \
  \end \
  \"

badNotExpr :: String
badNotExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = !y \
  \end \
  \end \
  \"

badNegateExpr :: String
badNegateExpr =
  "begin \
  \int x = 5; \
  \begin \
  \x = -y \
  \end \
  \end \
  \"

badLenExpr :: String
badLenExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = len y \
  \end \
  \end \
  \"

badOrdExpr :: String
badOrdExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = ord y \
  \end \
  \end \
  \"

badChrExpr :: String
badChrExpr =
  "begin \
  \char x = 'a'; \
  \begin \
  \x = chr y \
  \end \
  \end \
  \"

badMulExpr :: String
badMulExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = y * z \
  \end \
  \end \
  \"

badDivExpr :: String
badDivExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = y / z \
  \end \
  \end \
  \"

badModExpr :: String
badModExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = y % z \
  \end \
  \end \
  \"

badAddExpr :: String
badAddExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = y + z \
  \end \
  \end \
  \"

badSubExpr :: String
badSubExpr =
  "begin \
  \int x = 2; \
  \begin \
  \x = y - z \
  \end \
  \end \
  \"

badGTExpr :: String
badGTExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y > z \
  \end \
  \end \
  \"

badGTEExpr :: String
badGTEExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y >= z \
  \end \
  \end \
  \"

badLTExpr :: String
badLTExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y < z \
  \end \
  \end \
  \"

badLTEExpr :: String
badLTEExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y <= z \
  \end \
  \end \
  \"

badEqExpr :: String
badEqExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y == z \
  \end \
  \end \
  \"

badIneqExpr :: String
badIneqExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y != z \
  \end \
  \end \
  \"

badAndExpr :: String
badAndExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y && z \
  \end \
  \end \
  \"

badOrExpr :: String
badOrExpr =
  "begin \
  \bool x = false; \
  \begin \
  \x = y || z \
  \end \
  \end \
  \"

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
skipStatement =
  "begin \
  \skip \
  \end \
  \"

declareStatement :: String
declareStatement =
  "begin \
  \int x = 5 \
  \end \
  \"

badAsgnStatement :: String
badAsgnStatement =
  "begin \
  \int x = 5; \
  \begin \
  \y = x \
  \end \
  \end \
  \"

badReadStatement :: String
badReadStatement =
  "begin \
  \int x = 5; \
  \begin \
  \read y \
  \end \
  \end \
  \"

badFreeStatement :: String
badFreeStatement =
  "begin \
  \int x = 5; \
  \begin \
  \free y \
  \end \
  \end \
  \"

badReturnStatement :: String
badReturnStatement =
  "begin \
  \int x = 5; \
  \begin \
  \return y \
  \end \
  \end \
  \"

badExitStatement :: String
badExitStatement =
  "begin \
  \int x = 5; \
  \begin \
  \exit y \
  \end \
  \end \
  \"

badPrintStatement :: String
badPrintStatement =
  "begin \
  \int x = 5; \
  \begin \
  \print y \
  \end \
  \end \
  \"

badPrintLnStatement :: String
badPrintLnStatement =
  "begin \
  \int x = 5; \
  \begin \
  \println y \
  \end \
  \end \
  \"

badIfElseExprstatement :: String
badIfElseExprstatement =
  "begin \
  \int x = 5; \
  \begin \
  \if y then x = 5 else x = 6 fi \
  \end \
  \end \
  \"

badIfElseStatementOneStatement :: String
badIfElseStatementOneStatement =
  "begin \
  \int x = 5; \
  \begin \
  \if true then y = 5 else skip fi \
  \end \
  \end \
  \"

badIfElseStatementTwoStatement :: String
badIfElseStatementTwoStatement =
  "begin \
  \int x = 5; \
  \begin \
  \if true then x = 5 else y = 6 fi \
  \end \
  \end \
  \"

badWhileExprStatement :: String
badWhileExprStatement =
  "begin \
  \int x = 5; \
  \begin \
  \while y do x = 5 done \
  \end \
  \end \
  \"

badWhileStatementOneStatement :: String
badWhileStatementOneStatement =
  "begin \
  \int x = 5; \
  \begin \
  \while true do y = 5 done \
  \end \
  \end \
  \"

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
badStatements =
  "begin \
  \int x = 5; \
  \begin \
  \int y = 5; \
  \z = 5 \
  \end \
  \end \
  \"

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
