{- AUTOCOLLECT.TEST -}

module Test.Parser.QuickCheck
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Control.Exception
import Data.Functor
import Data.List ((\\))
import Debug.Trace (trace, traceM)
import Language.WACC.Parser.Expr
import Language.WACC.Parser.Token (fully, keywords)
import Language.WACC.Parser.Type
import qualified Test.QuickCheck.Property as P
import Test.Tasty.QuickCheck
import qualified Text.Gigaparsec as T

optional :: Gen String -> Gen String
optional gen = frequency [(1, gen), (1, return "")]

many :: Gen String -> Gen String
many gen = listOf gen <&> concat

some :: Gen String -> Gen String
some gen = listOf1 gen <&> concat

limit :: Int -> Int
limit x = max 0 (1 - x)

(-/-) :: Int -> Int
(-/-) x = x - 1

-- someN :: Gen String -> Int -> Gen String
someN :: Gen String -> Int -> Gen String
someN gen n = do
  k <- choose (1, max 1 n)
  concat <$> vectorOf k gen

genIntLiter :: Gen String
genIntLiter = do
  sign <- optional genIntSign
  digits <- listOf1 genDigit
  return $ sign ++ digits

genDigit :: Gen Char
genDigit = choose ('0', '9')

genIntSign :: Gen String
genIntSign = elements ["+", "-"]

genBoolLiter :: Gen String
genBoolLiter = elements ["true", "false"]

genCharLiter :: Gen String
genCharLiter = do
  c <- genCharacter
  return $ "\'" ++ c ++ "\'"

genStringLiter :: Gen String
genStringLiter = do
  c <- genCharacter
  return $ "\"" ++ c ++ "\""

escapedChars :: [String]
escapedChars = ["0", "b", "t", "n", "f", "r", "\"", "'", "\\"]

graphicASCII :: [Char]
graphicASCII = ['\32' .. '\126']

genCharacter :: Gen String
genCharacter =
  elements $
    (map (: []) graphicASCII \\ escapedChars) ++ ["\\" ++ c | c <- escapedChars]

genPairLiter :: Gen String
genPairLiter = return "null"

genIdent :: Gen String
genIdent = genIdent' `suchThat` (`notElem` keywords)
  where
    genIdent' = do
      c <- elements $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
      cs <- listOf $ elements $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
      return $ c : cs

genComment :: Gen String
genComment = do
  c <- elements ['#']
  cs <- listOf1 $ elements $ graphicASCII ++ ['\n']
  return $ c : cs

genExpr :: Int -> Gen String
genExpr depth
  | depth < 0 = genAtom depth
  | otherwise =
      frequency
        [ (1, genExpr1)
        , (1, genExpr2)
        , (1, genExpr3)
        ]
  where
    genExpr1 = do
      c1 <- genUnaryOper
      c2 <- genExpr (depth - 1)
      return $ c1 ++ " " ++ c2
    genExpr2 = do
      c1 <- genExpr (depth - 1)
      c2 <- genBinaryOper
      c3 <- genExpr (depth - 1)
      return $ "(" ++ c1 ++ " " ++ c2 ++ " " ++ c3 ++ ")"
    genExpr3 = do
      genAtom $ depth - 1

genAtom :: Int -> Gen String
genAtom depth
  | depth < 0 =
      oneof
        [ genIntLiter
        , genBoolLiter
        , genCharLiter
        , genStringLiter
        , genPairLiter
        , genIdent
        ]
  | otherwise =
      frequency
        [ (1, genIntLiter)
        , (1, genBoolLiter)
        , (1, genCharLiter)
        , (1, genStringLiter)
        , (1, genPairLiter)
        , (1, genIdent)
        , (1, genArrayElem $ depth - 1)
        , (1, genExpr')
        ]
  where
    genExpr' = do
      c1 <- genExpr (depth - 1)
      return $ "(" ++ c1 ++ ")"

genUnaryOper :: Gen String
genUnaryOper = elements ["!", "-", "len", "ord", "chr"]

genBinaryOper :: Gen String
genBinaryOper =
  elements ["*", "/", "%", "+", "-", "<", "<=", ">", ">=", "==", "!=", "&&", "||"]

genArrayElem :: Int -> Gen String
genArrayElem depth = do
  c1 <- genIdent
  c2 <- genBrackets
  return $ c1 ++ c2
  where
    genBracket = do c <- genExpr $ depth - 1; return $ "[" ++ c ++ "]"
    genBrackets = someN genBracket 3

genType :: Int -> Gen String
genType depth
  | depth < 0 = genBaseType
  | otherwise =
      oneof [genBaseType, genArrayType (depth - 1), genPairType (depth - 1)]

genBaseType :: Gen String
genBaseType = elements ["int", "bool", "char", "string"]

genArrayType :: Int -> Gen String
genArrayType depth = do
  c1 <- genType (depth - 1)
  return $ c1 ++ "[]"

genPairType :: Int -> Gen String
genPairType depth = do
  c1 <- genPairElemType (depth - 1)
  c2 <- genPairElemType (depth - 1)
  return $ "pair(" ++ c1 ++ "," ++ c2 ++ ")"

genPairElemType :: Int -> Gen String
genPairElemType depth
  | depth < 0 = oneof [genBaseType, return "pair"]
  | otherwise = oneof [genBaseType, genArrayType (depth - 1), return "pair"]

genProgram :: Int -> Gen String
genProgram depth = do
  c1 <- many (genFunc (depth - 1))
  c2 <- unlines <$> listOf (genStmt (depth - 1))
  return ("begin\n" ++ c1 ++ "\n" ++ c2 ++ "end")

genFunc :: Int -> Gen String
genFunc depth = do
  c1 <- genParam (depth - 1)
  c2 <- optional (genParamList (depth - 1))
  c3 <- genStmt (depth - 1)
  return (c1 ++ "(" ++ c2 ++ ")" ++ "is\n" ++ c3 ++ "end")

genParamList :: Int -> Gen String
genParamList depth = do
  c1 <- genParam (depth - 1)
  c2 <- many genParams
  return $ c1 ++ c2
  where
    genParams = do
      c1 <- genParam (depth - 1)
      return ("," ++ c1)

genParam :: Int -> Gen String
genParam depth = do
  c1 <- genType (depth - 1)
  c2 <- genIdent
  return $ c1 ++ " " ++ c2

genStmt :: Int -> Gen String
genStmt depth
  | depth < 0 = genSkip
  | otherwise =
      oneof
        [ genSkip
        , genDef
        , genAssign
        , genExpr' "read"
        , genExpr' "free"
        , genExpr' "return"
        , genExpr' "exit"
        , genExpr' "print"
        , genExpr' "println"
        ]
  where
    genSkip = return "skip"
    genDef = do
      c1 <- genParam (depth - 1)
      c2 <- genRvalue (depth - 1)
      return (c1 ++ "=" ++ c2)
    genAssign = do
      c1 <- genLvalue (depth - 1)
      c2 <- genRvalue (depth - 1)
      return $ c1 ++ "=" ++ c2
    genExpr' str = do
      c1 <- genLvalue (depth - 1)
      return $ str ++ " " ++ c1

genLvalue :: Int -> Gen String
genLvalue depth
  | depth < 0 = genIdent
  | otherwise =
      oneof [genIdent, genArrayElem (depth - 1), genPairElem (depth - 1)]

genRvalue :: Int -> Gen String
genRvalue depth =
  oneof
    [ genExpr (depth - 1)
    , genArrayLiter (depth - 1)
    , genNewpair
    , genPairElem (depth - 1)
    , genCall
    ]
  where
    genNewpair = do
      c1 <- genExpr (depth - 1)
      c2 <- genExpr (depth - 1)
      return $ "newpair(" ++ c1 ++ "," ++ c2 ++ ")"
    genCall = do
      c1 <- genIdent
      c2 <- optional (genArgList (depth - 1))
      return $ "call" ++ c1 ++ "(" ++ c2 ++ ")"

genArgList :: Int -> Gen String
genArgList depth = do
  c1 <- genExpr (depth - 1)
  c2 <- many (genArgs depth)
  return $ c1 ++ c2

genArgs :: Int -> Gen String
genArgs depth = do
  c1 <- genExpr (depth - 1)
  return ("," ++ c1)

genPairElem :: Int -> Gen String
genPairElem depth =
  oneof
    [ gen "fst"
    , gen "snd"
    ]
  where
    gen str = do
      c1 <- genExpr (depth - 1)
      return $ str ++ " " ++ c1

genArrayLiter :: Int -> Gen String
genArrayLiter depth = do
  c1 <- many (genArgs depth)
  c2 <- genExpr (depth - 1)
  c3 <- optional (pure (c1 ++ c2))
  return ("[" ++ c3 ++ "]")

parse' :: T.Parsec a -> String -> T.Result String a
parse' = T.parse

check :: T.Parsec a -> String -> P.Result
check parser str = case parse' (fully parser) str of
  T.Success _ -> P.succeeded
  T.Failure err -> P.failed {P.reason = "Failed to parse " ++ err}

check' :: T.Parsec a -> Gen String -> Property
check' parser gen = withMaxSuccess 100000 $ forAll gen $ check parser

test = testProperty "can parse intLiter" $ check' intLiter genIntLiter

test = testProperty "can parse boolLiter" $ check' boolLiter genBoolLiter

test = testProperty "can parse charLiter" $ check' charLiter genCharLiter

test = testProperty "can parse stringLiter" $ check' stringLiter genStringLiter

test = testProperty "can parse pairLiter" $ check' pairLiter genPairLiter

test = testProperty "can parse ident" $ check' ident genIdent

test =
  testProperty "can parse arrayElem" $ check' arrayElemExpr $ sized genArrayElem

test = testProperty "can parse atom" $ check' atom $ sized genAtom

test = testProperty "can parse expr" $ check' expr $ sized genExpr

test = testProperty "can parse type" $ check' wType $ sized genType

test = testProperty "can parse baseType" $ check' wBaseType genBaseType

test =
  testProperty "can parse arrayType" $
    check' (wTypeWithArray wBaseType) $
      sized genArrayType

test = testProperty "can parse pairType" $ check' wPairType $ sized genPairType

test_expectFail =
  testProperty "can parse pairElemType" $
    check' pairElemType $
      sized genPairElemType
