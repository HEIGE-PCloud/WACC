{- AUTOCOLLECT.TEST -}

module Test.Parser.QuickCheck
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.Functor
import Data.List ((\\))
import Debug.Trace (trace)
import Language.WACC.Parser.Expr
import Language.WACC.Parser.Token (fully, keywords)
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

-- someN :: Gen String -> Int -> Gen String
someN :: Gen String -> Int -> Gen String
someN gen n = do
  k <- choose (1, n)
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

genExpr :: Gen String
genExpr = sized $ \n ->
  frequency
    [ (limit n, genExpr1)
    , (limit n, genExpr2)
    , (1, genExpr3)
    ]
  where
    genExpr1 = do
      c1 <- genUnaryOper
      c2 <- genExpr
      return $ c1 ++ " " ++ c2
    genExpr2 = do
      c1 <- genExpr
      c2 <- genBinaryOper
      c3 <- genExpr
      return $ c1 ++ " " ++ c2 ++ " " ++ c3
    genExpr3 = do
      genAtom

genAtom :: Gen String
genAtom = sized $ \n ->
  frequency
    [ (1, genIntLiter)
    , (1, genBoolLiter)
    , (1, genCharLiter)
    , (1, genStringLiter)
    , (1, genPairLiter)
    , (1, genIdent)
    , (limit n, genArrayElem)
    , (limit n, genExpr')
    ]
  where
    genExpr' = do
      c1 <- genExpr
      return $ "(" ++ c1 ++ ")"

genUnaryOper :: Gen String
genUnaryOper = elements ["!", "-", "len", "ord", "chr"]

genBinaryOper :: Gen String
genBinaryOper =
  elements ["*", "/", "%", "+", "-", "<", "<=", ">", ">=", "==", "!=", "&&", "||"]

genArrayElem :: Gen String
genArrayElem = do
  c1 <- genIdent
  c2 <- genBrackets
  return $ c1 ++ c2
  where
    genBracket = do c <- genExpr; return $ "[" ++ c ++ "]"
    genBrackets = sized $ \n -> someN genBracket (limit n)

parse' :: T.Parsec a -> String -> T.Result String a
parse' = T.parse

check :: T.Parsec a -> String -> P.Result
check parser str = case parse' (fully parser) str of
  T.Success _ -> P.succeeded
  T.Failure err -> P.failed {P.reason = "Failed to parse " ++ err}

check' :: T.Parsec a -> Gen String -> Property
check' parser gen = withMaxSuccess 100 $ forAll gen $ check parser

test = testProperty "can parse intLiter" $ check' intLiter genIntLiter

test = testProperty "can parse boolLiter" $ check' boolLiter genBoolLiter

test = testProperty "can parse charLiter" $ check' charLiter genCharLiter

test = testProperty "can parse stringLiter" $ check' stringLiter genStringLiter

test = testProperty "can parse pairLiter" $ check' pairLiter genPairLiter

test = testProperty "can parse ident" $ check' ident genIdent

test = testProperty "can parse expr" $ check' expr genExpr
