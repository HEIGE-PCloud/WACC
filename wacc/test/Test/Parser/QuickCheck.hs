{- AUTOCOLLECT.TEST -}

module Test.Parser.QuickCheck
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.Functor
import Data.List
import Language.WACC.Parser.Expr
import Language.WACC.Parser.Prog
import Language.WACC.Parser.Stmt
import Language.WACC.Parser.Token (fully, keywords)
import Language.WACC.Parser.Type
import qualified Test.QuickCheck.Property as P
import Test.Tasty.QuickCheck
import qualified Text.Gigaparsec as T
import qualified Text.Gigaparsec.Char as TC

justParse :: T.Parsec [Char]
justParse = T.many TC.item

optional :: Gen String -> Gen String
optional gen = frequency [(1, gen), (1, return "")]

many :: Gen String -> Gen String
many gen = listOf gen <&> concat

-- someN :: Gen String -> Int -> Gen String
someN :: Gen String -> Int -> Gen String
someN gen n = do
  k <- choose (1, max 1 n)
  concat <$> vectorOf k gen

someSize :: Gen String -> Gen String
someSize gen = sized $ \n -> someN gen n

intMin :: Int
intMin = -2147483648

intMax :: Int
intMax = 2147483647

genIntLiter :: Gen String
genIntLiter = choose (intMin, intMax) <&> show

genBoolLiter :: Gen String
genBoolLiter = elements ["true", "false"]

genCharLiter :: Gen String
genCharLiter = do
  c <- genCharacter
  return $ "\'" ++ c ++ "\'"

genStringLiter :: Gen String
genStringLiter = do
  c <- someSize genCharacter
  return $ "\"" ++ c ++ "\""

escapedChar :: [String]
escapedChar = ["0", "b", "t", "n", "f", "r", "\"", "'", "\\"]

graphicASCII :: [Char]
graphicASCII = ['\32' .. '\126']

genCharacter :: Gen String
genCharacter =
  elements $
    (map (: []) graphicASCII \\ escapedChar) ++ ["\\" ++ c | c <- escapedChar]

genPairLiter :: Gen String
genPairLiter = return "null"

genIdent :: Gen String
genIdent = genIdent' `suchThat` (`notElem` keywords)
  where
    genIdent' = sized $ \k -> do
      c <- elements $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
      cs <- vectorOf k $ elements $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
      return $ c : cs

-- genComment :: Gen String
-- genComment = sized $ \k -> do
--   c <- elements ['#']
--   cs <- vectorOf k $ elements $ graphicASCII ++ ['\n']
--   return $ c : cs

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
  c2 <- intercalate "\n ;" <$> listOf1 (genStmt (depth - 1))
  return ("begin\n" ++ c1 ++ "\n" ++ c2 ++ "\nend\n")

genFunc :: Int -> Gen String
genFunc depth = do
  c1 <- genParam (depth - 1)
  c2 <- optional (genParamList (depth - 1))
  c3 <- genStmt (depth - 1)
  return (c1 ++ "(" ++ c2 ++ ")" ++ "is\n" ++ c3 ++ "\nend\n")

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
        , genExpr'' "free"
        , genExpr'' "return"
        , genExpr'' "exit"
        , genExpr'' "print"
        , genExpr'' "println"
        , genIf
        , genWhile
        , genBegin
        , genSeq
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
    genExpr'' str = do
      c1 <- genExpr (depth - 1)
      return $ str ++ " " ++ c1
    genIf = do
      c1 <- genExpr (depth - 1)
      c2 <- genStmt (depth - 1)
      c3 <- genStmt (depth - 1)
      return $ "if\n" ++ c1 ++ "\nthen\n" ++ c2 ++ "\nelse\n" ++ c3 ++ "\nfi"
    genWhile = do
      c1 <- genExpr (depth - 1)
      c2 <- genStmt (depth - 1)
      return $ "while\n" ++ c1 ++ "\ndo\n" ++ c2 ++ "\ndone"
    genBegin = do
      c1 <- genStmt (depth - 1)
      return $ "begin\n" ++ c1 ++ "\nend"
    genSeq = do
      c1 <- genStmt (depth - 1)
      c2 <- genStmt (depth - 1)
      return $ c1 ++ "\n;\n" ++ c2

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
      return $ "call " ++ c1 ++ "(" ++ c2 ++ ")"

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
      c1 <- genLvalue (depth - 1)
      return $ str ++ " " ++ c1

genArrayLiter :: Int -> Gen String
genArrayLiter depth = do
  c1 <- many (genArgs depth)
  c2 <- genExpr (depth - 1)
  c3 <- optional (pure (c2 ++ c1))
  return ("[" ++ c3 ++ "]")

parse' :: T.Parsec a -> String -> T.Result String a
parse' = T.parse

check :: T.Parsec a -> String -> P.Result
check parser str = case parse' (fully parser) str of
  T.Success _ -> P.succeeded
  T.Failure err -> P.failed {P.reason = "Failed to parse " ++ err}

check' :: T.Parsec a -> Gen String -> Property
check' parser gen = withMaxSuccess 2000 $ forAll gen $ check parser

test = testProperty "can parse intLiter" $ check' intLiter genIntLiter

test = testProperty "can parse boolLiter" $ check' boolLiter genBoolLiter

test = testProperty "can parse charLiter" $ check' charLiter genCharLiter

test = testProperty "can parse stringLiter" $ check' stringLiter genStringLiter

test = testProperty "can parse pairLiter" $ check' pairLiter genPairLiter

test = testProperty "can parse ident" $ check' arrOrIdent genIdent

test =
  testProperty "can parse arrayElem" $ check' arrOrIdent $ sized genArrayElem

test = testProperty "can parse atom" $ check' atom $ sized genAtom

test = testProperty "can parse expr" $ check' expr $ sized genExpr

test = testProperty "can parse type" $ check' wType $ sized genType

test = testProperty "can parse baseType" $ check' wBaseType genBaseType

test =
  testProperty "can parse arrayType" $
    check' (wType) $
      sized genArrayType

test = testProperty "can parse pairType" $ check' wPairType $ sized genPairType

test =
  testProperty "can parse pairElemType" $
    check' pairElemType $
      sized genPairElemType

test_ignoreTest = testProperty "can parse program" $ check' prog $ sized genProgram

test_ignoreTest = testProperty "can parse func" $ check' func $ sized genFunc

-- test = testProperty "can parse paramList" $ check' paramList $ sized genParamList

test = testProperty "can parse param" $ check' param $ sized genParam

test = testProperty "can parse stmt" $ check' stmts $ sized genStmt

test = testProperty "can parse lvalue" $ check' lValue $ sized genLvalue

test = testProperty "can parse rvalue" $ check' rValue $ sized genRvalue

test = testProperty "can parse arrayLiter" $ check' arrayLit $ sized genArrayLiter
