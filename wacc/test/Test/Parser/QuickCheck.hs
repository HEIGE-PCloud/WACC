{- AUTOCOLLECT.TEST -}

module Test.Parser.QuickCheck
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.List ((\\))
import Test.Tasty.QuickCheck
import Language.WACC.Parser.Token (fully)
import Language.WACC.Parser.Expr
import Text.Gigaparsec as T
import qualified Test.QuickCheck.Property as P
import Language.WACC.Parser.Prog (prog)

optional :: Gen String -> Gen String
optional gen = frequency [(1, gen), (1, return "")]

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

genCharacter :: Gen String
genCharacter = elements $ (map (:[]) ['\32' .. '\126'] \\ escapedChars) ++ ["\\" ++ c | c <- escapedChars]

genPairLiter :: Gen String
genPairLiter = return "null"

genIdent :: Gen String
genIdent = do
  c <- elements $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
  cs <- listOf $ elements $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
  return $ c : cs

genComment :: Gen String
genComment = do
  c <- elements ['#']
  cs <- listOf1 $ elements $ ['\32' .. '\126'] ++ ['\n']
  return $ c : cs

parse' :: Parsec a -> String -> Result String a
parse' = T.parse

check :: Parsec a -> Gen String -> Gen P.Result
check parser gen = do
  g <- gen
  return $ case parse' (fully parser) g of
    T.Success _ -> P.succeeded
    T.Failure _ -> P.failed{P.reason = "Failed to parse " ++ g}

test = testProperty "can parse intLiter"  $ check intLiter genIntLiter
test = testProperty "can parse boolLiter" $ check boolLiter genBoolLiter
test = testProperty "can parse charLiter" $ check charLiter genCharLiter
test = testProperty "can parse stringLiter" $ check stringLiter genStringLiter
test = testProperty "can parse pairLiter" $ check pairLiter genPairLiter
test = testProperty "can parse ident" $ check ident genIdent
