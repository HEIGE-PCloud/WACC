{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.WACC.X86.ATNT where

import Data.Char (toLower)
import Data.List (intercalate)
import Language.Haskell.TH

class ATNT a where
  formatA :: a -> String

-- Utility function to convert names to lowercase strings
nameToLower :: Name -> String
nameToLower = map toLower . nameBase

-- Generate formatA method cases based on constructor arity
genATNTInstruction :: Name -> Q [Dec]
genATNTInstruction typeName = do
  TyConI (DataD _ _ _ _ constructors _) <- reify typeName
  clauses <- mapM generateClause constructors
  let
    formatAMethod = FunD 'formatA clauses
  return
    [InstanceD Nothing [] (AppT (ConT ''ATNT) (ConT typeName)) [formatAMethod]]

generateGadtC :: (Quote m) => [Name] -> [a] -> m Clause
generateGadtC ns args = do
  argNames <- mapM (\_ -> newName "arg") args
  let
    name = head ns
  let
    opName = nameToLower name
  let
    pat = conP name (map varP argNames)
  let
    argsExps = listE $ map (\n -> [|formatA $(varE n)|]) argNames
  let
    spacing = if null argNames then "" else " "
  let
    body = case opName of
      "dir" -> normalB [|concat $(argsExps)|]
      "lab" -> normalB [|concat $(argsExps) ++ ":"|]
      "comment" -> normalB [|"# " ++ concat $(argsExps)|]
      _ -> normalB [|opName ++ spacing ++ intercalate ", " $(argsExps)|]
  clause [pat] body []

generateClause :: Con -> Q Clause
generateClause (GadtC ns args _) = generateGadtC ns args
generateClause (ForallC _ _ (GadtC ns args _)) = generateGadtC ns args
generateClause xs = fail $ "Unsupported constructor pattern" ++ show xs

inspectCode :: Q [Dec] -> Q [Dec]
inspectCode genCode = do
  code <- genCode
  let
    codeStr = pprint code
  runIO $ putStrLn codeStr -- Print the code at compile time
  return code -- Return the original code to not affect the compilation
