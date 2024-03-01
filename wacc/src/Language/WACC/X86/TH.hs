{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.WACC.X86.TH where

import Data.Char (toLower)
import Data.List (intercalate)
import Language.Haskell.TH

class ATNT a where
  formatA :: a -> String

-- Utility function to convert names to lowercase strings
constructorNameToLower :: Name -> String
constructorNameToLower = map toLower . nameBase

-- Generate formatA method cases based on constructor arity
genATNT :: Name -> Q [Dec]
genATNT typeName = do
  TyConI (DataD _ _ _ _ constructors _) <- reify typeName
  clauses <- mapM generateClause constructors
  let
    formatAMethod = FunD 'formatA clauses
  return
    [InstanceD Nothing [] (AppT (ConT ''ATNT) (ConT typeName)) [formatAMethod]]

generateClause :: Con -> Q Clause
generateClause (GadtC [name] _ _) = do
  let
    opName = constructorNameToLower name
  argName <- newName "arg"
  let
    pat = conP name [varP argName]
  let
    body = normalB [|opName ++ " " ++ formatA $(varE argName)|]
  clause [pat] body []
generateClause (ForallC _ _ (GadtC [name] args _)) = do
  let
    opName = constructorNameToLower name
  argNames <- mapM (\_ -> newName "arg") args
  let
    pat = conP name (map varP argNames)
  let
    argsExps = map (\n -> [|formatA $(varE n)|]) argNames

  let
    body = normalB $ case length args of
      0 -> [|opName|]
      1 -> [|opName ++ " " ++ concat $(listE argsExps)|]
      _ -> [|opName ++ " " ++ intercalate ", " $(listE argsExps)|]
  clause [pat] body []
generateClause xs = fail $ "Unsupported constructor pattern" ++ show xs

inspectCode :: Q [Dec] -> Q [Dec]
inspectCode genCode = do
  code <- genCode
  let
    codeStr = pprint code
  runIO $ putStrLn codeStr -- Print the code at compile time
  return code -- Return the original code to not affect the compilation
