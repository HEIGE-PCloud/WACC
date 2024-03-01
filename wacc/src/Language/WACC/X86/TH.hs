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

generateGadtC :: (Quote m) => [Name] -> [a] -> m Clause
generateGadtC ns args = do
  let
    name = head ns
  let
    opName = constructorNameToLower name
  argNames <- mapM (\_ -> newName "arg") args
  let
    pat = conP name (map varP argNames)
  let
    argsExps = map (\n -> [|formatA $(varE n)|]) argNames
  case opName of
    "lab" -> clause [pat] (normalB [|concat $(listE argsExps) ++ ":"|]) []
    "comment" -> clause [pat] (normalB [|"# " ++ concat $(listE argsExps)|]) []
    _ -> do
      let
        body = normalB [|opName ++ " " ++ intercalate ", " $(listE argsExps)|]
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
