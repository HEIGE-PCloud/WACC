{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.WACC.X86.OperandTH where

import Control.Monad (replicateM)
import Data.Char (toLower)
import Language.Haskell.TH
  ( Body (NormalB)
  , Clause (Clause)
  , Con (ForallC, GadtC)
  , Dec (DataD, FunD, SigD, ValD)
  , Exp (AppE, ConE, VarE)
  , Info (TyConI)
  , Name
  , Pat (VarP)
  , Q
  , Quote (newName)
  , Type (..)
  , mkName
  , nameBase
  , reify
  )
import Language.WACC.X86.Operand (OpType (RM), Operand (Reg))
import Language.WACC.X86.Register (Register (..))
import Language.WACC.X86.X86 (Instruction)

-- Assuming Size and OpType are defined elsewhere, along with Operand, Register, etc.

-- Generate expressions for each Register constructor
genRegOperand :: Q [Dec]
genRegOperand = do
  -- Retrieve information about the 'Register' type
  TyConI (DataD _ _ _ _ constructors _) <- reify ''Register
  -- For each constructor, generate the corresponding 'Operand' expression and type signature
  concat <$> mapM mkOperandDec constructors
  where
    mkOperandDec :: Con -> Q [Dec]
    mkOperandDec (GadtC [name] _ (AppT (ConT _) size)) = do
      let
        -- Create the name for the variable (e.g., "rax" from "Rax")
        varName = mkName . map toLower . nameBase $ name
        -- Create the expression 'Reg <Constructor>'
        expr = NormalB $ AppE (ConE 'Reg) (ConE name)
        -- Assuming the first type argument of the constructor is the size
        -- and constructing the type 'Operand size RM'
        operandType = AppT (AppT (ConT ''Operand) size) (ConT 'RM)
        -- Generate type signature: varName :: Operand size RM
        sig = SigD varName operandType
        -- Generate value binding: varName = Reg <Constructor>
        val = ValD (VarP varName) expr []
      return [sig, val]
    mkOperandDec _ = error "Unsupported constructor type"

genOperandWrappers :: Name -> Q [Dec]
genOperandWrappers funcName = do
  -- Retrieve information about the 'Instruction' type
  TyConI (DataD _ _ _ _ constructors _) <- reify ''Instruction
  -- For each constructor, generate the corresponding wrapper function
  mapM (mkWrapper funcName) (filter' constructors)
  where
    filter' = filterJumps

constructorName :: Con -> String
constructorName (GadtC [name] _ _) = nameBase name
constructorName (ForallC _ _ (GadtC [name] _ _)) = nameBase name
constructorName x = error ("Unsupport construtorName" ++ show x)

mkWrapper :: Name -> Con -> Q Dec
mkWrapper funcName (GadtC [name] args _) =
  mkWrapper' funcName name args
mkWrapper funcName (ForallC _ _ (GadtC [name] args _)) =
  mkWrapper' funcName name args
mkWrapper x y = fail (show x ++ "\n\n" ++ show y)

mkWrapper' :: (Foldable t, Quote m) => Name -> Name -> t a -> m Dec
mkWrapper' funcName name args = do
  -- Generate names for the function arguments
  argNames <- replicateM (length args) (newName "arg")
  let
    -- Create patterns for the function arguments
    patterns = map VarP argNames
    -- Create expressions for the function arguments
    exprs = map VarE argNames
    -- Create the expression that applies the constructor to the arguments
    constructorExpr = foldl AppE (ConE name) exprs
    -- Create the body of the function that applies 'foo' to the constructor expression
    body = NormalB $ AppE (VarE funcName) constructorExpr
    -- Combine everything into a function declaration
    funcDec = FunD (mkName (map toLower (nameBase name))) [Clause patterns body []]
  return funcDec

filterJumps :: [Con] -> [Con]
filterJumps = filter (\x -> head (constructorName x) /= 'J' && constructorName x /= "Call")
