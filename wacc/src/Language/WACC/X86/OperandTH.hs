{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.WACC.X86.OperandTH where

import Data.Char (toLower)
import Language.Haskell.TH
  ( Body (NormalB)
  , Con (GadtC)
  , Dec (DataD, SigD, ValD)
  , Exp (AppE, ConE)
  , Info (TyConI)
  , Pat (VarP)
  , Q
  , Type (..)
  , mkName
  , nameBase
  , reify
  )
import Language.WACC.X86.Operand (OpType (RM), Operand (Reg))
import Language.WACC.X86.Register (Register (..))

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
