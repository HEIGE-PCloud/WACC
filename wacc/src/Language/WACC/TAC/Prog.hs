{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WACC.TAC.Prog where

import qualified Language.WACC.AST.Prog as AST
import Language.WACC.TAC.Class
import Language.WACC.TAC.State
import Language.WACC.TAC.Stmt
import Language.WACC.TAC.TAC
  ( BasicBlock (..)
  , Jump (..)
  , TAC (LoadCI)
  , TACFunc (..)
  , Var (..)
  )
import Language.WACC.TypeChecking (BType)

type instance TACIdent (AST.Func typ fnident ident BType) = ident

instance
  (Enum fnident, Enum ident, Eq ident, Ord fnident)
  => FnToTAC (AST.Func typ fnident ident BType)
  where
  type TACFnRepr (AST.Func typ fnident ident BType) = ()
  type TACFnIdent (AST.Func typ fnident ident BType) = fnident
  fnToTAC (AST.Func _ fnIdent params stmts _) = do
    let
      paramVars = Var . snd <$> params
    stmtsThen stmts fnIdent $
      error "Function did not end with return or exit statement."
    funcBlocks <- collectBlocks
    putFunc fnIdent (TACFunc fnIdent paramVars funcBlocks)

type instance TACIdent (AST.Prog typ fnident ident BType) = ident

instance
  (Enum fnident, Enum ident, Eq ident, Num fnident, Ord fnident)
  => FnToTAC (AST.Prog typ fnident ident BType)
  where
  type TACFnRepr (AST.Prog typ fnident ident BType) = ()
  type TACFnIdent (AST.Prog typ fnident ident BType) = fnident
  fnToTAC (AST.Main funcs stmts _) = do
    mapM_ fnToTAC funcs
    completeMain <- stmtsToTAC stmts 0
    implicitExitLabel <- freshLabel
    completeMain $ Jump implicitExitLabel
    implicitExitCodeVar <- freshTemp
    appendBlock
      ( BasicBlock
          [LoadCI implicitExitCodeVar implicitExitCode]
          (Exit implicitExitCodeVar)
      )
      implicitExitLabel
    mainBlocks <- collectBlocks
    putFunc mainLabel (TACFunc mainLabel [] mainBlocks)
    where
      mainLabel = 0
      implicitExitCode = 0
