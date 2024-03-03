{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoOverloadedLists #-}

module Language.WACC.TAC.Prog where

-- Import any necessary modules here

-- Define your functions and types here

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
  fnToTAC (AST.Func _ fid params stmts _) = do
    let
      vars = Var . snd <$> params
    f <- stmtsToTAC stmts fid
    f $ error "Function did not end with return or exit statement."
    m <- collectBlocks
    putFunc fid (TACFunc fid vars m)

type instance TACIdent (AST.Prog typ fnident ident BType) = ident

instance
  (Enum fnident, Enum ident, Eq ident, Num fnident, Ord fnident)
  => FnToTAC (AST.Prog typ fnident ident BType)
  where
  type TACFnRepr (AST.Prog typ fnident ident BType) = ()
  type TACFnIdent (AST.Prog typ fnident ident BType) = fnident
  fnToTAC (AST.Main funcs stmts _) = do
    mapM_ fnToTAC funcs
    f <- stmtsToTAC stmts 0
    fl <- freshLabel
    f $ Jump fl
    t <- freshTemp
    appendBlock (BasicBlock [LoadCI t 0] (Exit t)) fl
    bs <- collectBlocks
    putFunc 0 (TACFunc 0 [] bs)
