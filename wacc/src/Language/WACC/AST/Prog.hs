{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
WACC programs.
-}
module Language.WACC.AST.Prog (Prog (..)) where

import Language.WACC.AST.Stmt (Stmts)
import Language.WACC.AST.WType (WType)

{- |
WACC programs.
-}
data Prog fnident ident
  = -- | > <type> <ident>(<type> <ident>, ...) is <stmt> end
    Func
      WType
      fnident
      [(WType, ident)]
      (Stmts fnident ident)
      (Prog fnident ident)
  | -- | Main program.
    Main (Stmts fnident ident)
  deriving (Eq, Show)
