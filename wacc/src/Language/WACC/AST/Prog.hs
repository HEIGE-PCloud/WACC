{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
WACC programs.
-}
module Language.WACC.AST.Prog (Prog (..), Func (..)) where

import Language.WACC.AST.Stmt (Stmts)
import Language.WACC.AST.WType (WType)
import Text.Gigaparsec.Position (Pos)

{- |
WACC programs.
-}
data Prog fnident ident
  = -- | > 'begin' (func)* stmt 'end'
    Main [Func fnident ident] (Stmts fnident ident)
  deriving (Eq, Show)

{- |
WACC Function definition and body
-}
data Func fnident ident
  = -- | > <type> <ident>(<type> <ident>, ...) is <stmt> end
    Func
      WType
      fnident
      [(WType, ident)]
      (Stmts fnident ident)
      Pos
  deriving (Eq, Show)
