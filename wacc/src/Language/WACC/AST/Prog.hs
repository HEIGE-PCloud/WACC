{-# LANGUAGE DeriveFunctor #-}

{- |
WACC programs.
-}
module Language.WACC.AST.Prog (Prog (..), Func (..)) where

import Language.WACC.AST.Stmt (Stmts)

{- |
WACC programs.
-}
data Prog typ fnident ident ann
  = -- | > begin <func> ... <stmt> end
    Main [Func typ fnident ident ann] (Stmts fnident ident ann) ann
  deriving (Eq, Functor, Show)

{- |
WACC function definitions.
-}
data Func typ fnident ident ann
  = -- | > <type> <ident>(<type> <ident>, ...) is <stmt> end
    Func
      typ
      fnident
      [(typ, ident)]
      (Stmts fnident ident ann)
      ann
  deriving (Eq, Functor, Show)
