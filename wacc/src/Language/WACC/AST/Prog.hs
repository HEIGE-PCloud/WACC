{- |
WACC programs.
-}
module Language.WACC.AST.Prog (Prog (..), Func (..)) where

import Language.WACC.AST.Stmt (Stmts)

{- |
WACC programs.
-}
data Prog ann typ fnident ident
  = -- | > begin <func> ... <stmt> end
    Main [Func ann typ fnident ident] (Stmts ann fnident ident) ann
  deriving (Eq, Show)

{- |
WACC function definitions.
-}
data Func ann typ fnident ident
  = -- | > <type> <ident>(<type> <ident>, ...) is <stmt> end
    Func
      typ
      fnident
      [(typ, ident)]
      (Stmts ann fnident ident)
      ann
  deriving (Eq, Show)
