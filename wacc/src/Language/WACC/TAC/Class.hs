{-# LANGUAGE TypeFamilies #-}

{- |
Type class for translating AST nodes into three-address code.
-}
module Language.WACC.TAC.Class (ToTAC (..)) where

{- |
AST nodes which can be translated into three-address code.
-}
class ToTAC a where
  -- | Three-address code representation type (from "Language.WACC.TAC.TAC").
  type TACRepr a

  -- | Translate an AST node.
  toTAC :: a -> TACRepr a
