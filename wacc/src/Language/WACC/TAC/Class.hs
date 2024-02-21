{-# LANGUAGE TypeFamilies #-}

{- |
Type class for translating AST nodes into three-address code.
-}
module Language.WACC.TAC.Class (ToTAC (..)) where

import Language.WACC.TAC.State (TACM)

{- |
AST nodes which can be translated into three-address code.
-}
class ToTAC a where
  -- | Three-address code representation type (from "Language.WACC.TAC.TAC").
  type TACRepr a

  -- | Identifier type.
  type TACIdent a

  -- | Label type.
  type TACLIdent a

  -- | Translate an AST node.
  toTAC :: a -> TACM (TACIdent a) (TACLIdent a) (TACRepr a)
