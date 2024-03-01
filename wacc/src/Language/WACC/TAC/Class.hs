{-# LANGUAGE TypeFamilies #-}

{- |
Type class for translating AST nodes into three-address code.
-}
module Language.WACC.TAC.Class (ToTAC (..), FnToTAC (..), TACIdent) where

import Language.WACC.TAC.State (TACM)

{- |
Identifier type used by 'ToTAC' and 'FnToTAC'.
-}
type family TACIdent a

{- |
AST nodes without function identifiers which can be translated into
three-address code.
-}
class ToTAC a where
  -- | Three-address code representation type (from "Language.WACC.TAC.TAC").
  type TACRepr a lident

  -- | Translate an AST node.
  toTAC :: (Ord lident) => a -> TACM (TACIdent a) lident (TACRepr a lident)

{- |
AST nodes containing function identifiers which can be translated into
three-address code.
-}
class FnToTAC a where
  -- | Three-address code representation type (from "Language.WACC.TAC.TAC").
  type TACFnRepr a

  -- | Function identifier type.
  type TACFnIdent a

  -- | Translate an AST node.
  fnToTAC :: a -> TACM (TACIdent a) (TACFnIdent a) (TACFnRepr a)
