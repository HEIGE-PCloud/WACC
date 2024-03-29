{-# LANGUAGE TypeFamilies #-}

{- |
Type class for type checked AST nodes.
-}
module Language.WACC.TypeChecking.Class
  ( TypeChecked (..)
  , FnTypeChecked (..)
  , Typed
  , TypingIdent
  )
where

import Language.WACC.TypeChecking.State (TypingM)

{- |
Re-annotated AST types.
-}
type family Typed t

{- |
Identifier type used by 'TypeChecked' and 'FnTypeChecked'
-}
type family TypingIdent t where
  TypingIdent (t a b ident ann) = ident
  TypingIdent (t a ident ann) = ident
  TypingIdent (t ident ann) = ident

{- |
Type checked AST nodes without function identifiers.
-}
class TypeChecked t where
  -- | Type check an AST node.
  check :: t -> TypingM fnident (TypingIdent t) (Typed t)

{- |
Type checked AST nodes containing function identifiers.
-}
class FnTypeChecked t where
  -- | Function identifier type.
  type TypingFnIdent t

  -- | Type check an AST node.
  fnCheck :: t -> TypingM (TypingFnIdent t) (TypingIdent t) (Typed t)
