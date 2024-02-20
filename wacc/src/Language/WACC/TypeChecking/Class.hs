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

import Language.WACC.TypeChecking.BType (BType)
import Language.WACC.TypeChecking.State (TypingM)
import Text.Gigaparsec.Position (Pos)

{- |
Re-annotated AST types.
-}
type family Typed t where
  Typed (t a b c Pos) = t a b c BType
  Typed (t a b Pos) = t a b BType
  Typed (t a Pos) = t a BType

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