{- |
Type checking monad and actions.
-}
module Language.WACC.TypeChecking.State
  ( TypingM
  , typeOf
  , fixType
  , tryUnify
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, gets, modify)
import Data.Map (Map, findWithDefault, insert)
import Language.WACC.AST (WType)
import Language.WACC.TypeChecking.BType (BType (BUnknown), fix, unify)

{- |
Type checking state monad.
-}
type TypingM ident = StateT (Map ident BType) Maybe

{- |
Look up the type of an identifier.

Returns 'BUnknown' if the identifier cannot be found.
-}
typeOf :: (Ord ident) => ident -> TypingM ident BType
typeOf v = gets (findWithDefault BUnknown v)

{- |
Fix the type of an identifier.
-}
fixType :: (Ord ident) => ident -> WType -> TypingM ident ()
fixType v t = modify (insert v $ fix t)

{- |
@tryUnify actT expT@ attempts to unify an actual type @actT@ with an expected
type @expT@, aborting the type check on failure.
-}
tryUnify :: BType -> BType -> TypingM ident BType
tryUnify actT expT = lift $ unify actT expT
