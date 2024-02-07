{- |
Type checking monad and actions.
-}
module Language.WACC.TypeChecking.State
  ( TypingM
  , typeOf
  , typeOfFn
  , setFnType
  , tryUnify
  , TypeError (..)
  , abort
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.RWS (RWS, asks, gets, modify)
import Data.DList (DList)
import Data.Map (Map, insert, unionWith, (!))
import Language.WACC.TypeChecking.BType (BType, FnType, unify)
import Text.Gigaparsec.Position (Pos)

data TypeError = TypeError {actualType :: BType, pos :: Pos}

newtype TypeErrors = TypeErrors (Map Integer (DList TypeError))

instance Semigroup TypeErrors where
  TypeErrors es1 <> TypeErrors es2 = TypeErrors (unionWith (<>) es1 es2)

instance Monoid TypeErrors where
  mempty = TypeErrors mempty

{- |
Type checking monad.
-}
type TypingM fnident ident =
  ExceptT () (RWS (ident -> BType) TypeErrors (Map fnident FnType))

{- |
Look up the type of an identifier.
-}
typeOf :: ident -> TypingM fnident ident BType
typeOf v = lift $ asks ($ v)

{- |
Look up the type of a function.
-}
typeOfFn :: (Ord fnident) => fnident -> TypingM fnident ident FnType
typeOfFn f = lift $ gets (! f)

{- |
Set the type of a function.
-}
setFnType :: (Ord fnident) => fnident -> FnType -> TypingM fnident ident ()
setFnType f t = lift $ modify (insert f t)

{- |
Abort a type check.
-}
abort :: TypingM fnident ident a
abort = throwE ()

{- |
@tryUnify actT expT@ attempts to unify an actual type @actT@ with an expected
type @expT@, aborting the type check on failure.
-}
tryUnify :: BType -> BType -> TypingM fnident ident BType
tryUnify actT expT = maybe abort pure $ unify actT expT
