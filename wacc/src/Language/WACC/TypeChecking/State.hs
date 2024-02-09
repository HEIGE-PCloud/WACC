{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

{- |
Type checking monad and actions.
-}
module Language.WACC.TypeChecking.State
  ( TypingM
  , runTypingM
  , typeOf
  , typeOfFn
  , setFnType
  , abort
  , abortActual
  , abortOverridePos
  , abortActualOverridePos
  , tryUnify
  , TypeError (..)
  , TypeErrors
  , reportAt
  , abortWith
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.RWS (RWS, asks, gets, modify, runRWS, tell)
import Data.DList (DList)
import Data.Map (Map, insert, (!?))
import Data.Maybe (fromMaybe)
import Language.WACC.AST (HasPos (getPos))
import Language.WACC.TypeChecking.BType (BType, FnType, unify)
import Language.WACC.TypeChecking.Error
import Text.Gigaparsec.Position (Pos)

{- |
Difference list used to collect multiple type errors.
-}
type TypeErrors = DList TypeError

type FnTypes fnident = Map fnident FnType

{- |
Type checking monad.
-}
type TypingM fnident ident =
  ExceptT
    (Maybe BType, Maybe Pos)
    (RWS (ident -> BType) TypeErrors (FnTypes fnident))

{- |
Run a type checking action.
-}
runTypingM
  :: TypingM fnident ident a
  -> (ident -> BType)
  -> FnTypes fnident
  -> (Maybe a, FnTypes fnident, TypeErrors)
runTypingM action typeOfIdent fnTypes = (mx, fts, es)
  where
    (ex, fts, es) = runRWS (runExceptT action) typeOfIdent fnTypes
    mx = case ex of
      Right x -> Just x
      _ -> Nothing

{- |
Look up the type of an identifier.
-}
typeOf :: ident -> TypingM fnident ident BType
typeOf v = lift $ asks ($ v)

{- |
Look up the type of a function.
-}
typeOfFn :: (Ord fnident) => fnident -> TypingM fnident ident FnType
typeOfFn f = lift (gets (!? f)) >>= maybe abort pure

{- |
Set the type of a function.
-}
setFnType :: (Ord fnident) => fnident -> FnType -> TypingM fnident ident ()
setFnType f t = lift $ modify (insert f t)

{- |
Abort a type check.
-}
abort :: TypingM fnident ident a
abort = throwE (Nothing, Nothing)

{- |
Abort a type check, overriding the position of the type error.
-}
abortOverridePos :: (HasPos a) => a -> TypingM fnident ident b
abortOverridePos = throwE . (Nothing,) . pure . getPos

{- |
Abort a type check, saving an invalid actual type.
-}
abortActual :: BType -> TypingM fnident ident a
abortActual = throwE . (,Nothing) . pure

{- |
Abort a type check, saving an invalid actual type and overriding the position of
the type error.
-}
abortActualOverridePos :: (HasPos a) => BType -> a -> TypingM fnident ident b
abortActualOverridePos t x = throwE (pure t, pure $ getPos x)

{- |
@tryUnify actT expT@ attempts to unify an actual type @actT@ with an expected
type @expT@, aborting the type check on failure.
-}
tryUnify :: BType -> BType -> TypingM fnident ident BType
tryUnify actT expT = maybe (abortActual actT) pure $ unify actT expT

{- |
@reportAt p expT action@ runs @action@ and reports a 'TypeError' at @p@ if the
action is aborted with a saved invalid actual type (see 'abortActual').
-}
reportAt
  :: (HasPos a)
  => a
  -> BType
  -> TypingM fnident ident b
  -> TypingM fnident ident b
reportAt x expT action = catchE action report
  where
    report (Just actT, mp) =
      lift (tell [IncompatibleTypesError actT expT $ fromMaybe p mp]) *> abort
    report _ = abort
    p = getPos x

{- |
Report a specialised 'TypeError' and abort.
-}
abortWith :: TypeError -> TypingM fnident ident a
abortWith err = lift (tell [err]) *> abort
