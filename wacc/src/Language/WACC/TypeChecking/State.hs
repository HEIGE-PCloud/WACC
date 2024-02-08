{-# LANGUAGE OverloadedLists #-}

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
  , tryUnify
  , TypeError (..)
  , reportAt
  , abortWithArityError
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.RWS (RWS, asks, gets, modify, runRWS, tell)
import Data.DList (DList)
import Data.Map (Map, insert, (!?))
import Language.WACC.TypeChecking.BType (BType, FnType, unify)
import Text.Gigaparsec.Position (Pos)

{- |
A type error found during type checking.
-}
data TypeError
  = -- | Incompatible types.
    IncompatibleTypesError
      BType
      -- ^ The type of the provided value.
      BType
      -- ^ The expected type.
      Pos
      -- ^ The position of the ill-typed expression or statement.
  | -- | Invalid number of parameters in function call.
    FunctionCallArityError
      Int
      -- ^ The arity of the ill-typed function call.
      Int
      -- ^ The declared arity of the function.
      Pos
      -- ^ The position of the ill-typed function call.

type TypeErrors = DList TypeError

type FnTypes fnident = Map fnident FnType

{- |
Type checking monad.
-}
type TypingM fnident ident =
  ExceptT (Maybe BType) (RWS (ident -> BType) TypeErrors (FnTypes fnident))

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
abort = throwE Nothing

{- |
Abort a type check, saving an invalid actual type.
-}
abortActual :: BType -> TypingM fnident ident a
abortActual = throwE . pure

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
  :: Pos -> BType -> TypingM fnident ident a -> TypingM fnident ident a
reportAt p expT action = catchE action report
  where
    report (Just actT) =
      lift (tell [IncompatibleTypesError actT expT p]) *> abort
    report _ = abort

{- |
Report a 'FunctionCallArityError' and abort.
-}
abortWithArityError :: Int -> Int -> Pos -> TypingM fnident ident a
abortWithArityError actN expN p =
  lift (tell [FunctionCallArityError actN expN p]) *> abort
