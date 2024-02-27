{-# LANGUAGE NamedFieldPuns #-}

{- |
TAC translation monad and actions.
-}
module Language.WACC.TAC.State
  ( TACM
  , runTACM
  , evalTACM
  , freshTemp
  , freshLabel
  , putTACs
  , collectTACs
  , completeBlock
  , getTarget
  , into
  )
where

import Control.Monad.Trans.RWS (RWS, ask, evalRWS, gets, local, modify, tell)
import Data.DList (DList, toList)
import Language.WACC.TAC.TAC (BasicBlock (..), Jump (..), TAC, Var (Temp))

data TACMState ident lident = TACMState
  { nextTemp :: ident
  , nextLabel :: lident
  , tacs :: DList (TAC ident lident)
  }

type Blocks ident lident = DList (BasicBlock ident lident)

{- |
TAC translation monad.
-}
type TACM ident lident =
  RWS (Var ident) (Blocks ident lident) (TACMState ident lident)

{- |
Run a TAC translation action.

Basic block labels are allocated starting from the given @lident@.
-}
runTACM
  :: (Num ident) => lident -> TACM ident lident a -> (a, Blocks ident lident)
runTACM l action = evalRWS action (Temp (-1)) (TACMState 0 l mempty)

{- |
Run a TAC translation action, returning only the generated basic blocks.
-}
evalTACM :: (Num ident) => lident -> TACM ident lident a -> Blocks ident lident
evalTACM l = snd . runTACM l

{- |
Get a fresh temporary variable.
-}
freshTemp :: (Enum ident) => TACM ident lident (Var ident)
freshTemp = Temp <$> gets nextTemp <* modify incrTemp
  where
    incrTemp st@TACMState {nextTemp} = st {nextTemp = succ nextTemp}

{- |
Get a fresh basic block label.
-}
freshLabel :: (Enum lident) => TACM ident lident lident
freshLabel = gets nextLabel <* modify incrLabel
  where
    incrLabel st@TACMState {nextLabel} = st {nextLabel = succ nextLabel}

{- |
Append some TAC instructions to the state.
-}
putTACs :: DList (TAC ident lident) -> TACM ident lident ()
putTACs ts = modify appendTACs
  where
    appendTACs st@TACMState {tacs} = st {tacs = tacs <> ts}

{- |
Collect TAC instructions from the state.
-}
collectTACs :: TACM ident lident (DList (TAC ident lident))
collectTACs = gets tacs <* modify dropTACs
  where
    dropTACs st = st {tacs = mempty}

{- |
Collect TAC instructions from the state into a basic block.
-}
completeBlock :: Jump ident lident -> TACM ident lident ()
completeBlock j = do
  ts <- collectTACs
  tell $ pure BasicBlock {block = toList ts, nextBlock = j}

{- |
Read the target variable from the reader component.
-}
getTarget :: TACM ident lident (Var ident)
getTarget = ask

{- |
@toTAC x `into` v@ translates @x@ and stores its result into @v@.
-}
into :: TACM ident lident a -> Var ident -> TACM ident lident a
into action v = local (const v) action
