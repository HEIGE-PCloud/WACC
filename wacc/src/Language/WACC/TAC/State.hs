{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

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
  , appendBlock
  , completeBlock
  , getTarget
  , into
  , tempWith
  , loadConst
  )
where

import Control.Monad.Trans.RWS (RWS, ask, evalRWS, gets, local, modify, tell)
import Data.DList (DList, toList)
import Data.Map (Map, empty, insert)
import Language.WACC.TAC.TAC
  ( BasicBlock (..)
  , Jump (..)
  , Label
  , TAC (LoadCI)
  , TACProgram
  , Var (Temp)
  )

data TACMState ident lident = TACMState
  { nextTemp :: ident
  , nextLabel :: lident
  , tacs :: DList (TAC ident lident)
  , funcBlocks :: Map lident (BasicBlock ident lident)
  }

type Blocks ident lident = DList (BasicBlock ident lident)

{- |
TAC translation monad.
-}
type TACM ident lident =
  RWS (Var ident) (TACProgram ident lident) (TACMState ident lident)

{- |
Run a TAC translation action.

Basic block labels are allocated starting from the given @lident@.
-}
runTACM
  :: (Num ident) => lident -> TACM ident lident a -> (a, TACProgram ident lident)
runTACM l action = evalRWS action (Temp 0) (TACMState 1 l mempty empty)

{- |
Run a TAC translation action, returning only the generated basic blocks.
-}
evalTACM
  :: (Num ident) => lident -> TACM ident lident a -> TACProgram ident lident
evalTACM l = snd . runTACM l

{- |
Get a fresh temporary variable.
-}
freshTemp :: (Enum ident, Ord lident) => TACM ident lident (Var ident)
freshTemp = Temp <$> gets nextTemp <* modify incrTemp
  where
    incrTemp st@TACMState {nextTemp} = st {nextTemp = succ nextTemp}

{- |
Get a fresh basic block label.
-}
freshLabel :: (Enum lident, Ord lident) => TACM ident lident lident
freshLabel = gets nextLabel <* modify incrLabel
  where
    incrLabel st@TACMState {nextLabel} = st {nextLabel = succ nextLabel}

{- |
Append some TAC instructions to the state.
-}
putTACs :: (Ord lident) => DList (TAC ident lident) -> TACM ident lident ()
putTACs ts = modify appendTACs
  where
    appendTACs st@TACMState {tacs} = st {tacs = tacs <> ts}

{- |
Collect TAC instructions from the state.
-}
collectTACs :: (Ord lident) => TACM ident lident (DList (TAC ident lident))
collectTACs = gets tacs <* modify dropTACs
  where
    dropTACs st = st {tacs = mempty}

{- |
Inserts a basic block into the current function map.
-}
appendBlock
  :: (Ord lident) => BasicBlock ident lident -> lident -> TACM ident lident ()
appendBlock bb l = modify appendBlock'
  where
    appendBlock' st@TACMState {funcBlocks} = st {funcBlocks = insert l bb funcBlocks}

{- |
Collect TAC instructions from the state into a basic block.
-}
completeBlock
  :: (Ord lident) => Jump ident lident -> lident -> TACM ident lident ()
completeBlock j l = do
  ts <- collectTACs
  appendBlock (BasicBlock (toList ts) j) l

{- |
Read the target variable from the reader component.
-}
getTarget :: (Ord lident) => TACM ident lident (Var ident)
getTarget = ask

{- |
@toTAC x `into` v@ translates @x@ and stores its result into @v@.
-}
into :: TACM ident lident a -> Var ident -> TACM ident lident a
into action v = local (const v) action

{- |
@tempWith action@ executes @action@ targeting a fresh temporary variable, which
is then returned.
-}
tempWith
  :: (Enum ident, Ord lident)
  => TACM ident lident ()
  -> TACM ident lident (Var ident)
tempWith action = do
  t <- freshTemp
  action `into` t
  pure t

{- |
@loadConst x@ loads @x@ into a fresh temporary variable, which is then returned.
-}
loadConst :: (Enum ident, Ord lident) => Int -> TACM ident lident (Var ident)
loadConst x = tempWith $ do
  target <- getTarget
  putTACs [LoadCI target x]
