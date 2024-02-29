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
  , completeBlock
  , getTarget
  , into
  , tempWith
  , loadConst
  , move
  )
where

import Control.Monad.Trans.RWS (RWS, ask, evalRWS, gets, local, modify, tell)
import Data.DList (DList, toList)
import Language.WACC.TAC.TAC
  ( BasicBlock (..)
  , Jump (..)
  , TAC (LoadCI, Move)
  , Var (Temp)
  )

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
runTACM l action = evalRWS action (Temp 0) (TACMState 1 l mempty)

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
  tell [BasicBlock {block = toList ts, nextBlock = j}]

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

{- |
@tempWith action@ executes @action@ targeting a fresh temporary variable, which
is then returned.
-}
tempWith
  :: (Enum ident) => TACM ident lident () -> TACM ident lident (Var ident)
tempWith action = do
  t <- freshTemp
  action `into` t
  pure t

{- |
@loadConst x@ loads @x@ into a fresh temporary variable, which is then returned.
-}
loadConst :: (Enum ident) => Int -> TACM ident lident (Var ident)
loadConst x = tempWith $ do
  target <- getTarget
  putTACs [LoadCI target x]

{- |
@move dest src@ generates @'Move' dest src@ only if @dest@ and @src@ differ.
-}
move :: (Eq ident) => Var ident -> Var ident -> TACM ident lident ()
move dest src
  | dest == src = pure ()
  | otherwise = putTACs [Move dest src]
