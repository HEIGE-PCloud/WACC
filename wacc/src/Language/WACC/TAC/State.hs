{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

{- |
TAC translation monad and actions.
-}
module Language.WACC.TAC.State
  ( TACM
  , defaultTarget
  , runTACM
  , evalTACM
  , freshTemp
  , freshLabel
  , putTACs
  , collectTACs
  , appendBlock
  , completeBlock
  , collectBlocks
  , putFunc
  , getTarget
  , into
  , tempWith
  , loadConst
  , move
  )
where

import Control.Monad.Trans.RWS (RWS, ask, evalRWS, gets, local, modify, tell)
import Data.DList (DList, toList)
import Data.Map (Map, empty, insert, singleton)
import Language.WACC.TAC.TAC

data TACMState ident lident = TACMState
  { nextTemp :: ident
  , nextLabel :: lident
  , tacs :: DList (TAC ident lident)
  , funcBlocks :: Map lident (BasicBlock ident lident)
  }

{- |
TAC translation monad.
-}
type TACM ident lident =
  RWS (Var ident) (TACProgram ident lident) (TACMState ident lident)

{- |
Initial target variable for executing TACM actions.
-}
defaultTarget :: (Num a) => Var a
defaultTarget = Temp 0

{- |
Run a TAC translation action.

Basic block labels are allocated starting from the given @lident@.
-}
runTACM
  :: (Num ident)
  => lident
  -> TACM ident lident a
  -> (a, TACProgram ident lident)
runTACM l action =
  evalRWS action defaultTarget (TACMState firstTempID l mempty empty)
  where
    firstTempID = 1

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
Collects the map of basic blocks from the state.
-}
collectBlocks
  :: (Ord lident) => TACM ident lident (Map lident (BasicBlock ident lident))
collectBlocks = gets funcBlocks <* modify clearBlocks
  where
    clearBlocks st = st {funcBlocks = empty}

{- |
Inserts a function into the TAC program.
-}
putFunc :: lident -> TACFunc ident lident -> TACM ident lident ()
putFunc name func = tell $ singleton name func

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

{- |
@move dest src@ generates @'Move' dest src@ only if @dest@ and @src@ differ.
-}
move :: (Eq ident, Ord lident) => Var ident -> Var ident -> TACM ident lident ()
move dest src
  | dest == src = pure ()
  | otherwise = putTACs [Move dest src]
