{- |
TAC translation monad and actions.
-}
module Language.WACC.TAC.State (TACM, runTACM, freshTemp, freshBlock) where

import Control.Monad.Trans.State (State, evalState, gets, modify)
import Data.Bifunctor (first, second)
import Language.WACC.TAC.TAC (Var (Temp))

{- |
TAC translation monad.
-}
type TACM ident lident = State (ident, lident)

{- |
Run a TAC translation action.

Basic block labels are allocated starting from the given @lident@.
-}
runTACM :: (Num ident, Num lident) => lident -> TACM ident lident a -> a
runTACM l = flip evalState (0, l)

{- |
Get a fresh temporary variable.
-}
freshTemp :: (Enum ident) => TACM ident lident (Var ident)
freshTemp = Temp <$> gets fst <* modify (first succ)

{- |
Get a fresh basic block label.
-}
freshBlock :: (Enum lident) => TACM ident lident lident
freshBlock = gets snd <* modify (second succ)
