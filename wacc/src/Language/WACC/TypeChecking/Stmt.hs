{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Type checking actions for WACC statements.
-}
module Language.WACC.TypeChecking.Stmt
  ( checkStmt
  , unifyStmts
  , unifyStmtsAt
  , checkLValue
  , checkRValue
  , checkPairElem
  )
where

import Control.Monad (foldM, unless, zipWithM_)
import Data.List.NonEmpty (sort)
import Language.WACC.AST.Stmt
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Expr
import Language.WACC.TypeChecking.State
import Text.Gigaparsec.Position (Pos)

unifyPair :: LValue ident -> TypingM fnident ident (BType, BType)
unifyPair lv = do
  lvt <- checkLValue lv
  pt <- tryUnify (BKnownPair BAny BAny) lvt
  case pt of
    BKnownPair t1 t2 -> pure (t1, t2)
    _ -> abort

{- |
Type check an element of a WACC @pair@.
-}
checkPairElem :: PairElem ident -> TypingM fnident ident BType
checkPairElem (FstElem lv _) = fst <$> unifyPair lv
checkPairElem (SndElem lv _) = snd <$> unifyPair lv

{- |
Type check a WACC @lvalue@.
-}
checkLValue :: LValue ident -> TypingM fnident ident BType
checkLValue (LVIdent v _) = typeOf v
checkLValue (LVArrayElem ai _) = checkArrayIndex ai
checkLValue (LVPairElem pe _) = checkPairElem pe

{- |
Type check a WACC @rvalue@.
-}
checkRValue
  :: (Ord fnident) => RValue fnident ident -> TypingM fnident ident BType
checkRValue (RVExpr x _) = checkExpr x
checkRValue (RVArrayLit xs p) = BArray <$> unifyExprsAt p BAny xs
checkRValue (RVNewPair x1 x2 _) = BKnownPair <$> checkExpr x1 <*> checkExpr x2
checkRValue (RVPairElem pe _) = checkPairElem pe
checkRValue (RVCall f xs p) = do
  FnType {..} <- typeOfFn f
  let
    actN = length xs
    expN = length paramTypes
  unless (actN == expN) $ abortWithArityError actN expN p
  ts <- mapM checkExpr xs
  zipWithM_ (\xt pt -> reportAt p pt $ tryUnify xt pt) ts paramTypes
  pure retType

{- |
@unifyStmts t0 (s1 :| [s2, ..., sn])@ attempts to unify @s1@ with @t0@ to obtain
@t1@, which is unified with @s2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyStmts
  :: (Ord fnident)
  => BType
  -> Stmts fnident ident
  -> TypingM fnident ident BType
unifyStmts t ss = traverse checkStmt ss >>= foldM (flip tryUnify) t . sort

{- |
Associate a 'Pos' with a @unifyStmts@ action.
-}
unifyStmtsAt
  :: (Ord fnident)
  => Pos
  -> BType
  -> Stmts fnident ident
  -> TypingM fnident ident BType
unifyStmtsAt p t ss = reportAt p t $ unifyStmts t ss

{- |
Type check a WACC statement.

@checkStmt (Return x)@ returns the type of @x@.

@checkStmt (IfElse x ifBody elseBody)@ unifies the types returned by
@unifyStmts ifBody@ and @unifyStmts elseBody@.

@checkStmt (While x body)@ and @checkStmt (BeginEnd body)@ return the result of
@unifyStmts body@.

Otherwise, 'BAny' is returned.
-}
checkStmt :: (Ord fnident) => Stmt fnident ident -> TypingM fnident ident BType
checkStmt (Skip _) = pure BAny
checkStmt (Decl wt v rv p) =
  [ BAny
  | vt <- typeOf v
  , t' <- reportAt p t $ tryUnify vt t
  , rvt <- checkRValue rv
  , _ <- reportAt p t $ tryUnify rvt t'
  ]
  where
    t = fix wt
checkStmt (Asgn lv rv p) =
  [ BAny
  | rvt <- checkRValue rv
  , lvt <- checkLValue lv
  , _ <- reportAt p lvt $ tryUnify rvt lvt
  ]
checkStmt (Read lv _) = BAny <$ checkLValue lv
checkStmt (Free x _) =
  [ BAny
  | t <- checkExpr x
  , let
      t' = case t of
        BFixed ft -> BFixed $ BAny <$ ft
        _ -> t
  , t' `elem` heapAllocatedTypes
  ]
checkStmt (Return x _) = checkExpr x
checkStmt (Exit x p) = BAny <$ unifyExprsAt p BInt [x]
checkStmt (Print x _) = BAny <$ checkExpr x
checkStmt (PrintLn x _) = BAny <$ checkExpr x
checkStmt (IfElse x ifBody elseBody p) = do
  _ <- unifyExprsAt p BBool [x]
  t <- unifyStmtsAt p BAny ifBody
  unifyStmtsAt p t elseBody
checkStmt (While x body p) =
  unifyExprsAt p BBool [x] *> unifyStmtsAt p BAny body
checkStmt (BeginEnd body p) = unifyStmtsAt p BAny body
