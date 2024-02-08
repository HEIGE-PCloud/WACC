{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Type checking actions for WACC statements.
-}
module Language.WACC.TypeChecking.Stmt
  ( checkStmt
  , unifyStmts
  , checkLValue
  , checkRValue
  , checkPairElem
  )
where

import Control.Monad (foldM, unless, zipWithM_)
import Language.WACC.AST.Stmt
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Expr
import Language.WACC.TypeChecking.State

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
checkPairElem (FstElem lv) = fst <$> unifyPair lv
checkPairElem (SndElem lv) = snd <$> unifyPair lv

{- |
Type check a WACC @lvalue@.
-}
checkLValue :: LValue ident -> TypingM fnident ident BType
checkLValue (LVIdent v) = typeOf v
checkLValue (LVArrayElem ai) = checkArrayIndex ai
checkLValue (LVPairElem pe) = checkPairElem pe

{- |
Type check a WACC @rvalue@.
-}
checkRValue
  :: (Ord fnident) => RValue fnident ident -> TypingM fnident ident BType
checkRValue (RVExpr x) = checkExpr x
checkRValue (RVArrayLit xs) = BArray <$> unifyExprs BAny xs
checkRValue (RVNewPair x1 x2) = BKnownPair <$> checkExpr x1 <*> checkExpr x2
checkRValue (RVPairElem pe) = checkPairElem pe
checkRValue (RVCall f xs) = do
  FnType {..} <- typeOfFn f
  let
    actN = length paramTypes
    expN = length xs
  unless (actN == expN) $ abortWithArityError actN expN undefined
  ts <- mapM checkExpr xs
  zipWithM_ tryUnify ts paramTypes
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
unifyStmts t ss = traverse checkStmt ss >>= foldM (flip tryUnify) t

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
checkStmt Skip = pure BAny
checkStmt (Decl t v rv) =
  [ BAny
  | vt <- typeOf v
  , t' <- tryUnify vt $ fix t
  , rvt <- checkRValue rv
  , _ <- tryUnify rvt t'
  ]
checkStmt (Asgn lv rv) =
  [ BAny
  | rvt <- checkRValue rv
  , lvt <- checkLValue lv
  , _ <- tryUnify rvt lvt
  ]
checkStmt (Read lv) = BAny <$ checkLValue lv
checkStmt (Free x) =
  [ BAny
  | t <- checkExpr x
  , let
      t' = case t of
        BFixed ft -> BFixed $ BAny <$ ft
        _ -> t
  , t' `elem` heapAllocatedTypes
  ]
checkStmt (Return x) = checkExpr x
checkStmt (Exit x) = BAny <$ unifyExprs BInt [x]
checkStmt (Print x) = BAny <$ checkExpr x
checkStmt (PrintLn x) = BAny <$ checkExpr x
checkStmt (IfElse x ifBody elseBody) = do
  _ <- unifyExprs BBool [x]
  t <- unifyStmts BAny ifBody
  unifyStmts t elseBody
checkStmt (While x body) = unifyExprs BBool [x] *> unifyStmts BAny body
checkStmt (BeginEnd body) = unifyStmts BAny body
