{-# LANGUAGE MonadComprehensions #-}

{- |
Type checking actions for WACC statements.
-}
module Language.WACC.TypeChecking.Stmt (checkStmt, unifyStmts) where

import Control.Monad (foldM)
import Language.WACC.AST.Stmt
import Language.WACC.TypeChecking.BType
import Language.WACC.TypeChecking.Expr
import Language.WACC.TypeChecking.State

checkLValue :: (Ord ident) => LValue ident -> TypingM ident BType
checkLValue = undefined

checkRValue :: (Ord ident) => RValue fnident ident -> TypingM ident BType
checkRValue = undefined

{- |
@unifyStmts t0 (s1 :| [s2, ..., sn])@ attempts to unify @s1@ with @t0@ to obtain
@t1@, which is unified with @s2@ to obtain @t2@, and so on, returning @tn@.

If a unification fails, the traversal is aborted.
-}
unifyStmts :: (Ord ident) => BType -> Stmts fnident ident -> TypingM ident BType
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
checkStmt :: (Ord ident) => Stmt fnident ident -> TypingM ident BType
checkStmt Skip = pure BAny
checkStmt (Decl t v rv) = do
  vt <- typeOf v
  t' <- tryUnify vt $ fix t
  rvt <- checkRValue rv
  tryUnify rvt t'
checkStmt (Asgn lv rv) =
  BAny <$ (tryUnify <$> checkRValue rv <*> checkLValue lv)
checkStmt (Read lv) = BAny <$ checkLValue lv
checkStmt (Free x) = [BAny | t <- checkExpr x, t `elem` heapAllocatedTypes]
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
