{-# LANGUAGE ApplicativeDo #-}

{- |
The strategy for scope analysis is add all function names into a symbol table,
analyse the @main@ statement and on completeion leave the outermost scope in the table
(global variables), then analyse each function
-}
module Language.WACC.Semantic.Scope () where

import Control.Monad (foldM, liftM)
import Control.Monad.RWS (RWS, ask, asks, get, gets, local, modify, put, tell)
import Data.DList (DList (..), singleton)
import Data.List.NonEmpty (NonEmpty (..), singleton, unzip, (<|))
import Data.Map hiding (singleton)
import qualified Data.Map as Map
import Language.WACC.AST
  ( ArrayIndex (..)
  , Expr (..)
  , LValue (..)
  , RValue (..)
  , Stmt (..)
  , Stmts
  , WAtom (..)
  )
import Language.WACC.AST.Prog (Prog)
import Language.WACC.AST.WType (WType)
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (reverse, unzip)

{- |
The @Integer  acts as a UID (which will not clash with variable @Vident@s either)
The @String@ is the original name required error reporting, helpful for scope analysis
-}
data Fnident = Fnident Integer String WType
  deriving (Show)

instance Eq Fnident where
  (Fnident n1 _ _) == (Fnident n2 _ _) = n1 == n1

instance Ord Fnident where
  (Fnident n1 _ _) <= (Fnident n2 _ _) = n1 <= n1

{- |
The @Integer  acts as a UID (which will not clash with @Fnident@s either)
The @String@ is the original name required error reporting, helpful for scope analysis
-}
data Vident = Vident Integer String WType
  deriving (Show)

instance Eq Vident where
  (Vident n1 _ _) == (Vident n2 _ _) = n1 == n1

instance Ord Vident where
  (Vident n1 _ _) <= (Vident n2 _ _) = n1 <= n1

type Error = String

type SuperST = Map String (Integer, WType, Pos)

type LocalST = Map String (Integer, WType, Pos)

type FuncST = Map String (Integer, WType, Pos)

type Ctr = Integer

type Analysis = RWS SuperST (DList Error) (LocalST, Ctr)

renameRValue :: RValue String String -> Analysis (RValue Fnident Vident)
renameRValue = undefined

renameLValue :: LValue String -> Analysis (LValue Vident)
renameLValue = undefined

renameExpr :: Expr String -> Analysis (Expr Vident)
renameExpr = undefined

mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (f, g) (x, y) = (f x, g y)

fresh :: Analysis ()
fresh = modify $ mapPair (id, (+ 1))

getDecl :: String -> LocalST -> SuperST -> Maybe (Integer, WType, Pos)
getDecl str localST superST = case localVal of
  Nothing -> superVal
  x -> x
  where
    localVal = localST !? str
    superVal = superST !? str

getST :: Analysis (LocalST, SuperST)
getST = (,) <$> gets fst <*> ask

report :: Pos -> String -> Analysis ()
report pos str = tell $ Data.DList.singleton ("Line " ++ show pos ++ ": " ++ str)

renameStmt :: Stmt String String -> Analysis (Stmt Fnident Vident)
renameStmt Skip = pure Skip
renameStmt (Decl t ident rv pos) = do
  rv' <- renameRValue rv
  (localST, superST) <- getST
  fresh
  n <- gets snd
  case (getDecl ident localST superST) of
    Nothing -> modify insertDecl
    (Just (_, _, posOrig)) -> report pos (ident ++ "already defined in line " ++ show posOrig)
  return (Decl t (Vident n ident t) rv' pos)
  where
    insertDecl :: (LocalST, Ctr) -> (LocalST, Ctr)
    insertDecl (localST, n) = (insert ident (n, t, pos) localST, n)
renameStmt (Asgn lv rv) = do
  lv' <- renameLValue lv
  rv' <- renameRValue rv
  return (Asgn lv' rv')
renameStmt (Read lv) = do
  lv' <- renameLValue lv
  return (Read lv')
renameStmt (Free e) = Free <$> renameExpr e
renameStmt (Return e) = Return <$> renameExpr e
renameStmt (Exit e) = Exit <$> renameExpr e
renameStmt (Print e) = Print <$> renameExpr e
renameStmt (PrintLn e) = PrintLn <$> renameExpr e
renameStmt (IfElse e l1 l2) = do
  e' <- renameExpr e
  l1' <- renameStmts l1
  l2' <- renameStmts l2
  return (IfElse e' l1' l2')
renameStmt (While e ls) = do
  e' <- renameExpr e
  ls' <- renameStmts ls
  return (While e' ls')
renameStmt (BeginEnd ls) = BeginEnd <$> renameStmts ls

{- |
make a new scope too
-}
renameStmts :: Stmts String String -> Analysis (Stmts Fnident Vident)
renameStmts ls = do
  localST <- gets fst
  modify $ mapPair (const Data.Map.empty, id)
  -- TODO does the ctr of the outer scope get modified by local?
  ls' <- local (\globalST -> localST `union` globalST) (mapM renameStmt ls)
  modify $ mapPair (const localST, id)
  return ls'
