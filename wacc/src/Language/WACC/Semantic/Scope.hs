{-# LANGUAGE ApplicativeDo #-}

{- |
The strategy for scope analysis is add all function names into a symbol table,
analyse the @Stmt@ of each function and the main @Stmt@
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
  , PairElem (..)
  , RValue (..)
  , Stmt (..)
  , Stmts
  , WAtom (..)
  )
import Language.WACC.AST.Prog (Prog)
import Language.WACC.AST.WType (WType)
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT, reverse, unzip)

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

renamePairElem :: PairElem String -> Analysis (PairElem Vident)
renamePairElem (FstElem lv) = FstElem <$> renameLValue lv
renamePairElem (SndElem lv) = SndElem <$> renameLValue lv

renameRValue :: RValue String String -> Analysis (RValue Fnident Vident)
renameRValue (RVExpr e) = RVExpr <$> renameExpr e
renameRValue (RVArrayLit es) = RVArrayLit <$> (mapM renameExpr es)
renameRValue (RVNewPair e1 e2) = RVNewPair <$> renameExpr e1 <*> renameExpr e2
renameRValue (RVPairElem pe) = RVPairElem <$> renamePairElem pe
renameRValue (RVCall fnident es) = undefined

renameLValue :: LValue String -> Analysis (LValue Vident)
renameLValue (LVIdent ident pos) = renameOrErr ident pos (\n t -> (LVIdent (Vident n ident t) pos))
renameLValue (LVArrayElem arrI) = LVArrayElem <$> renameArrayIndex arrI
renameLValue (LVPairElem pe) = LVPairElem <$> renamePairElem pe

renameOrErr :: String -> Pos -> (Integer -> WType -> a) -> Analysis a
renameOrErr ident pos constr = do
  (localST, superST) <- getST
  case (getDecl ident localST superST) of
    Just (n, t, _) -> return $ constr n t
    Nothing -> do
      report pos "ident was not previously declared"
      return undefined

renameArrayIndex :: ArrayIndex String -> Analysis (ArrayIndex Vident)
renameArrayIndex (ArrayIndex ident es pos) = do
  es' <- mapM renameExpr es
  renameOrErr ident pos (\n t -> ArrayIndex (Vident n ident t) es' pos)

renameAtom :: WAtom String -> Analysis (WAtom Vident)
renameAtom (Ident ident pos) = renameOrErr ident pos (\n t -> Ident (Vident n ident t) pos)
renameAtom (ArrayElem arrI) = ArrayElem <$> (renameArrayIndex arrI)
renameAtom (IntLit x) = pure (IntLit x)
renameAtom (BoolLit x) = pure (BoolLit x)
renameAtom (CharLit x) = pure (CharLit x)
renameAtom (StringLit x) = pure (StringLit x)
renameAtom Null = pure Null

-- renameAtom x = pure x -- TODO Doesn't work because x :: WAtom String. Is there some way to change this

renameExpr :: Expr String -> Analysis (Expr Vident)
renameExpr (WAtom atom) = WAtom <$> renameAtom atom
renameExpr (Not e) = Not <$> renameExpr e
renameExpr (Negate e) = Negate <$> renameExpr e
renameExpr (Len e) = Len <$> renameExpr e
renameExpr (Ord e) = Ord <$> renameExpr e
renameExpr (Chr e) = Chr <$> renameExpr e
renameExpr (Mul e1 e2) = Mul <$> renameExpr e1 <*> renameExpr e2
renameExpr (Div e1 e2) = Div <$> renameExpr e1 <*> renameExpr e2
renameExpr (Mod e1 e2) = Mod <$> renameExpr e1 <*> renameExpr e2
renameExpr (Add e1 e2) = Add <$> renameExpr e1 <*> renameExpr e2
renameExpr (Sub e1 e2) = Sub <$> renameExpr e1 <*> renameExpr e2
renameExpr (GT e1 e2) = GT <$> renameExpr e1 <*> renameExpr e2
renameExpr (GTE e1 e2) = GTE <$> renameExpr e1 <*> renameExpr e2
renameExpr (LT e1 e2) = LT <$> renameExpr e1 <*> renameExpr e2
renameExpr (LTE e1 e2) = LTE <$> renameExpr e1 <*> renameExpr e2
renameExpr (Eq e1 e2) = Eq <$> renameExpr e1 <*> renameExpr e2
renameExpr (Ineq e1 e2) = Ineq <$> renameExpr e1 <*> renameExpr e2
renameExpr (And e1 e2) = And <$> renameExpr e1 <*> renameExpr e2
renameExpr (Or e1 e2) = Or <$> renameExpr e1 <*> renameExpr e2

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
