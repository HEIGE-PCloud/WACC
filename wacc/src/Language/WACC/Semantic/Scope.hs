{-# LANGUAGE ApplicativeDo #-}

{- |
The strategy for scope analysis is add all function names into a symbol table,
analyse the @Stmt@ of each function and the main @Stmt@
-}
module Language.WACC.Semantic.Scope () where

import Control.Monad (foldM, liftM)
import Control.Monad.RWS
  ( RWS
  , RWST
  , ask
  , asks
  , evalRWS
  , get
  , gets
  , local
  , modify
  , put
  , runRWS
  , tell
  )
import Data.DList (DList (..), singleton)
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..), singleton, unzip, (<|))
import Data.Map hiding (null, singleton, toList)
import qualified Data.Map as Map
import Language.WACC.AST
  ( ArrayIndex (..)
  , Expr (..)
  , Func (..)
  , LValue (..)
  , PairElem (..)
  , Prog (..)
  , RValue (..)
  , Stmt (..)
  , Stmts
  , WAtom (..)
  )
import Language.WACC.AST.WType (WType)
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT, reverse, unzip)

{- |
The @Integer  acts as a UID (which will not clash with variable @Vident@s either)
The @String@ is the original name required error reporting, helpful for scope analysis
-}
data Fnident = Fnident Integer String
  deriving (Show)

instance Eq Fnident where
  (Fnident n1 _) == (Fnident n2 _) = n1 == n1

instance Ord Fnident where
  (Fnident n1 _) <= (Fnident n2 _) = n1 <= n1

{- |
The @Integer  acts as a UID (which will not clash with @Fnident@s either)
The @String@ is the original name required error reporting, helpful for scope analysis
-}
data Vident = Vident Integer String
  deriving (Show)

instance Eq Vident where
  (Vident n1 _) == (Vident n2 _) = n1 == n1

instance Ord Vident where
  (Vident n1 _) <= (Vident n2 _) = n1 <= n1

type Error = String

type SuperST = Map String (Integer, Pos)

type LocalST = Map String (Integer, Pos)

type VarST = Map Vident (WType, Pos)

type FuncST = Map String (Integer, Pos)

type Ctr = Integer

type Analysis = RWS (SuperST, FuncST) (DList Error, VarST) (LocalST, Ctr)

renamePairElem :: PairElem String -> Analysis (PairElem Vident)
renamePairElem (FstElem lv) = FstElem <$> renameLValue lv
renamePairElem (SndElem lv) = SndElem <$> renameLValue lv

renameRValue :: RValue String String -> Analysis (RValue Fnident Vident)
renameRValue (RVExpr e) = RVExpr <$> renameExpr e
renameRValue (RVArrayLit es) = RVArrayLit <$> (mapM renameExpr es)
renameRValue (RVNewPair e1 e2) = RVNewPair <$> renameExpr e1 <*> renameExpr e2
renameRValue (RVPairElem pe) = RVPairElem <$> renamePairElem pe
renameRValue (RVCall str es pos) = do
  es' <- mapM renameExpr es
  renameFuncOrErr str pos (\n -> RVCall (Fnident n str) es' pos)

renameLValue :: LValue String -> Analysis (LValue Vident)
renameLValue (LVIdent str pos) = renameOrErr str pos (\n -> (LVIdent (Vident n str) pos))
renameLValue (LVArrayElem arrI) = LVArrayElem <$> renameArrayIndex arrI
renameLValue (LVPairElem pe) = LVPairElem <$> renamePairElem pe

renameFuncOrErr :: String -> Pos -> (Integer -> a) -> Analysis a
renameFuncOrErr str pos constr = do
  funcST <- asks snd
  case (funcST !? str) of
    Just (n, _) -> return $ constr n
    Nothing -> do
      report pos (str ++ "was not previously declared")
      return undefined

renameOrErr :: String -> Pos -> (Integer -> a) -> Analysis a
renameOrErr str pos constr = do
  (localST, superST) <- getST
  case (getDecl str localST superST) of
    Just (n, _) -> return $ constr n
    Nothing -> do
      report pos (str ++ " was not previously declared")
      return undefined

renameArrayIndex :: ArrayIndex String -> Analysis (ArrayIndex Vident)
renameArrayIndex (ArrayIndex str es pos) = do
  es' <- mapM renameExpr es
  renameOrErr str pos (\n -> ArrayIndex (Vident n str) es' pos)

renameAtom :: WAtom String -> Analysis (WAtom Vident)
renameAtom (Ident str pos) = renameOrErr str pos (\n -> Ident (Vident n str) pos)
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

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

ctr = snd

freshN :: Analysis (Ctr)
freshN = do
  modify $ mapPair id (+ 1)
  gets ctr

getDecl :: String -> LocalST -> SuperST -> Maybe (Integer, Pos)
getDecl str localST superST = case localVal of
  Nothing -> superVal
  x -> x
  where
    localVal = localST !? str
    superVal = superST !? str

insertDecl :: Pos -> WType -> String -> Analysis (Ctr)
insertDecl pos t str = do
  n <- freshN
  modify $ mapPair (\localST -> insert str (n, pos) localST) id
  tell (mempty, Map.singleton (Vident n str) (t, pos))
  return n

getST :: Analysis (LocalST, SuperST)
getST = (,) <$> gets fst <*> asks fst

report :: Pos -> String -> Analysis ()
report pos str = tell $ (Data.DList.singleton ("Line " ++ show pos ++ ": " ++ str), mempty)

renameStmt :: Stmt String String -> Analysis (Stmt Fnident Vident)
renameStmt Skip = pure Skip
renameStmt (Decl t str rv pos) = do
  rv' <- renameRValue rv
  (localST, superST) <- getST
  case (getDecl str localST superST) of
    Nothing -> insertDecl pos t str >> return ()
    (Just (_, posOrig)) -> report pos (str ++ "already defined in line " ++ show posOrig)
  n <- gets snd
  return (Decl t (Vident n str) rv' pos) -- Should I return a bogus node?
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
  modify $ mapPair (const Data.Map.empty) id
  -- TODO does the ctr of the outer scope get modified by local?
  ls' <-
    local (mapPair (\globalST -> localST `union` globalST) id) (mapM renameStmt ls)
  modify $ mapPair (const localST) id
  return ls'

renameFunc :: Func String String -> Analysis (Func Fnident Vident)
renameFunc (Func t str params ls pos) = do
  ns <- mapM (uncurry $ insertDecl pos) params
  let
    params' = (((mapPair id) . Vident) <$> ns) <*> params
  funcST <- asks snd
  let
    (n, _) = funcST ! str
  --  Just (n, pos) -> report pos (str ++ " already declared in line " ++ show pos)
  ls' <- renameStmts ls
  return (Func t (Fnident n str) params' ls' pos)

renameProg :: Prog String String -> Analysis (Prog Fnident Vident)
renameProg (Main fs ls) = do
  fs' <- mapM renameFunc fs
  ls' <- renameStmts ls
  return (Main fs' ls')

scopeAnalysis
  :: Prog String String -> Either [Error] (Prog Fnident Vident, VarST)
scopeAnalysis p@(Main fs ls) = pass2 maybeFuncST
  where
    (maybeFuncST, n, errs1) = runRWS (foo fs) () 0
    pass2 :: Maybe FuncST -> Either [Error] (Prog Fnident Vident, VarST)
    pass2 Nothing = Left (DList.toList errs1)
    pass2 (Just funcST)
      | null errs2 = Right (p', varST)
      | otherwise = Left (DList.toList errs2)
      where
        errs2 :: DList Error
        (p', (errs2, varST)) = evalRWS (renameProg p) (Map.empty, funcST) (Map.empty, n)

type Pass1 = RWS () (DList Error) Ctr (Maybe FuncST)

foo :: [Func String String] -> Pass1
foo = foldM bar (Just Map.empty)
  where
    bar :: Maybe FuncST -> Func String String -> Pass1
    bar (Just funcST) (Func _ str _ _ pos) = case (funcST !? str) of
      Just (_, origPos) -> do
        tell . DList.singleton $
          ((show pos) ++ ": " ++ str ++ " already defined in " ++ (show origPos))
        return Nothing
      Nothing -> do
        n <- freshN
        return (Just (insert str (n, pos) funcST))
    bar Nothing _ = return Nothing
    freshN = do
      modify $ (+ 1)
      get
