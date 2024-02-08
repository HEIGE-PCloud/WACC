{-# LANGUAGE ApplicativeDo #-}

{- |
The strategy for scope analysis is add all function names into a symbol table,
analyse the @Stmt@ of each function and the main @Stmt@
-}
module Language.WACC.Semantic.Scope (scopeAnalysis, Fnident, Vident, VarST) where

import Control.Monad (foldM, liftM, void)
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
import Language.WACC.Error
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

type SuperST = Map String (Integer, Pos)

type LocalST = Map String (Integer, Pos)

type VarST = Map Vident (WType, Pos)

type FuncST = Map String (Integer, Pos)

type Ctr = Integer

type Analysis = RWS (SuperST, FuncST) (DList Error, VarST) (LocalST, Ctr)

renamePairElem :: PairElem String -> Analysis (PairElem Vident)
renamePairElem (FstElem lv p) = (`FstElem` p) <$> renameLValue lv
renamePairElem (SndElem lv p) = (`SndElem` p) <$> renameLValue lv

renameRValue :: RValue String String -> Analysis (RValue Fnident Vident)
renameRValue (RVExpr e p) = (`RVExpr` p) <$> renameExpr e
renameRValue (RVArrayLit es p) = (`RVArrayLit` p) <$> mapM renameExpr es
renameRValue (RVNewPair e1 e2 p) = (\x y -> RVNewPair x y p) <$> renameExpr e1 <*> renameExpr e2
renameRValue (RVPairElem pe p) = (`RVPairElem` p) <$> renamePairElem pe
renameRValue (RVCall str es pos) = do
  es' <- mapM renameExpr es
  renameFuncOrErr str pos (\n -> RVCall (Fnident n str) es' pos)

renameLValue :: LValue String -> Analysis (LValue Vident)
renameLValue (LVIdent str pos) = renameOrErr str pos (\n -> LVIdent (Vident n str) pos)
renameLValue (LVArrayElem arrI p) = (`LVArrayElem` p) <$> renameArrayIndex arrI
renameLValue (LVPairElem pe p) = (`LVPairElem` p) <$> renamePairElem pe

renameFuncOrErr :: String -> Pos -> (Integer -> a) -> Analysis a
renameFuncOrErr str pos constr = do
  funcST <- asks snd
  case funcST !? str of
    Just (n, _) -> return $ constr n
    Nothing -> do
      report (notDecl str) pos (length str)
      return undefined

renameOrErr :: String -> Pos -> (Integer -> a) -> Analysis a
renameOrErr str pos constr = do
  (localST, superST) <- getST
  case getDecl str localST superST of
    Just (n, _) -> return $ constr n
    Nothing -> do
      report (notDecl str) pos (length str)
      return undefined

renameArrayIndex :: ArrayIndex String -> Analysis (ArrayIndex Vident)
renameArrayIndex (ArrayIndex str es pos) = do
  es' <- mapM renameExpr es
  renameOrErr str pos (\n -> ArrayIndex (Vident n str) es' pos)

renameAtom :: WAtom String -> Analysis (WAtom Vident)
renameAtom (Ident str pos) = renameOrErr str pos (\n -> Ident (Vident n str) pos)
renameAtom (ArrayElem arrI p) = (`ArrayElem` p) <$> renameArrayIndex arrI
renameAtom (IntLit x p) = pure (IntLit x p)
renameAtom (BoolLit x p) = pure (BoolLit x p)
renameAtom (CharLit x p) = pure (CharLit x p)
renameAtom (StringLit x p) = pure (StringLit x p)
renameAtom (Null p) = pure (Null p)

-- renameAtom x = pure x -- TODO Doesn't work because x :: WAtom String. Is there some way to change this

renameExpr :: Expr String -> Analysis (Expr Vident)
renameExpr (WAtom atom p) = (`WAtom` p) <$> renameAtom atom
renameExpr (Not e p) = (`Not` p) <$> renameExpr e
renameExpr (Negate e p) = (`Negate` p) <$> renameExpr e
renameExpr (Len e p) = (`Len` p) <$> renameExpr e
renameExpr (Ord e p) = (`Ord` p) <$> renameExpr e
renameExpr (Chr e p) = (`Chr` p) <$> renameExpr e
renameExpr (Mul e1 e2 p) = (\x y -> Mul x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Div e1 e2 p) = (\x y -> Div x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Mod e1 e2 p) = (\x y -> Mod x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Add e1 e2 p) = (\x y -> Add x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Sub e1 e2 p) = (\x y -> Sub x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (GT e1 e2 p) = (\x y -> GT x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (GTE e1 e2 p) = (\x y -> GTE x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (LT e1 e2 p) = (\x y -> LT x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (LTE e1 e2 p) = (\x y -> LTE x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Eq e1 e2 p) = (\x y -> Eq x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Ineq e1 e2 p) = (\x y -> Ineq x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (And e1 e2 p) = (\x y -> And x y p) <$> renameExpr e1 <*> renameExpr e2
renameExpr (Or e1 e2 p) = (\x y -> Or x y p) <$> renameExpr e1 <*> renameExpr e2

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

ctr = snd

freshN :: Analysis Ctr
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

insertDecl :: Pos -> WType -> String -> Analysis Ctr
insertDecl pos t str = do
  n <- freshN
  modify $ mapPair (insert str (n, pos)) id
  tell (mempty, Map.singleton (Vident n str) (t, pos))
  return n

getST :: Analysis (LocalST, SuperST)
getST = (,) <$> gets fst <*> asks fst

alreadyDecl str origPos = str ++ " was already defined in " ++ show origPos ++ "."
notDecl str = str ++ " was not declared."

report :: String -> Pos -> Int -> Analysis ()
report str pos len = tell (Data.DList.singleton (Error str pos (toEnum len)), mempty)

renameStmt :: Stmt String String -> Analysis (Stmt Fnident Vident)
renameStmt (Skip p) = pure (Skip p)
renameStmt (Decl t str rv pos) = do
  rv' <- renameRValue rv
  (localST, superST) <- getST
  case getDecl str localST superST of
    Nothing -> Control.Monad.void (insertDecl pos t str)
    (Just (_, posOrig)) -> report (alreadyDecl str posOrig) pos (length str)
  n <- gets snd
  return (Decl t (Vident n str) rv' pos) -- Should I return a bogus node?
renameStmt (Asgn lv rv p) = do
  lv' <- renameLValue lv
  rv' <- renameRValue rv
  return (Asgn lv' rv' p)
renameStmt (Read lv p) = do
  lv' <- renameLValue lv
  return (Read lv' p)
renameStmt (Free e p) = (`Free` p) <$> renameExpr e
renameStmt (Return e p) = (`Return` p) <$> renameExpr e
renameStmt (Exit e p) = (`Exit` p) <$> renameExpr e
renameStmt (Print e p) = (`Print` p) <$> renameExpr e
renameStmt (PrintLn e p) = (`PrintLn` p) <$> renameExpr e
renameStmt (IfElse e l1 l2 p) = do
  e' <- renameExpr e
  l1' <- renameStmts l1
  l2' <- renameStmts l2
  return (IfElse e' l1' l2' p)
renameStmt (While e ls p) = do
  e' <- renameExpr e
  ls' <- renameStmts ls
  return (While e' ls' p)
renameStmt (BeginEnd ls p) = (`BeginEnd` p) <$> renameStmts ls

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
    params' = (mapPair id) . Vident <$> ns <*> params
  funcST <- asks snd
  let
    (n, _) = funcST ! str
  ls' <- renameStmts ls
  return (Func t (Fnident n str) params' ls' pos)

renameProg :: Prog String String -> Analysis (Prog Fnident Vident)
renameProg (Main fs ls p) = do
  fs' <- mapM renameFunc fs
  ls' <- renameStmts ls
  return (Main fs' ls' p)

scopeAnalysis
  :: Prog String String -> Either [Error] (Prog Fnident Vident, VarST)
scopeAnalysis p@(Main fs ls _) = pass2 maybeFuncST
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
    bar (Just funcST) (Func _ str _ _ pos) = case funcST !? str of
      Just (_, origPos) -> do
        tell . DList.singleton $
          Error (alreadyDecl str origPos) pos (toEnum (length str)) 
        return Nothing
      Nothing -> do
        n <- freshN
        return (Just (insert str (n, pos) funcST))
    bar Nothing _ = return Nothing
    freshN = do
      modify (+ 1)
      get
