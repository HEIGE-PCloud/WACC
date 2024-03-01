{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
The strategy for scope analysis is first add all function names into a symbol table (pass1).
The AST renaming and scope analysis occurs simultaneously in the 2nd pass using a RWS monad, @Analysis@.
-}
module Language.WACC.Semantic.Scope (scopeAnalysis, Fnident (..), Vident (..), VarST) where

import Control.Monad.RWS
  ( RWS
  , asks
  , evalRWS
  , gets
  , local
  , modify
  , tell
  )
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Function (on)
import Data.List (deleteFirstsBy, nub, nubBy)
import Data.Map (Map, empty, insert, union, (!), (!?))
import qualified Data.Map as Map
import Language.WACC.AST.Expr
  ( ArrayIndex (..)
  , Expr (..)
  , WAtom (..)
  )
import Language.WACC.AST.Prog (Func (..), Prog (..))
import Language.WACC.AST.Stmt
  ( LValue (..)
  , PairElem (..)
  , RValue (..)
  , Stmt (..)
  , Stmts (..)
  )
import Language.WACC.AST.WType (WType)
import Language.WACC.Error (Error (Error), quote)
import Text.Gigaparsec (Result (..))
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT, reverse)

{- |
The @Integer@ acts as a UID (which will not clash with variables @Vident@s either).
-}
type Fnident = Integer

{- |
The @Integer@ acts as a UID (which will not clash with @Fnident@s either)
-}
type Vident = Integer

-- | Symbol Table of variables for all scopes above the current one
type SuperST = Map String (Integer, Pos)

-- | Symbol Table of variables in current scope
type LocalST = Map String (Integer, Pos)

-- |  The output symbol table containing every variable in program, indexed by its unique name.
type VarST = Map Vident (WType, Pos)

-- | Symbol Table for functions
type FuncST = Map String (Integer, Pos)

-- | Incremented and used as a unique id across all scopes
type Ctr = Integer

{- |
Used in the Main (2nd pass).
The Write state and Output state stores elements of the result of scope analysis.
-}
type Analysis = RWS (SuperST, FuncST) (DList Error, VarST) (LocalST, Ctr)

superST :: (SuperST, FuncST) -> SuperST
superST = fst

funcST :: (SuperST, FuncST) -> FuncST
funcST = snd

localST :: (LocalST, Ctr) -> LocalST
localST = fst

ctr :: (LocalST, Ctr) -> Ctr
ctr = snd

{- |
  id 0 is reserved for invalid nodes.
-}
errorId :: Integer
errorId = 0

defaultCaretLen :: Int
defaultCaretLen = 1

---------------------
-- Utility Functions--
---------------------

{- |
Ensure that an encountered variable identifier is present in @localST@ or @globalST@
If not, write an error and annotate the AST node with @errorId@
-}
renameFuncOrErr :: String -> Pos -> (Integer -> a) -> Analysis a
renameFuncOrErr str pos constr = do
  funcST <- asks funcST
  case funcST !? str of
    Just (n, _) -> return $ constr n
    Nothing -> do
      report ("function " ++ notDecl str) pos defaultCaretLen
      return $ constr errorId

{- |
Ensure that an encountered variable identifier is present in @localST@ or @globalST@
If not, write an error and annotate the AST node with @errorId@
-}
renameOrErr :: String -> Pos -> (Integer -> a) -> Analysis a
renameOrErr str pos constr = do
  (localST, superST) <- getST
  case getDecl str localST (Just superST) of
    Just (n, _) -> return $ constr n
    Nothing -> do
      report ("variable " ++ notDecl str) pos (length str)
      return $ constr errorId

alreadyDecl :: [Char] -> Pos -> [Char]
alreadyDecl str origPos = quote str ++ " was already defined in line " ++ show (fst origPos)

notDecl :: [Char] -> [Char]
notDecl str = quote str ++ " was not declared"

-- | write an error in a uniform format
report :: String -> Pos -> Int -> Analysis ()
report str pos len =
  tell (DList.singleton (Error ("scope error: " ++ str) pos (toEnum len)), mempty)

-- | Apply functions to corresponding pair elems
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

-- | Gnenerate a new UID.
freshN :: Analysis Ctr
freshN = do
  modify $ mapPair id (+ 1)
  gets ctr

-- | Get info on variable declarations
getDecl :: String -> LocalST -> Maybe SuperST -> Maybe (Integer, Pos)
getDecl str localST maybeSuperST = case localVal of
  Nothing -> superVal
  x -> x
  where
    localVal = localST !? str
    superVal = case maybeSuperST of
      Nothing -> Nothing
      Just superST -> superST !? str

-- | Check if variable already exists and if not include it in @localST@
insertDecl :: Pos -> WType -> String -> Analysis Ctr
insertDecl pos t str = do
  (localST, _) <- getST
  case getDecl str localST Nothing of
    Nothing -> do
      n <- freshN
      modify $ mapPair (insert str (n, pos)) id
      tell (mempty, Map.singleton (n) (t, pos))
      return n
    (Just (_, posOrig)) -> do
      report (alreadyDecl str posOrig) pos defaultCaretLen
      return errorId

-- | Helper for getting full variable table
getST :: Analysis (LocalST, SuperST)
getST = (,) <$> gets localST <*> asks superST

---------------------
-- Renamer Functions--
---------------------

type family R a where
  R (a String b c) = a Fnident Vident c
  R (a b c) = a Vident c

class Rename t where
  rename :: t String Pos -> Analysis (R (t String Pos))

instance Rename PairElem where
  rename :: PairElem String Pos -> Analysis (PairElem Vident Pos)
  rename (FstElem lv p) = (`FstElem` p) <$> rename lv
  rename (SndElem lv p) = (`SndElem` p) <$> rename lv

instance Rename (RValue String) where
  rename :: RValue String String Pos -> Analysis (RValue Fnident Vident Pos)
  rename (RVExpr e p) = (`RVExpr` p) <$> rename e
  rename (RVArrayLit es p) = (`RVArrayLit` p) <$> mapM rename es
  rename (RVNewPair e1 e2 p) = (\x y -> RVNewPair x y p) <$> rename e1 <*> rename e2
  rename (RVPairElem pe p) = (`RVPairElem` p) <$> rename pe
  rename (RVCall str es pos) = do
    es' <- mapM rename es
    renameFuncOrErr str pos (\n -> RVCall (n) es' pos)

instance Rename LValue where
  rename :: LValue String Pos -> Analysis (LValue Vident Pos)
  rename (LVIdent str pos) = renameOrErr str pos (\n -> LVIdent (n) pos)
  rename (LVArrayElem arrI p) = (`LVArrayElem` p) <$> rename arrI
  rename (LVPairElem pe p) = (`LVPairElem` p) <$> rename pe

instance Rename ArrayIndex where
  rename :: ArrayIndex String Pos -> Analysis (ArrayIndex Vident Pos)
  rename (ArrayIndex str es pos) = do
    es' <- mapM rename es
    renameOrErr str pos (\n -> ArrayIndex (n) es' pos)

{- |
Use @renameOrErr@ to inspect every occurence of an ident and rename it
-}
instance Rename WAtom where
  rename :: WAtom String Pos -> Analysis (WAtom Vident Pos)
  rename (Ident str pos) = renameOrErr str pos (\n -> Ident (n) pos)
  rename (ArrayElem arrI p) = (`ArrayElem` p) <$> rename arrI
  rename (IntLit x p) = pure (IntLit x p)
  rename (BoolLit x p) = pure (BoolLit x p)
  rename (CharLit x p) = pure (CharLit x p)
  rename (StringLit x p) = pure (StringLit x p)
  rename (Null p) = pure (Null p)

instance Rename Expr where
  rename :: Expr String Pos -> Analysis (Expr Vident Pos)
  rename (WAtom atom p) = (`WAtom` p) <$> rename atom
  rename (Not e p) = (`Not` p) <$> rename e
  rename (Negate e p) = (`Negate` p) <$> rename e
  rename (Len e p) = (`Len` p) <$> rename e
  rename (Ord e p) = (`Ord` p) <$> rename e
  rename (Chr e p) = (`Chr` p) <$> rename e
  rename (Mul e1 e2 p) = (\x y -> Mul x y p) <$> rename e1 <*> rename e2
  rename (Div e1 e2 p) = (\x y -> Div x y p) <$> rename e1 <*> rename e2
  rename (Mod e1 e2 p) = (\x y -> Mod x y p) <$> rename e1 <*> rename e2
  rename (Add e1 e2 p) = (\x y -> Add x y p) <$> rename e1 <*> rename e2
  rename (Sub e1 e2 p) = (\x y -> Sub x y p) <$> rename e1 <*> rename e2
  rename (GT e1 e2 p) = (\x y -> GT x y p) <$> rename e1 <*> rename e2
  rename (GTE e1 e2 p) = (\x y -> GTE x y p) <$> rename e1 <*> rename e2
  rename (LT e1 e2 p) = (\x y -> LT x y p) <$> rename e1 <*> rename e2
  rename (LTE e1 e2 p) = (\x y -> LTE x y p) <$> rename e1 <*> rename e2
  rename (Eq e1 e2 p) = (\x y -> Eq x y p) <$> rename e1 <*> rename e2
  rename (Ineq e1 e2 p) = (\x y -> Ineq x y p) <$> rename e1 <*> rename e2
  rename (And e1 e2 p) = (\x y -> And x y p) <$> rename e1 <*> rename e2
  rename (Or e1 e2 p) = (\x y -> Or x y p) <$> rename e1 <*> rename e2

{- |
Recursively delegate renaming based on the structure of the statment
-}
instance Rename (Stmt String) where
  rename :: Stmt String String Pos -> Analysis (Stmt Fnident Vident Pos)
  rename (Skip p) = pure (Skip p)
  rename (Decl t str rv pos) = do
    rv' <- rename rv
    n <- insertDecl pos t str
    return (Decl t (n) rv' pos)
  rename (Asgn lv rv p) = do
    lv' <- rename lv
    rv' <- rename rv
    return (Asgn lv' rv' p)
  rename (Read lv p) = do
    lv' <- rename lv
    return (Read lv' p)
  rename (Free e p) = (`Free` p) <$> rename e
  rename (Return e p) = (`Return` p) <$> rename e
  rename (Exit e p) = (`Exit` p) <$> rename e
  rename (Print e p) = (`Print` p) <$> rename e
  rename (PrintLn e p) = (`PrintLn` p) <$> rename e
  rename (IfElse e l1 l2 p) = do
    e' <- rename e
    l1' <- rename l1
    l2' <- rename l2
    return (IfElse e' l1' l2' p)
  rename (While e ls p) = do
    e' <- rename e
    ls' <- rename ls
    return (While e' ls' p)
  rename (BeginEnd ls p) = (`BeginEnd` p) <$> rename ls

{- |
Rename all statements inside a new scope. Then restore the old scope.
-}
instance Rename (Stmts String) where
  rename :: Stmts String String Pos -> Analysis (Stmts Fnident Vident Pos)
  rename ls = do
    localST <- gets localST
    modify $ mapPair (const Data.Map.empty) id
    ls' <-
      local
        (mapPair (\globalST -> localST `union` globalST) id)
        (mapM rename (unwrap ls))
    modify $ mapPair (const localST) id
    return (Stmts ls')

{- |
Wipe the localST before looking at the param list because previous function
params interfere otherwise. Delegate stmts to @rename@
-}
instance Rename (Func WType String) where
  rename
    :: Func WType String String Pos -> Analysis (Func WType Fnident Vident Pos)
  rename (Func t str params ls pos) = do
    modify $ mapPair (const Data.Map.empty) id
    params' <- mapM renameParam params
    funcST <- asks funcST
    let
      (n, _) = funcST ! str
    ls' <- rename ls
    return (Func t (n) params' ls' pos)
    where
      renameParam :: (WType, String) -> Analysis (WType, Vident)
      renameParam (t, str) = do
        n <- insertDecl pos t str
        return (t, n)

instance Rename (Prog WType String) where
  rename
    :: Prog WType String String Pos -> Analysis (Prog WType Fnident Vident Pos)
  rename (Main fs ls p) = do
    fs' <- mapM rename fs
    ls' <- rename ls
    return (Main fs' ls' p)

{- |
Run both passes failing if errors are encountered at each stage.
-}
scopeAnalysis
  :: Prog WType String String Pos
  -> Result [Error] (Prog WType Fnident Vident Pos, VarST)
scopeAnalysis p@(Main fs _ _)
  | not (null errs1) = Failure errs1
  | otherwise = processPass2 (DList.toList errs2)
  where
    processPass2
      :: [Error] -> Result [Error] (Prog WType Fnident Vident Pos, VarST)
    processPass2 errs2
      | not (null errs2) = Failure errs2
      | otherwise = Success (p', varST)
    errs1 = pass1 (zip names poss)
    names = map (\(Func _ str _ _ _) -> str) fs
    poss = map (\(Func _ _ _ _ pos) -> pos) fs
    n = toInteger $ length fs
    funcST = Map.fromList $ zip names (zip [1 ..] poss)
    (p', (errs2, varST)) = evalRWS (rename p) (Map.empty, funcST) (Map.empty, n + 1)

{- |
Runs the first pass non-monadically checking if there are duplicate function names
and reporting error messages
-}
pass1 :: [(String, Pos)] -> [Error]
pass1 fns = map mkErr dups
  where
    eqFsts = (==) `on` fst
    dups = nub $ deleteFirstsBy eqFsts fns (nubBy eqFsts fns)
    mkErr (str, pos) =
      Error
        ("function " ++ quote str ++ " is already defined")
        pos
        (toEnum (length str))
