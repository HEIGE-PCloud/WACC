{-# LANGUAGE ApplicativeDo #-}

{- |
The strategy for scope analysis is first add all function names into a symbol table (pass1).
The AST renaming and scope analysis occurs simultaneously in the 2nd pass using a RWS monad, @Analysis@.
-}
module Language.WACC.Semantic.Scope (scopeAnalysis, Fnident, Vident, VarST) where

import Control.Monad (foldM, void)
import Control.Monad.RWS
  ( RWS
  , asks
  , evalRWS
  , get
  , gets
  , local
  , modify
  , runRWS
  , tell
  )
import Data.DList (DList (..), singleton)
import qualified Data.DList as DList
import Data.Function (on)
import Data.List (deleteFirstsBy, nub, nubBy)
import Data.List.NonEmpty (NonEmpty (..), singleton, unzip, (<|))
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
  , Stmts
  )
import Language.WACC.AST.WType (WType)
import Language.WACC.Error (Error (Error), quote)
import Text.Gigaparsec (Result (..))
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT, reverse, unzip)

{- |
The @Integer@ acts as a UID (which will not clash with variables @Vident@s either).
The @String@ is the original name required error reporting.
-}
data Fnident = Fnident Integer String
  deriving (Show)

instance Eq Fnident where
  (Fnident n1 _) == (Fnident n2 _) = n1 == n2

instance Ord Fnident where
  (Fnident n1 _) <= (Fnident n2 _) = n1 <= n2

{- |
The @Integer@ acts as a UID (which will not clash with @Fnident@s either)
The @String@ is the original name required error reporting.
-}
data Vident = Vident Integer String
  deriving (Show)

instance Eq Vident where
  (Vident n1 _) == (Vident n2 _) = n1 == n2

instance Ord Vident where
  (Vident n1 _) <= (Vident n2 _) = n1 <= n2

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

superST = fst

funcST = snd

localST = fst

ctr = snd

{- |
  id 0 is reserved for invalid nodes.
-}
errorId :: Integer
errorId = 0

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
      report ("Function " ++ notDecl str) pos (length str)
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
      report ("Variable " ++ notDecl str) pos (length str)
      return $ constr errorId

alreadyDecl :: (Show a) => [Char] -> a -> [Char]
alreadyDecl str origPos = quote str ++ " was already defined in " ++ show origPos ++ "."

notDecl :: [Char] -> [Char]
notDecl str = quote str ++ " was not declared."

-- | write an error in a uniform format
report :: String -> Pos -> Int -> Analysis ()
report str pos len = tell (Data.DList.singleton (Error str pos (toEnum len)), mempty)

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
      tell (mempty, Map.singleton (Vident n str) (t, pos))
      return n
    (Just (_, posOrig)) -> do
      report (alreadyDecl str posOrig) pos 1
      return errorId

-- | Helper for getting full variable table
getST :: Analysis (LocalST, SuperST)
getST = (,) <$> gets localST <*> asks superST

---------------------
-- Renamer Functions--
---------------------

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

renameArrayIndex :: ArrayIndex String -> Analysis (ArrayIndex Vident)
renameArrayIndex (ArrayIndex str es pos) = do
  es' <- mapM renameExpr es
  renameOrErr str pos (\n -> ArrayIndex (Vident n str) es' pos)

{- |
Use @renameOrErr@ to inspect every occurence of an ident and rename it
-}
renameAtom :: WAtom String -> Analysis (WAtom Vident)
renameAtom (Ident str pos) = renameOrErr str pos (\n -> Ident (Vident n str) pos)
renameAtom (ArrayElem arrI p) = (`ArrayElem` p) <$> renameArrayIndex arrI
renameAtom (IntLit x p) = pure (IntLit x p)
renameAtom (BoolLit x p) = pure (BoolLit x p)
renameAtom (CharLit x p) = pure (CharLit x p)
renameAtom (StringLit x p) = pure (StringLit x p)
renameAtom (Null p) = pure (Null p)

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

{- |
Recursively delegate renaming based on the structure of the statment
-}
renameStmt :: Stmt String String -> Analysis (Stmt Fnident Vident)
renameStmt (Skip p) = pure (Skip p)
renameStmt (Decl t str rv pos) = do
  rv' <- renameRValue rv
  n <- insertDecl pos t str
  return (Decl t (Vident n str) rv' pos)
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
Rename all statements inside a new scope. Then restore the old scope.
-}
renameStmts :: Stmts String String -> Analysis (Stmts Fnident Vident)
renameStmts ls = do
  localST <- gets localST
  modify $ mapPair (const Data.Map.empty) id
  ls' <-
    local (mapPair (\globalST -> localST `union` globalST) id) (mapM renameStmt ls)
  modify $ mapPair (const localST) id
  return ls'

{- |
Wipe the localST before looking at the param list because previous function
params interfere otherwise. Delegate stmts to @renameStmts@
-}
renameFunc :: Func String String -> Analysis (Func Fnident Vident)
renameFunc (Func t str params ls pos) = do
  modify $ mapPair (const Data.Map.empty) id
  params' <- mapM renameParam params
  funcST <- asks funcST
  let
    (n, _) = funcST ! str
  ls' <- renameStmts ls
  return (Func t (Fnident n str) params' ls' pos)
  where
    renameParam :: (WType, String) -> Analysis (WType, Vident)
    renameParam (t, str) = do
      n <- insertDecl pos t str
      return (t, Vident n str)

renameProg :: Prog String String -> Analysis (Prog Fnident Vident)
renameProg (Main fs ls p) = do
  fs' <- mapM renameFunc fs
  ls' <- renameStmts ls
  return (Main fs' ls' p)

{- |
Run both passes failing if errors are encountered at each stage.
-}
scopeAnalysis
  :: Prog String String -> Result [Error] (Prog Fnident Vident, VarST)
scopeAnalysis p@(Main fs ls pos)
  | not (null errs1) = Failure errs1
  | otherwise = processPass2 (DList.toList errs2)
  where
    processPass2 errs2
      | not (null errs2) = Failure errs2
      | otherwise = Success (p', varST)
    errs1 = pass1 (zip names poss)
    names = map (\(Func _ str _ _ _) -> str) fs
    poss = map (\(Func _ _ _ _ pos) -> pos) fs
    n = toInteger $ length fs
    funcST = Map.fromList $ zip names (zip [1 ..] poss)
    (p', (errs2, varST)) = evalRWS (renameProg p) (Map.empty, funcST) (Map.empty, n + 1)

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
