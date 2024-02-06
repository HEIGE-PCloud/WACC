{-# LANGUAGE NamedFieldPuns #-}

{- |
The strategy for scope analysis is add all function names into a symbol table,
analyse the @main@ statement and on completeion leave the outermost scope in the table
(global variables), then analyse each function
-}
module Language.WACC.Semantic.Scope () where

import Control.Monad (foldM, liftM)
import Control.Monad.RWS (RWS)
import Control.Monad.State (MonadState (state), State)
import Data.List.NonEmpty (NonEmpty (..), singleton, unzip, (<|))
import Data.Map hiding (singleton)
import qualified Data.Map as Map
import Language.WACC.AST (Expr, Stmt (..), Stmts)
import Language.WACC.AST.Prog (Prog)
import Language.WACC.AST.WType (WType)
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (reverse, unzip)

{- |
The @Int@ acts as a UID (which will not clash with variable @Ident@s either)
The @String@ is the original name required error reporting, helpful for scope analysis
-}
data Fnident = Fnident Int String
  deriving (Ord, Eq, Show)

{- |
The @Int@ acts as a UID (which will not clash with @Fnident@s either)
The @String@ is the original name required error reporting, helpful for scope analysis
-}
data Ident = Ident Int String
  deriving (Ord, Eq, Show)

type Error = String

-- scopeCheck :: Prog String String -> Either (Prog Fnident Ident) [Error]
-- scopeCheck ast =
--  undefined

data Analysis = Analysis
  { superST :: Map String (Int, Pos)
  -- ^ Symbol Table with variables of the scopes above the local scope
  , localST :: Map String (Int, Pos)
  -- ^ Symbol Table of the local scope
  , funcST :: Map String (Int, Pos)
  -- ^ Symbol Table for functions
  , ctr :: Integer
  -- ^ ls for lines. suggest better name pls
  }

renameIdent :: (Functor f) => Analysis -> f String -> f Ident
renameIdent Analysis {superST, localST} = fmap go
  where
    go :: String -> Ident
    go str = Ident n str
      where
        n :: Int
        n
          | member str localST = fst $ localST ! str
          | member str superST = fst $ superST ! str
          | otherwise = 0 -- 0 is index for invalid reference

-- rename :: Prog String String -> Program Fnident Ident
-- rename (Main fs ls) = Main fs' ls'
--  where

type Result = ([Error], Stmt Fnident Ident)

type Results = ([Error], Stmts Fnident Ident)

renameExpr :: Expr String -> State Analysis ([Error], Expr Ident)
renameExpr = undefined

renameStmt :: Stmt String String -> State Analysis Result
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) Skip
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Decl WType ident (RValue fnident ident) Pos)
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Asgn (LValue ident) (RValue fnident ident))
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Read (LValue ident))
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Free (Expr ident))
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Return (Expr ident))
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Exit (Expr ident))
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (Print (Expr ident))
-- renameStmt a@(Analysis {superST, localST, functST, ctr}) (PrintLn (Expr ident))
-- renameStmt a (IfElse e s1 s2) = (a'', IfElse (renameIdent a e) s1'
--    where
--      a' = a {superST = union localST superST, localST = Map.empty}
--      (a'', stmts') = renameStmts a' s
renameStmt (While e ls) = do
  (err1, e') <- renameExpr e
  (err2, rls) <- foldM foo ([], singleton Skip) ls
  return (err1 ++ err2, While e' rls)
  where
    foo :: Results -> Stmt String String -> State Analysis Results
    foo (errs1, rls) l = do
      -- l for line (stmt), rl for renamed line
      (errs2, rl) <- renameStmt l
      return (errs1 ++ errs2, rl <| rls)

-- renameStmt (BeginEnd stmts) a@(Analysis {superST, localST}) = (a {ctr = ctr''} , BeginEnd stmts')
--    where
--      a' = a {superST = localST `union` superST, localST = Map.empty}
--      (Analysis {ctr=ctr''}, stmts') = renameStmts a' stmts
