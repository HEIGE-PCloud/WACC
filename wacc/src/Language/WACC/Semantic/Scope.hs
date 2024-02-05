{-# LANGUAGE NamedFieldPuns #-}
{-|
The strategy for scope analysis is add all function names into a symbol table,
analyse the @main@ statement and on completeion leave the outermost scope in the table
(global variables), then analyse each function
-}
module Language.WACC.Semantic.Scope () where

import Data.Map as Map
import Data.List.NonEmpty
import Prelude hiding (reverse)
import Language.WACC.AST (Stmt (..), Stmts)
import Language.WACC.AST.Prog (Prog)
import Text.Gigaparsec.Position (Pos)

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

--scopeCheck :: Prog String String -> Either (Prog Fnident Ident) [Error]
--scopeCheck ast =
--  undefined

data Analysis = Analysis
  { -- | Symbol Table with variables of the scopes above the local scope
    superST :: Map String (Int, Pos)
    -- | Symbol Table of the local scope
  , localST :: Map String (Int, Pos)
    -- | Symbol Table for functions
  , funcST :: Map String (Int, Pos)
    -- | ls for lines. suggest better name pls
  , ctr :: Integer
  }

renameIdent :: (Functor f) => Analysis -> f String -> f Ident
renameIdent Analysis {superST, localST} = fmap go
  where
    go :: String -> Ident
    go str = Ident n str where
      n :: Int
      n
        | member str localST = fst $ localST ! str
        | member str superST = fst $ superST ! str
        | otherwise          = 0 -- 0 is index for invalid reference


--rename :: Prog String String -> Program Fnident Ident
--rename (Main fs ls) = Main fs' ls'
--  where

renameStmt :: Analysis -> Stmt String String -> (Analysis, Stmt Fnident Ident)
--renameStmt a@(Analysis {superST, localST, functST, ctr}) Skip
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Decl WType ident (RValue fnident ident) Pos)
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Asgn (LValue ident) (RValue fnident ident))
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Read (LValue ident))
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Free (Expr ident))
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Return (Expr ident))
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Exit (Expr ident))
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (Print (Expr ident))
--renameStmt a@(Analysis {superST, localST, functST, ctr}) (PrintLn (Expr ident))
--
--
--renameStmt a (IfElse e s1 s2) = (a'', IfElse (renameIdent a e) s1'
--    where
--      a' = a {superST = union localST superST, localST = Map.empty}
--      (a'', stmts') = renameStmts a' s
--renameStmt a (While expr) stmts = (a'', While (renameIdent a expr) stmts')
--    where
--      a' = a {superST = union localST superST, localST = Map.empty}
--      (a'', stmts') = renameStmts a' stmts
renameStmt a@(Analysis {superST, localST}) (BeginEnd stmts) = (a {ctr = ctr''} , BeginEnd stmts')
    where
      a' = a {superST = union localST superST, localST = Map.empty}
      (Analysis {ctr=ctr''}, stmts') = renameStmts a' stmts

renameStmts = undefined



---- local scope analysis
--data Analysis = Analysis
--  { -- | Symbol Table with variables of the scopes above the local scope
--    superST :: Map String (Int, Pos)
--    -- | Symbol Table of the local scope
--  , localST :: Map String (Int, Pos)
--    -- | Symbol Table for functions
--  , funcST :: Map String (Int, Pos)
--    -- | ls for lines. suggest better name pls
--  , ls :: Stmts Fnident Ident
--    -- | Error messages to be reported to user
--  , errs :: [Error]
--  , ctr :: Integer
--  }
--
--incr :: State Analysis ()
--incr = modify $ \a@(Analysis {ctr}) -> a {ctr = ctr + 1}
--
--decl :: State Analaysis ()
--decl = modify foo
--  where
--    foo :: Analysis -> Analysis
--    foo a@(Analysis {localST, err})
--      | member ident localST = a {err = ((show ident) ++" alreeady defined in line XX"):err}
--      | otherwise = a
--
----also take Pos as parameter for err message
--encounter :: String -> State Analaysis ()
--encounter ident = modify foo
--  where
--    foo :: Analysis -> Analysis
--    foo a@(Analysis {superST, localST, err})
--      | member ident localST || member ident superST = a
--      | otherwise = a {err = ((show ident) ++" not found in line XX"):err}
--
--encounterFn :: String -> State Analaysis ()
--encounterFn ident = modify foo
--  where
--    foo :: Analysis -> Analysis
--    foo a@(Analysis {funcST, err})
--      | member ident funcST = a
--      | otherwise = a {err = ((show ident) ++" not found in line XX"):err}
--
--exprScopeCheck :: Expr String -> State Analysis ()
--exprScopeCheck (WAtom (Ident ident pos)) = encounter ident
--exprScopeCheck (Not (Expr ident)
--exprScopeCheck (Negate (Expr ident)
--exprScopeCheck (Len (Expr ident)
--exprScopeCheck (Ord (Expr ident)
--exprScopeCheck (Chr (Expr ident)
--exprScopeCheck (Mul (Expr ident) (Expr ident)
--exprScopeCheck (Div (Expr ident) (Expr ident)
--exprScopeCheck (Mod (Expr ident) (Expr ident)
--exprScopeCheck (Add (Expr ident) (Expr ident)
--exprScopeCheck (Sub (Expr ident) (Expr ident)
--exprScopeCheck (GT (Expr ident) (Expr ident)
--exprScopeCheck (GTE (Expr ident) (Expr ident)
--exprScopeCheck (LT (Expr ident) (Expr ident)
--exprScopeCheck (LTE (Expr ident) (Expr ident)
--exprScopeCheck (Eq (Expr ident) (Expr ident)
--exprScopeCheck (Ineq (Expr ident) (Expr ident)
--exprScopeCheck (And (Expr ident) (Expr ident)
--exprScopeCheck (Or (Expr ident) (Expr ident)
--
--stmtScopeCheck
--  :: Stmts String String -> Analysis -> (Stmt Fnident Ident, [Error])
--stmtScopeCheck ls st = undefined -- fold ls
--  where
--    sl = reverse ls
--    consumeStmt :: Stmts String String -> Analysis -> Analysis
--    consumeStmt ((Decl _ ident _ pos):|ls) analysis
--      | member ident (localST analysis) = analysis {errs = "line " ++ show pos ++ ": was already defined":es}
--      | otherwise                       = analysis {localST = insert ident pos loc}
--      where
--        es = errs analysis
--        loc = localST analysis
----    consumeStmt (Skip) = undefined
----    consumeStmt (Asgn) = undefined
----    consumeStmt (Read) = undefined
----    consumeStmt (Free) = undefined
----    consumeStmt (Return) = undefined
----    consumeStmt (Exit) = undefined
----    consumeStmt (Print) = undefined
----    consumeStmt (PrintLn) = undefined
----    consumeStmt (IfElse) = undefined
----    consumeStmt (While) = undefined
----    consumeStmt (BeginEnd) = undefined
----
