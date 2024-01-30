{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
WACC programs.
-}
module Language.WACC.AST.Prog (Prog (..), FnArgs) where

import Data.HList (HList (..))
import Data.Kind (Type)
import Language.WACC.AST.Stmt (RetType (..))
import Language.WACC.AST.WType (WType (..))

{- |
Identifiers for WACC function arguments.
-}
type family
  FnArgs
    (ident :: WType erasure -> Type)
    (args :: [WType erasure])
    :: [Type]
  where
  FnArgs ident '[] = '[]
  FnArgs ident (t : ts) = ident t : FnArgs ident ts

{- |
WACC programs.
-}
data
  Prog
    (ident :: WType erasure -> Type)
    (fnident :: [WType erasure] -> WType erasure -> Type)
    (stmt :: RetType erasure -> Type)
  = -- | > <type> <ident>(<type> <ident>, ...) is <stmt> end
    forall args ret.
    Func
      (fnident args ret)
      (HList (FnArgs ident args))
      (stmt (Ret ret))
      (Prog ident fnident stmt)
  | -- | Main program.
    MainStmt (stmt Main)

{- |
This instance can only test equality on the number of functions as the 'Func'
constructor uses existentially quantified type variables.
-}
instance (Eq (stmt Main)) => Eq (Prog ident fnident stmt) where
  Func _ _ _ p1 == Func _ _ _ p2 = p1 == p2
  MainStmt s1 == MainStmt s2 = s1 == s2
  _ == _ = False

instance
  (forall args ret. Show (fnident args ret), forall ret. Show (stmt ret))
  => Show (Prog ident fnident stmt)
  where
  show (Func fId _ body prog) =
    "Func " ++ show fId ++ " [...] " ++ show body ++ " (" ++ show prog ++ ")"
  show (MainStmt stmt) = "MainStmt (" ++ show stmt ++ ")"
