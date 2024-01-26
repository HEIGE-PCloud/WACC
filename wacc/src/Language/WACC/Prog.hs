{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
WACC programs.
-}
module Language.WACC.Prog (Prog (..), FnArgs) where

import Data.HList (HList (..))
import Data.Kind (Type)
import Language.WACC.Stmt (RetType (..))
import Language.WACC.WType (WType (..))

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
  where
  -- | > <type> <ident>(<type> <ident>, ...) is <stmt> end
  Func
    :: fnident args ret
    -> HList (FnArgs ident args)
    -> stmt (Ret ret)
    -> Prog ident fnident stmt
    -> Prog ident fnident stmt
  -- | Main program.
  MainStmt :: stmt Main -> Prog ident fnident stmt
