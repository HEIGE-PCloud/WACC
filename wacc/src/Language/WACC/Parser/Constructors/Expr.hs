{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Deferred and lifted constructors for data types defined in
"Language.WACC.AST.Expr".
-}
module Language.WACC.Parser.Constructors.Expr
  ( -- * 'AST.ArrayIndex'
    mkArrayIndex

    -- * 'AST.WAtom'
  , mkIntLit
  , mkBoolLit
  , mkCharLit
  , mkStringLit
  , mkNull
  , mkIdent
  , mkArrayElem

    -- * 'AST.Expr'
  , mkWAtom
  , mkNot
  , mkNegate
  , mkLen
  , mkOrd
  , mkChr
  , mkMul
  , mkDiv
  , mkMod
  , mkAdd
  , mkSub
  , mkGT
  , mkGTE
  , mkLT
  , mkLTE
  , mkEq
  , mkIneq
  , mkAnd
  , mkOr
  )
where

import Language.WACC.AST.Expr (ArrayIndex, Expr, WAtom)
import qualified Language.WACC.AST.Expr as AST
import Text.Gigaparsec.Patterns
  ( deriveDeferredConstructors
  , deriveLiftedConstructors
  )
import Text.Gigaparsec.Position (Pos)
import Prelude hiding (GT, LT)

pattern ArrayIndex :: ident -> [Expr Pos ident] -> Pos -> ArrayIndex Pos ident
pattern ArrayIndex v xs p = AST.ArrayIndex v xs p

pattern IntLit :: Integer -> Pos -> WAtom Pos ident
pattern IntLit i p = AST.IntLit i p

pattern BoolLit :: Bool -> Pos -> WAtom Pos ident
pattern BoolLit b p = AST.BoolLit b p

pattern CharLit :: Char -> Pos -> WAtom Pos ident
pattern CharLit c p = AST.CharLit c p

pattern StringLit :: String -> Pos -> WAtom Pos ident
pattern StringLit s p = AST.StringLit s p

pattern Null :: Pos -> WAtom Pos ident
pattern Null p = AST.Null p

pattern Ident :: ident -> Pos -> WAtom Pos ident
pattern Ident v p = AST.Ident v p

pattern ArrayElem :: ArrayIndex Pos ident -> Pos -> WAtom Pos ident
pattern ArrayElem ai p = AST.ArrayElem ai p

pattern WAtom :: WAtom Pos ident -> Pos -> Expr Pos ident
pattern WAtom x p = AST.WAtom x p

$( deriveLiftedConstructors
    "mk"
    [ 'IntLit
    , 'BoolLit
    , 'CharLit
    , 'StringLit
    , 'Null
    , 'Ident
    , 'ArrayIndex
    , 'ArrayElem
    , 'WAtom
    ]
 )

pattern Not :: Expr Pos ident -> Pos -> Expr Pos ident
pattern Not x p = AST.Not x p

pattern Negate :: Expr Pos ident -> Pos -> Expr Pos ident
pattern Negate x p = AST.Negate x p

pattern Len :: Expr Pos ident -> Pos -> Expr Pos ident
pattern Len x p = AST.Len x p

pattern Ord :: Expr Pos ident -> Pos -> Expr Pos ident
pattern Ord x p = AST.Ord x p

pattern Chr :: Expr Pos ident -> Pos -> Expr Pos ident
pattern Chr x p = AST.Chr x p

pattern Mul :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Mul x y p = AST.Mul x y p

pattern Div :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Div x y p = AST.Div x y p

pattern Mod :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Mod x y p = AST.Mod x y p

pattern Add :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Add x y p = AST.Add x y p

pattern Sub :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Sub x y p = AST.Sub x y p

pattern GT :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern GT x y p = AST.GT x y p

pattern GTE :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern GTE x y p = AST.GTE x y p

pattern LT :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern LT x y p = AST.LT x y p

pattern LTE :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern LTE x y p = AST.LTE x y p

pattern Eq :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Eq x y p = AST.Eq x y p

pattern Ineq :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Ineq x y p = AST.Ineq x y p

pattern And :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern And x y p = AST.And x y p

pattern Or :: Expr Pos ident -> Expr Pos ident -> Pos -> Expr Pos ident
pattern Or x y p = AST.Or x y p

$( deriveDeferredConstructors
    "mk"
    [ 'Not
    , 'Negate
    , 'Len
    , 'Ord
    , 'Chr
    , 'Mul
    , 'Div
    , 'Mod
    , 'Add
    , 'Sub
    , 'GT
    , 'GTE
    , 'LT
    , 'LTE
    , 'Eq
    , 'Ineq
    , 'And
    , 'Or
    ]
 )
