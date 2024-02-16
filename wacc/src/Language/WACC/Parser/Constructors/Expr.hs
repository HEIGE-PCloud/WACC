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

pattern ArrayIndex :: ident -> [Expr ident Pos] -> Pos -> ArrayIndex ident Pos
pattern ArrayIndex v xs p = AST.ArrayIndex v xs p

pattern IntLit :: Integer -> Pos -> WAtom ident Pos
pattern IntLit i p = AST.IntLit i p

pattern BoolLit :: Bool -> Pos -> WAtom ident Pos
pattern BoolLit b p = AST.BoolLit b p

pattern CharLit :: Char -> Pos -> WAtom ident Pos
pattern CharLit c p = AST.CharLit c p

pattern StringLit :: String -> Pos -> WAtom ident Pos
pattern StringLit s p = AST.StringLit s p

pattern Null :: Pos -> WAtom ident Pos
pattern Null p = AST.Null p

pattern Ident :: ident -> Pos -> WAtom ident Pos
pattern Ident v p = AST.Ident v p

pattern ArrayElem :: ArrayIndex ident Pos -> Pos -> WAtom ident Pos
pattern ArrayElem ai p = AST.ArrayElem ai p

pattern WAtom :: WAtom ident Pos -> Pos -> Expr ident Pos
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

pattern Not :: Expr ident Pos -> Pos -> Expr ident Pos
pattern Not x p = AST.Not x p

pattern Negate :: Expr ident Pos -> Pos -> Expr ident Pos
pattern Negate x p = AST.Negate x p

pattern Len :: Expr ident Pos -> Pos -> Expr ident Pos
pattern Len x p = AST.Len x p

pattern Ord :: Expr ident Pos -> Pos -> Expr ident Pos
pattern Ord x p = AST.Ord x p

pattern Chr :: Expr ident Pos -> Pos -> Expr ident Pos
pattern Chr x p = AST.Chr x p

pattern Mul :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Mul x y p = AST.Mul x y p

pattern Div :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Div x y p = AST.Div x y p

pattern Mod :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Mod x y p = AST.Mod x y p

pattern Add :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Add x y p = AST.Add x y p

pattern Sub :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Sub x y p = AST.Sub x y p

pattern GT :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern GT x y p = AST.GT x y p

pattern GTE :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern GTE x y p = AST.GTE x y p

pattern LT :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern LT x y p = AST.LT x y p

pattern LTE :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern LTE x y p = AST.LTE x y p

pattern Eq :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Eq x y p = AST.Eq x y p

pattern Ineq :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern Ineq x y p = AST.Ineq x y p

pattern And :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
pattern And x y p = AST.And x y p

pattern Or :: Expr ident Pos -> Expr ident Pos -> Pos -> Expr ident Pos
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
