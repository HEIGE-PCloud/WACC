{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Lifted constructors for data types defined in "Language.WACC.AST.Prog".
-}
module Language.WACC.Parser.Constructors.Prog
  ( -- * 'AST.Prog'
    mkMain

    -- * 'AST.Func'
  , mkFunc
  )
where

import Language.WACC.AST (Func, Prog, Stmts)
import qualified Language.WACC.AST as AST
import Text.Gigaparsec.Patterns (deriveLiftedConstructors)
import Text.Gigaparsec.Position (Pos)

pattern Main
  :: [Func Pos typ fnident ident]
  -> Stmts Pos fnident ident
  -> Pos
  -> Prog Pos typ fnident ident
pattern Main fs b p = AST.Main fs b p

pattern Func
  :: typ
  -> fnident
  -> [(typ, ident)]
  -> Stmts Pos fnident ident
  -> Pos
  -> Func Pos typ fnident ident
pattern Func rt f ps b p = AST.Func rt f ps b p

$( deriveLiftedConstructors
    "mk"
    [ 'Main
    , 'Func
    ]
 )
