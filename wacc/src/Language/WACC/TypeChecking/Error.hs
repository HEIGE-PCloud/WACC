{- |
Type checking errors.
-}
module Language.WACC.TypeChecking.Error
  ( TypeError (..)
  , convertTypeError
  , prettyType
  , showType
  )
where

import Data.List (isSuffixOf)
import Language.WACC.Error (Error (Error))
import Language.WACC.TypeChecking.BType (BType (..))
import Text.Gigaparsec.Position (Pos)

{- |
A type error found during type checking.
-}
data TypeError
  = -- | Incompatible types.
    IncompatibleTypesError
      BType
      -- ^ The type of the provided value.
      BType
      -- ^ The expected type.
      Pos
      -- ^ The position of the ill-typed expression or statement.
  | -- | Invalid number of parameters in function call.
    FunctionCallArityError
      Int
      -- ^ The arity of the ill-typed function call.
      Int
      -- ^ The declared arity of the function.
      Pos
      -- ^ The position of the ill-typed function call.

{- |
Pretty-print a 'BType'.
-}
prettyType :: BType -> String
prettyType BAny = "any value"
prettyType BInt = "an int"
prettyType BBool = "a bool"
prettyType BChar = "a char"
prettyType BString = "a string"
prettyType BErasedPair = "a pair"
prettyType (BKnownPair BAny BAny) = "a pair"
prettyType (BKnownPair t BAny) =
  "a pair with left element of type " ++ prettyType t
prettyType (BKnownPair BAny t) =
  "a pair with right element of type " ++ prettyType t
prettyType (BKnownPair t1 t2) =
  concat ["a pair of ", prettyType t1, " and ", prettyType t2]
prettyType (BArray BAny) = "an array"
prettyType (BArray t) = prettyType t ++ "[]"

{- |
Provide a syntactic representation of a 'BType'.
-}
showType :: BType -> String
showType BAny = ""
showType BInt = "int"
showType BBool = "bool"
showType BChar = "char"
showType BString = "string"
showType BErasedPair = "pair"
showType (BKnownPair t1 t2) =
  concat ["pair(", showType t1, ", ", showType t2, ")"]
showType (BArray BAny) = "T[] for some type T"
showType (BArray t) = showType t ++ "[]"

formatType :: BType -> String
formatType t
  | null st || st `isSuffixOf` pt = pt
  | otherwise = concat [pt, " (", st, ")"]
  where
    pt = prettyType t
    st = showType t

mkTypeErrorMessage :: [String] -> String
mkTypeErrorMessage = concat . ("type error: " :)

{- |
Convert a 'TypeError' into an 'Error' for printing.
-}
convertTypeError :: TypeError -> Error
convertTypeError (IncompatibleTypesError actT expT p) =
  Error
    ( mkTypeErrorMessage
        [ "expected "
        , formatType expT
        , " but found "
        , formatType actT
        ]
    )
    p
    1
convertTypeError (FunctionCallArityError actN expN p) =
  Error
    ( mkTypeErrorMessage
        [ "expected "
        , show expN
        , " arguments but found "
        , show actN
        , " arguments"
        ]
    )
    p
    1
