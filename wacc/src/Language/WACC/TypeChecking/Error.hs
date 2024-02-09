{- |
Type checking errors.
-}
module Language.WACC.TypeChecking.Error (TypeError (..)) where

import Language.WACC.TypeChecking.BType (BType)
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
