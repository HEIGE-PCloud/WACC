{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.WACC.X86.Memory where

import Language.WACC.X86.Label (Label)
import Language.WACC.X86.Register (RegisterQ)

data Memory where
  -- | (53) :- immediate memory access
  MI :: Integer -> Memory
  -- | (%rax) :- single register memory access
  MReg :: RegisterQ -> Memory
  -- | (%rsp, %rax) = M[R[rsp] + R[rax]] :- register sum memory access
  MTwoReg :: RegisterQ -> RegisterQ -> Memory
  -- | (%rsp, %rax, 4) = M[R[rsp] + R[rax]*4] :- register scaled (by 1,2,4 or 8) memory access
  MScale :: RegisterQ -> RegisterQ -> Integer -> Memory
  -- | 7(%rax) = M[7 + R[rax]] :- offset single register memory access
  MRegI :: Integer -> RegisterQ -> Memory
  -- | 7(%rsp, %rax) = M[7 + R[rsp] + R[rax]] :- offset register sum memory access
  MTwoRegI :: Integer -> RegisterQ -> RegisterQ -> Memory
  -- | 7(%rsp, %rax, 4) = M[7 + R[rsp] + R[rax]*4] :- offset register scaled (by 1,2,4 or 8) memory access
  MScaleI :: Integer -> RegisterQ -> RegisterQ -> Integer -> Memory
  -- | f4(%rax) :- offset to label, single register memory access
  MRegL :: Label -> RegisterQ -> Memory

deriving instance Eq Memory

deriving instance Ord Memory

deriving instance Show Memory
