{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.WACC.X86.Register where

import Language.WACC.X86.Size (Size (..))

type RegisterQ = Register Q

type RegisterD = Register D

type RegisterW = Register W

type RegisterB = Register B

data Register (size :: Size) where
  Rax :: RegisterQ
  Eax :: RegisterD
  Ax :: RegisterW
  Al :: RegisterB
  Ah :: RegisterB
  Rbx :: RegisterQ
  Ebx :: RegisterD
  Bx :: RegisterW
  Bl :: RegisterB
  Bh :: RegisterB
  Rcx :: RegisterQ
  Ecx :: RegisterD
  Cx :: RegisterW
  Cl :: RegisterB
  Ch :: RegisterB
  Rdx :: RegisterQ
  Edx :: RegisterD
  Dx :: RegisterW
  Dl :: RegisterB
  Dh :: RegisterB
  Rsi :: RegisterQ
  Esi :: RegisterD
  Si :: RegisterW
  Sil :: RegisterB
  Rdi :: RegisterQ
  Edi :: RegisterD
  Di :: RegisterW
  Dil :: RegisterB
  Rbp :: RegisterQ
  Ebp :: RegisterD
  Bp :: RegisterW
  Bpl :: RegisterB
  Rsp :: RegisterQ
  Esp :: RegisterD
  Sp :: RegisterW
  Spl :: RegisterB
  R8 :: RegisterQ
  R8d :: RegisterD
  R8w :: RegisterW
  R8b :: RegisterB
  R9 :: RegisterQ
  R9d :: RegisterD
  R9w :: RegisterW
  R9b :: RegisterB
  R10 :: RegisterQ
  R10d :: RegisterD
  R10w :: RegisterW
  R10b :: RegisterB
  R11 :: RegisterQ
  R11d :: RegisterD
  R11w :: RegisterW
  R11b :: RegisterB
  R12 :: RegisterQ
  R12d :: RegisterD
  R12w :: RegisterW
  R12b :: RegisterB
  R13 :: RegisterQ
  R13d :: RegisterD
  R13w :: RegisterW
  R13b :: RegisterB
  R14 :: RegisterQ
  R14d :: RegisterD
  R14w :: RegisterW
  R14b :: RegisterB
  R15 :: RegisterQ
  R15d :: RegisterD
  R15w :: RegisterW
  R15b :: RegisterB
  Rip :: RegisterQ

deriving instance Show (Register size)

deriving instance Eq (Register size)

deriving instance Ord (Register size)

{- | RegisterF represents the physical registers of the x86-64 architecture.
| This is used for register allocation.
-}
data RegisterF where
  RaxF :: RegisterF
  RbxF :: RegisterF
  RcxF :: RegisterF
  RdxF :: RegisterF
  RsiF :: RegisterF
  RdiF :: RegisterF
  RbpF :: RegisterF
  RspF :: RegisterF
  R8F :: RegisterF
  R9F :: RegisterF
  R10F :: RegisterF
  R11F :: RegisterF
  R12F :: RegisterF
  R13F :: RegisterF
  R14F :: RegisterF
  R15F :: RegisterF
  RipF :: RegisterF

deriving instance Show RegisterF

deriving instance Eq RegisterF

deriving instance Ord RegisterF

regF :: Register size -> RegisterF
regF Rax = RaxF
regF Eax = RaxF
regF Ax = RaxF
regF Al = RaxF
regF Ah = RaxF
regF Rbx = RbxF
regF Ebx = RbxF
regF Bx = RbxF
regF Bl = RbxF
regF Bh = RbxF
regF Rcx = RcxF
regF Ecx = RcxF
regF Cx = RcxF
regF Cl = RcxF
regF Ch = RcxF
regF Rdx = RdxF
regF Edx = RdxF
regF Dx = RdxF
regF Dl = RdxF
regF Dh = RdxF
regF Rsi = RsiF
regF Esi = RsiF
regF Si = RsiF
regF Sil = RsiF
regF Rdi = RdiF
regF Edi = RdiF
regF Di = RdiF
regF Dil = RdiF
regF Rbp = RbpF
regF Ebp = RbpF
regF Bp = RbpF
regF Bpl = RbpF
regF Rsp = RspF
regF Esp = RspF
regF Sp = RspF
regF Spl = RspF
regF R8 = R8F
regF R8d = R8F
regF R8w = R8F
regF R8b = R8F
regF R9 = R9F
regF R9d = R9F
regF R9w = R9F
regF R9b = R9F
regF R10 = R10F
regF R10d = R10F
regF R10w = R10F
regF R10b = R10F
regF R11 = R11F
regF R11d = R11F
regF R11w = R11F
regF R11b = R11F
regF R12 = R12F
regF R12d = R12F
regF R12w = R12F
regF R12b = R12F
regF R13 = R13F
regF R13d = R13F
regF R13w = R13F
regF R13b = R13F
regF R14 = R14F
regF R14d = R14F
regF R14w = R14F
regF R14b = R14F
regF R15 = R15F
regF R15d = R15F
regF R15w = R15F
regF R15b = R15F
regF Rip = RipF
