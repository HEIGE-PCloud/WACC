{-# LANGUAGE NamedFieldPuns #-}

module Language.WACC.X86.Translate where

import Control.Monad.RWS
  ( RWS
  , asks
  , evalRWS
  , get
  , gets
  , local
  , modify
  , put
  , tell
  )
import Data.Bimap hiding ((!))
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.Map (Map, findMin, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.WACC.TAC.TAC
import qualified Language.WACC.TAC.TAC as TAC
import Language.WACC.X86.X86
  ( Instr (..)
  , Label (..)
  , Memory (..)
  , Operand (..)
  , Prog
  , Register (R10, Rbp)
  , Runtime (..)
  )
import qualified Language.WACC.X86.X86 as X86

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg = D.toList . foldr (D.append . translateFunc) D.empty . M.elems

{- | Rbp points to the location just before the first stack variable of a frame
This means the last callee saved register pushed onto the stack
-}
translateFunc :: Func Integer Integer -> DList Instr
translateFunc (Func l v bs) = is
  where
    ((), is) =
      evalRWS
        (translateBlocks (TAC.Label n) startBlock)
        bs
        (TransST B.empty S.empty 0 S.empty []) -- Not empty regs list
    (n, startBlock) = M.findMin bs

{- | When registers run out and values in the stack are
required for a computation, they put in this register.
A caller saved register.
-}
swapReg :: Register
swapReg = R10

-- | The state of RWS monad for translation of each function
data TransST = TransST
  { alloc :: Bimap (Var Integer) Operand
  -- ^ bijection between variables in TAC to register/memory in X86
  , translated :: Set (TAC.Label Integer)
  -- ^ Set of basic blocks that have been translated
  , stackVarNum :: Integer
  -- ^ The number of stack variables used so far (not incl. saving callee saved reg)
  , runtimeFns :: Set Runtime
  -- ^ Set of runtime functions which need to be included
  , freeRegs :: [Register]
  -- ^ Unused registers
  }

-- | Reader maps labels to basic blocks. Writer holds output X86 program
type Analysis =
  RWS
    (Map Integer (BasicBlock Integer Integer))
    (DList Instr)
    TransST

-- | translate each statement of the block. then figure out which block to go to
translateBlocks
  :: TAC.Label Integer
  -> BasicBlock Integer Integer
  -> Analysis ()
translateBlocks l (BasicBlock is next) = do
  translateBlock l is
  translateNext next

-- | modify the state of the RWS monad
puts :: (Monoid w) => (s -> s) -> RWS r w s ()
puts f = do
  s <- get
  put (f s)

-- | Mark the block as translated, so its not re-translated
setTranslated :: (TAC.Label Integer) -> TransST -> TransST
setTranslated l x@(TransST {translated}) = x {translated = (S.insert l) translated}

-- | translate each TAC statement
translateBlock
  :: TAC.Label Integer
  -> [TAC Integer Integer]
  -> Analysis ()
translateBlock l@(TAC.Label n) is = do
  puts (setTranslated l) -- include label in translated set
  tellInstr (Lab (mapLab l))
  mapM translateTAC is
  return ()

mapLab :: (TAC.Label Integer) -> X86.Label
mapLab (Label x) = I x

tellInstr = tell . D.singleton

isTranslated :: TAC.Label Integer -> Analysis Bool
isTranslated l = gets ((S.member l) . translated)

{- | Flow control: For CJump translate l2 first then translate l1 after returning from the recursive call
This generates weird (but correct) assembly with else first then if. The if block appears roughly at the
end of the recursive call.
-}
translateNext :: Jump Integer Integer -> Analysis ()
translateNext (Jump l1@(Label n)) = do
  nextBlock <- asks (! n)
  t <- isTranslated l1
  case t of
    False -> translateBlocks l1 nextBlock
    True -> tellInstr (Jmp (mapLab l1))
translateNext (CJump v l1 l2@(TAC.Label n2)) = do
  operand <- gets ((B.! v) . alloc)
  tellInstr (Cmpq operand (Imm 0))
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
  translateNext (Jump l2)
  t <- isTranslated l1
  case t of
    False -> translateNext (Jump l1)
    True -> pure ()
translateNext (TAC.Ret var) = pure ()

addAlloc :: Var Integer -> Operand -> TransST -> TransST
addAlloc v o x@(TransST {alloc}) = x {alloc = (B.insert v o) alloc}

-- | Free a register if none are available by pushing the swapReg onto stack
getReg :: Analysis Register
getReg = do
  rs <- gets freeRegs
  case rs of
    [] -> do
      tellInstr (Pushq (Reg swapReg))
      swapVar <- gets ((B.!> (Reg swapReg)) . alloc)
      puts (\x@(TransST {stackVarNum}) -> x {stackVarNum = stackVarNum + 1})
      stackAddr <- (gets ((* 4) . stackVarNum))
      puts (addAlloc swapVar (Mem (MRegI stackAddr Rbp)))
      return swapReg
    (r : rs) -> do
      puts (\x -> x {freeRegs = rs})
      return r

translateBinOp :: BinOp -> (Operand -> Operand -> Instr)
translateBinOp = undefined -- TODO deal with division separately

translateTAC :: TAC Integer Integer -> Analysis ()
translateTAC (BinInstr lv rv1 op rv2) = do
  -- always assign new variables to a register
  r <- getReg
  puts (addAlloc lv (Reg r))
  op1 <- gets ((B.! rv1) . alloc)
  op2 <- gets ((B.! rv2) . alloc)
  tellInstr (Movq op1 (Reg r))
  tellInstr ((translateBinOp op) op2 (Reg r))
  tellInstr (Jo (R ErrOverflow))

-- translateTAC (UnInstr (Var ident) UnOp (Var ident)
-- translateTAC (Store (Var ident) (Offset ident) (Var ident) WType
-- translateTAC (LoadCI (Var ident) Int
-- translateTAC (LoadCS (Var ident) String
-- translateTAC (LoadM (Var ident) (Var ident) (Offset ident) WType
-- translateTAC (Call (Var ident) (Label lident) [Var ident]
-- translateTAC (Print (Var ident) WType
-- translateTAC (PrintLn (Var ident) WType
-- translateTAC (Exit (Var ident)
-- translateTAC (Read (Var ident) WType
-- translateTAC (TAC.Malloc lv rv) = translateTAC (TAC.Call lv (R X86.Malloc) [rv])
-- translateTAC (Free (Var ident)
