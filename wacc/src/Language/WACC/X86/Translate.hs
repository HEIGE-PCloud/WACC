{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.WACC.X86.Translate where

import Control.Monad.RWS
  ( RWS
  , asks
  , execRWS
  , get
  , gets
  , put
  , tell
  )
import Data.Bimap hiding (map, (!))
import qualified Data.Bimap as B
import Data.DList (DList)
import qualified Data.DList as D
import Data.List ((\\))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.WACC.TAC.TAC
import qualified Language.WACC.TAC.TAC as TAC
import Language.WACC.X86.Runtime (runtimeLib)
import Language.WACC.X86.X86
  ( Directive (..)
  , Instr (..)
  , Label (..)
  , Memory (..)
  , Operand (..)
  , Prog
  , Register (R10, Rax, Rbp, Rsp)
  , Runtime (..)
  , argRegs
  , callee
  , caller
  )
import qualified Language.WACC.X86.X86 as X86

-- | Translate every function's instructions and concat
translateProg :: TACProgram Integer Integer -> Prog
translateProg p = D.toList $ preamble `D.append` (D.concat is) `D.append` (D.concat runtime)
  where
    runtime :: [DList Instr]
    runtime = (runtimeLib !) <$> (S.toList (S.unions runtimeLs))
    (runtimeLs, is) = unzip $ map translateFunc (M.elems p)
    preamble :: DList Instr
    preamble =
      D.fromList
        [ Dir $ DirGlobl (S "main")
        , Dir $ DirSection
        , Dir $ DirRodata
        , Dir $ DirText
        ]

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

{- | Rbp points to the location just before the first stack variable of a frame
This means the last callee saved register pushed onto the stack

Assuming stack looks like this: (initial value of Rsp and constant value of Rbp)

--------------
Extra Arg: 1
.
.
Extra Arg: n     <-- Rsp
--------------
Callee Saved: 1
.
.
Callee Saved: m  <-- Rbp
-------------
Local Regs: 1
.
.
-}
translateFunc :: Func Integer Integer -> (Set Runtime, DList Instr)
translateFunc (Func l vs bs) = (runtimeFns st, is)
  where
    (st, is) =
      execRWS
        funcRWS
        bs
        (TransST B.empty S.empty 0 S.empty ((callee \\ [Rbp]) ++ caller)) -- Not empty regs list
    (nStart, startBlock) = M.findMin bs

    funcRWS = do
      mapM_ (tellInstr . Pushq . Reg) callee -- callee saving registers
      tellInstr (X86.Movq (Reg Rsp) (Reg Rbp)) -- set the stack base pointer
      mapM_ setupRegArgs (zip vs argRegs)
      puts
        ( \x@(TransST {freeRegs}) -> x {freeRegs = freeRegs ++ (drop (length vs) argRegs)} -- mark extra arg regs as usable
        )
      -- \| if more than 6 arguments, subtract from rsp (more than the number of callee saved) to get stack position
      mapM_
        setupStackArgs
        ( zip
            (drop (length argRegs) vs)
            (map (\x -> (-x) - toInteger (length callee)) [0 ..])
        )
      translateBlocks (TAC.Label nStart) startBlock -- translate main part of code
      svn <- gets stackVarNum
      tellInstr (X86.Addq (Imm (-svn)) (Reg Rsp)) -- effectively delete local variables on stack
      mapM_ (tellInstr . Popq . Reg) (reverse callee) -- callee saving registers
      -- assigning arg vars to registers
    setupRegArgs :: (Var Integer, Register) -> Analysis ()
    setupRegArgs (v, r) = puts (addAlloc v (Reg r))
    -- assigning extra arg vars to stack
    setupStackArgs :: (Var Integer, Integer) -> Analysis ()
    setupStackArgs (v, n) = puts (addAlloc v (Mem (MRegI n Rbp)))

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
translateBlock l is = do
  puts (setTranslated l) -- include label in translated set
  tellInstr (Lab (mapLab l))
  mapM_ translateTAC is
  return ()

mapLab :: (TAC.Label Integer) -> X86.Label
mapLab (Label x) = I x

tellInstr :: Instr -> Analysis ()
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
translateNext (CJump v l1 l2) = do
  operand <- gets ((B.! v) . alloc)
  tellInstr (X86.Cmpq operand (Imm 0))
  tellInstr (Jne (mapLab l1)) -- jump to l1 if v != 0. Otherwise keep going
  translateNext (Jump l2)
  t <- isTranslated l1
  case t of
    False -> translateNext (Jump l1)
    True -> pure ()
translateNext (TAC.Ret var) = do
  retVal <- gets ((B.! var) . alloc)
  tellInstr (Movl retVal (Reg Rax))

addAlloc :: Var Integer -> Operand -> TransST -> TransST
addAlloc v o x@(TransST {alloc}) = x {alloc = (B.insert v o) alloc}

-- | Free a register if none are available by pushing the swapReg onto stack
getReg :: Analysis Register
getReg = do
  rss <- gets freeRegs
  case rss of
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
  tellInstr (X86.Movq op1 (Reg r))
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
