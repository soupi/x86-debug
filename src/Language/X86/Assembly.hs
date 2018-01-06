
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, BangPatterns #-}

module Language.X86.Assembly where

import Data.Foldable (foldl')
import Data.Data
import GHC.Generics
import Control.DeepSeq

import Data.Int (Int32)
import qualified Data.Sequence as S
import qualified Data.Map as M





data Code = Code
  { cCode :: S.Seq Line
  , cLabelMap :: M.Map Label Int32
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)


data Line = Line
  { lineAnn   :: !Int32
  , lineInst  :: !Instruction
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Instruction type
--   represents an x86 assembly instruction
data Instruction
  = IMov  !Arg !Arg
  | IAdd  !Arg !Arg
  | ISub  !Arg !Arg
  | ICmp  !Arg !Arg
  | IXor  !Arg !Arg
  | IAnd  !Arg !Arg
  | IOr   !Arg !Arg
  | IShl  !Arg !Arg
  | IShr  !Arg !Arg
  | ISar  !Arg !Arg
  | ISal  !Arg !Arg
  | ITest !Arg !Arg
  | IMul  !Arg
  | Label !Label
  | IJmp  !Address
  | IJe   !Address
  | IJne  !Address
  | IJnz  !Address
  | IJz   !Address
  | ICall !Address
  | IPush !Arg
  | IPop  !Arg
  | IRet
  | IHalt
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

type Label = String

data AddressVar
  = AL Label
  | AR Reg
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

type Address = ArithExpr AddressVar

-- | The Arg type
--   represents an x86 assembly argument to an instruction
data Arg
  = Ref !Arg
  | AE !(ArithExpr Reg)
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data ArithExpr var
  = Lit !Int32
  | Var !var
  | Add !(ArithExpr var) !(ArithExpr var)
  | Mul !(ArithExpr var) !(ArithExpr var)
  | Sub !(ArithExpr var) !(ArithExpr var)
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Reg type
--   represents an x86 assembly register
data Reg
  = EAX
  | EBX
  | ECX
  | EDX
  | ESP
  | EBP
  | ESI
  | EIP
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

data Flag
  = ZF
  | CF
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)


data Loc
  = LocReg Reg
  | LocMem Int32
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)


toCode :: [Instruction] -> Code
toCode insts = Code
  { cCode =
    S.fromList
    . reverse
    . (Line lastNum IHalt:)
    $ rInstructions
  , cLabelMap = labelmap
  }
  where
    (labelmap, rInstructions, lastNum) = foldl' go (M.empty, [], 0) insts
    go (labels, rLines, num) = \case
      Label l ->
        (M.insert l num labels, rLines, num)
      inst ->
        (labels, Line num inst : rLines, num + 1)
