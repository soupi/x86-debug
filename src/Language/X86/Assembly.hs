
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, BangPatterns #-}

module Language.X86.Assembly where

import Data.Data
import GHC.Generics
import Control.DeepSeq

import Data.Int (Int32)
import qualified Data.Sequence as S





type Code = S.Seq Line


data Line = Line
  { lineAnn   :: !Int32
  , lineLabel :: !(Maybe String)
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

data Address
  = Label !Label
  | AAE !ArithExpr
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Arg type
--   represents an x86 assembly argument to an instruction
data Arg
  = Ref !Arg
  | AE !ArithExpr
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data ArithExpr
  = Lit !Int32
  | Reg !Reg
  | Add !ArithExpr !ArithExpr
  | Mul !ArithExpr !ArithExpr
  | Sub !ArithExpr !ArithExpr
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
toCode insts =
  S.fromList $ zipWith (\l i -> Line l Nothing i) [0..] (insts ++ [IHalt])

