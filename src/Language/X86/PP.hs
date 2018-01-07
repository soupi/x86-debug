
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Language.X86.PP where

import Data.Monoid
import Data.Char (toLower)
import Data.Data
import GHC.Generics
import Control.DeepSeq

import Language.X86.Assembly




-- | Output
newtype Assembly = Assembly String
  deriving (Eq, Ord, Generic, NFData, Data, Typeable)

instance Show Assembly where
  show (Assembly asmStr) = asmStr

-----------------
-- PP Assembly --
-----------------

-- | Pretty print a list of instructions to an assembly string
ppAsm :: [Instruction] -> String
ppAsm = unlines . map ppInstruction

-- | Pretty print an instruction to an assembly string
ppInstruction :: Instruction -> String
ppInstruction = \case
  IMov dest src ->
    ppOp "mov" dest src
  IAdd dest src ->
    ppOp "add" dest src
  ISub dest src ->
    ppOp "sub" dest src
  IMul dest ->
    "mul " <> ppArg dest
  IXor dest src ->
    ppOp "xor" dest src
  IAnd dest src ->
    ppOp "and" dest src
  IOr  dest src ->
    ppOp "or"  dest src
  ICmp dest src ->
    ppOp "cmp" dest src
  ITest dest src ->
    ppOp "test" dest src
  IShr dest src ->
    ppOp "shr" dest src
  ISar dest src ->
    ppOp "sar" dest src
  IShl dest src ->
    ppOp "shl" dest src
  ISal dest src ->
    ppOp "sal" dest src
  IJmp lbl ->
    "jmp " <> ppAddress lbl
  IJe  lbl ->
    "je " <> ppAddress lbl
  IJne  lbl ->
    "jne " <> ppAddress lbl
  IJnz lbl ->
    "jnz " <> ppAddress lbl
  IJz lbl ->
    "jz " <> ppAddress lbl
  Label lbl ->
    lbl <> ":"
  IRet ->
    "ret"
  ICall lbl ->
    "call " <> ppAddress lbl
  IPush arg ->
    "push " <> ppArg arg
  IPop arg ->
    "pop " <> ppArg arg
  IHalt ->
    ""

-- | Pretty print a label
ppAddress :: Address -> String
ppAddress = ppAE $ \case
  AL l -> l
  AR r -> show r

-- | Pretty print an operation with two arguments
ppOp :: String -> Arg -> Arg -> String
ppOp cmd dest src =
    unwords
      [ cmd
      , ppArg dest <> ","
      , ppArg src
      ]

-- | Pretty print an argument
ppArg :: Arg -> String
ppArg = \case
  Ref a -> "[" ++ ppArg a ++ "]"
  AE a  -> ppAE show a

-- | Pretty print an argument
ppAE :: (var -> String) -> ArithExpr var -> String
ppAE ppVar e = go (normalizeAE e)
  where
    go = \case
      Neg v -> "-" <> pp v
      Pos v -> pp v
    pp = \case
      Lit i -> show i
      Var v -> ppVar v
      Add e1 e2 -> pp e1 <> " + " <> pp e2
      Sub e1 e2 -> pp e1 <> " - " <> pp e2
      Mul e1 e2 -> pp e1 <>  "*"  <> pp e2

normalizeAE :: ArithExpr var -> Sign (ArithExpr var)
normalizeAE = \case
  Lit i
    | i < 0 -> Neg $ Lit $ 0 - i
    | otherwise -> Pos $ Lit i
  Var v -> Pos $ Var v
  Add e1 e2 ->
    case (normalizeAE e1, normalizeAE e2) of
      (Neg v1, Pos v2) -> Pos $ Sub v2 v1
      (Pos v1, Neg v2) -> Pos $ Sub v1 v2
      (Pos v1, Pos v2) -> Pos $ Add v1 v2
      (Neg v1, Neg v2) -> Neg $ Sub v1 v2
  Sub e1 e2 ->
    case (normalizeAE e1, normalizeAE e2) of
      (Neg v1, Pos v2) -> Neg $ Sub v1 v2
      (Pos v1, Neg v2) -> Pos $ Add v1 v2
      (Pos v1, Pos v2) -> Pos $ Sub v1 v2
      (Neg v1, Neg v2) -> Pos $ Sub v2 v1
  Mul e1 e2 ->
    case (normalizeAE e1, normalizeAE e2) of
      (Neg v1, Pos v2) -> Neg $ Mul v1 v2
      (Pos v1, Neg v2) -> Neg $ Mul v2 v1
      (Pos v1, Pos v2) -> Pos $ Mul v1 v2
      (Neg v1, Neg v2) -> Pos $ Mul v1 v2

data Sign v
  = Neg v
  | Pos v
  deriving Functor

-- | Pretty print a register
ppReg :: Reg -> String
ppReg = map toLower . show
