{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns #-}

module Language.X86.Interpreter where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M

import Data.Int (Int32)
import Data.Maybe
import Data.Data
import GHC.Generics
import Control.DeepSeq

import Language.X86.Assembly

import Debug.Trace


-----------
-- Types --
-----------

type State = [Machine]

data Machine = Machine
  { mMem  :: V.Vector Int32
  , mRegs :: M.Map Reg Int32
  , mZf   :: !Bool
  , mCode :: Code
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

evalArith :: Machine -> ArithExpr -> Int32
evalArith m@Machine{..} = \case
  Lit i -> i
  Reg reg -> getReg reg m
  Add e1 e2 -> evalArith m e1 + evalArith m e2
  Sub e1 e2 -> evalArith m e1 - evalArith m e2
  Mul e1 e2 -> evalArith m e1 * evalArith m e2

data Error
  = Error Machine String ErrorType
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data ErrorType
  = StackOverflow
  | StackUnderflow
  | DivByZero
  | LabelNotFound Label
  | InvalidMem Int32
  | InvalidDest ArithExpr
  | InstructionNotFound Int32
  | UnexpectedNoMachine
  | Unexpected String
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

throwError :: String -> Machine -> ErrorType -> Either Error a
throwError f m = Left . Error m f

----------------
-- Evaluators --
----------------

evalArg :: Machine -> Arg -> Either Error Int32
evalArg m = \case
  AE e -> pure $ evalArith m e
  Ref arg -> do
    i <- evalArg m arg
    getMem i m

evalLoc :: Machine -> Arg -> Either Error Loc
evalLoc m = \case
  AE (Reg r) -> pure $ LocReg r
  AE e -> throwError "evalLoc" m $ InvalidDest e
  arg -> LocMem <$> evalArg m arg

stepForward :: Machine -> Either Error Machine
stepForward machine@Machine{} =
  getInstruction machine >>= {- pure . traceShowId >>= -} \case
    IHalt -> pure machine
    IMov dest src ->
      evalDestSrc dest src machine >>= \case
        (LocReg r, v) -> do
          next $ setReg r v machine
          
        (LocMem i, v)  ->
          next =<< setMem i v machine

interpret :: State -> Either Error State
interpret !state = do
  !machine <- getMachine state
  getInstruction machine >>= \case
    IHalt -> pure state
    _ -> do
      m' <- stepForward machine
      interpret (m' : state)


-----------
-- Utils --
-----------

initMachine :: Code -> Machine
initMachine code = Machine
  { mRegs = M.fromList [(EIP, memSize * 4)]
  , mMem  = V.replicate memSize 0
  , mZf   = False
  , mCode = code
  }
  where
    memSize :: forall a. Integral a => a
    memSize = 4096

getMachine :: State -> Either Error Machine
getMachine = \case
  m:_ -> pure m
  _ -> throwError "getMachine" undefined UnexpectedNoMachine

getInstruction :: Machine -> Either Error Instruction
getInstruction machine = do
  let eip = getReg EIP machine
  ip <- evalIndex (eip - fromIntegral (4 * (length $ mMem machine))) machine
  maybe
    (throwError "getInstruction" machine $ InvalidMem eip)
    (pure . lineInst)
    (S.lookup (fromIntegral ip) (mCode machine))

getReg :: Reg -> Machine -> Int32
getReg reg = fromMaybe 0 . M.lookup reg . mRegs

setReg :: Reg -> Int32 -> Machine -> Machine
setReg reg val machine =
  machine { mRegs = M.alter (const $ Just val) reg (mRegs machine) }

overReg :: Reg -> (Int32 -> Int32) -> Machine -> Machine
overReg reg f machine =
  machine { mRegs = M.alter (Just . f . fromMaybe 0) reg (mRegs machine) }

next :: Machine -> Either Error Machine
next = pure . overReg EIP (+4)

evalDestSrc :: Arg -> Arg -> Machine -> Either Error (Loc, Int32)
evalDestSrc dest src state =
  (,) <$> evalLoc state dest <*> evalArg state src

getMem :: Int32 -> Machine -> Either Error Int32
getMem i m = do
  index <- evalIndex i m
  pure $ mMem m V.! index

setMem :: Int32 -> Int32 -> Machine -> Either Error Machine
setMem i val machine = do
  index <- evalIndex i machine
  pure $ machine { mMem = mMem machine V.// [(fromIntegral index, val)] }

overMem :: Int32 -> (Int32 -> Int32) -> Machine -> Either Error Machine
overMem i f machine = do
  index <- evalIndex i machine
  pure $ machine
    { mMem =
        mMem machine
        V.// [(fromIntegral index, f $ mMem machine V.! fromIntegral index)]
    }

evalIndex :: Integral a => Int32 -> Machine -> Either Error a
evalIndex i m = do
  index <-
    if
      | i == 0 ->
        pure 0
      | i `mod` 4 == 0 ->
        pure $ i `div` 4
      | otherwise ->
        throwError "evalIndex" m $ InvalidMem i
  
  if
    | index < 0 ->
      throwError "evalIndex2" m $ InvalidMem index
    | fromIntegral index >= length (mMem m) ->
      throwError "evalIndex3" m $ InvalidMem index
    | otherwise ->
      pure $ fromIntegral index
