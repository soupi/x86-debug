{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns #-}

module Language.X86.Interpreter where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M

import Data.Int (Int32)
import Data.Bits
import Data.Maybe
import Data.Data
import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Control.Arrow (second)

import Language.X86.Assembly

import Debug.Trace


-----------
-- Types --
-----------

type State = [Machine]

data Machine = Machine
  { mMem   :: V.Vector Int32
  , mRegs  :: M.Map Reg Int32
  , mFlags :: M.Map Flag Bool
  , mCode  :: Code
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

evalArith :: (ref -> Either Error Int32) -> ArithExpr ref -> Either Error Int32
evalArith lookupVar = \case
  Lit i -> pure i
  Var var -> lookupVar var
  Add e1 e2 -> (+) <$> evalArith lookupVar e1 <*> evalArith lookupVar e2
  Sub e1 e2 -> (-) <$> evalArith lookupVar e1 <*> evalArith lookupVar e2
  Mul e1 e2 -> (*) <$> evalArith lookupVar e1 <*> evalArith lookupVar e2

data Error
  = Error Machine String ErrorType
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data ErrorType
  = StackOverflow
  | StackUnderflow
  | DivByZero
  | LabelNotFound Label
  | InvalidMem Int32
  | InvalidDest (ArithExpr Reg)
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
  AE e -> evalArith (pure . flip getReg m) e
  Ref arg -> do
    i <- evalArg m arg
    getMem i m

evalLoc :: Machine -> Arg -> Either Error Loc
evalLoc m = \case
  AE (Var r) -> pure $ LocReg r
  AE e -> throwError "evalLoc" m $ InvalidDest e
  arg -> LocMem <$> evalArg m arg

stepForward :: Machine -> Either Error Machine
stepForward machine@Machine{} =
  getInstruction machine >>= {- pure . traceShowId >>= -} \case
    IHalt -> pure machine
    IMov dest src -> applyDestSrc (flip const) dest src machine
    IAdd dest src -> applyDestSrc (+) dest src machine
    ISub dest src -> applyDestSrc (-) dest src machine
    IAnd dest src -> applyDestSrc (.&.) dest src machine
    IOr  dest src -> applyDestSrc (.|.) dest src machine
    IXor dest src -> applyDestSrc xor dest src machine
    ISal dest src -> applyDestSrc (\x y -> shiftL x (fromIntegral y)) dest src machine
    ISar dest src -> applyDestSrc (\x y -> shiftR x (fromIntegral y)) dest src machine
    IShl dest src -> applyDestSrc (\x y -> shiftL (clearBit x 31) (fromIntegral y)) dest src machine
    IShr dest src -> applyDestSrc (\x y -> shiftR (clearBit x 31) (fromIntegral y)) dest src machine
    IMul src -> do
      v <- evalArg machine src
      next $ overReg EAX (*v) machine
    ICmp dest src -> do
      evalDestSrc dest src machine >>= \case
        (LocReg r, v) -> do
          let rv = getReg r machine
          next $ setFlag ZF (rv == v) machine
        (LocMem i, v)  -> do
          mv <- getMem i machine
          next $ setFlag ZF (mv == v) machine
    ITest dest src -> do
      evalDestSrc dest src machine >>= \case
        (LocReg r, v) -> do
          let rv = getReg r machine
          next $ setFlag ZF (rv .&. v == 0) machine
        (LocMem i, v)  -> do
          mv <- getMem i machine
          next $ setFlag ZF (mv .&. v == 0) machine
    IJmp address ->
      setAddress address machine
    IJz address -> do
      if getFlag ZF machine
        then
          setAddress address machine
        else
          next machine
    IJnz address -> do
      if not (getFlag ZF machine)
        then
          setAddress address machine
        else
          next machine
    IJe address -> do
      if not (getFlag ZF machine)
        then
          setAddress address machine
        else
          next machine
    IJne address -> do
      if not (getFlag ZF machine)
        then
          setAddress address machine
        else
          next machine

    IPop dest ->
      next
        <=< checkStack
          . overReg ESP (\v -> v + 4)
        <=< applyDestSrc (flip const) dest (Ref $ AE $ Var ESP)
          $ machine

    IPush src -> do
      next
        <=< applyDestSrc (flip const) (Ref $ AE $ Var ESP) src
        <=< checkStack
          . overReg ESP (\v -> v - 4)
          $ machine

    ICall addr -> do
      setAddress addr
        <=< applyDestSrc (flip const) (Ref $ AE $ Var ESP) (AE $ Add (Var EIP) (Lit 4))
        <=< checkStack
          . overReg ESP (\v -> v - 4)
          $ machine

    IRet -> do
      uncurry setAddress
        <=< secondF checkStack
          . second  (overReg ESP (\v -> v + 4))
        <=< secondF (applyDestSrc (flip const) (AE $ Var EIP) (Ref $ AE $ Var ESP))
          $ (Lit $ getReg EIP machine, machine)


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
  { mRegs  = M.fromList
    [ (EIP, memSize * 4)
    , (ESP, memSize * 4 - 4)
    ]
  , mMem   = V.replicate memSize 0
  , mFlags = M.empty
  , mCode  = code
    { cCode = cCode code
    , cLabelMap = fmap ((memSize * 4 +) . (4*)) $ cLabelMap code
    }
  }
  where
    memSize :: forall a. Integral a => a
    memSize = 4096

---------------
-- Accessors --
---------------

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
    (S.lookup (fromIntegral ip) (cCode $ mCode machine))

getReg :: Reg -> Machine -> Int32
getReg reg = fromMaybe 0 . M.lookup reg . mRegs

setReg :: Reg -> Int32 -> Machine -> Machine
setReg reg val machine =
  machine { mRegs = M.alter (const $ Just val) reg (mRegs machine) }

overReg :: Reg -> (Int32 -> Int32) -> Machine -> Machine
overReg reg f machine =
  machine { mRegs = M.alter (Just . f . fromMaybe 0) reg (mRegs machine) }

getFlag :: Flag -> Machine -> Bool
getFlag flag = fromMaybe False . M.lookup flag . mFlags

setFlag :: Flag -> Bool -> Machine -> Machine
setFlag flag val machine =
  machine { mFlags = M.alter (const $ Just val) flag (mFlags machine) }

overFlag :: Flag -> (Bool -> Bool) -> Machine -> Machine
overFlag flag f machine =
  machine { mFlags = M.alter (Just . f . fromMaybe False) flag (mFlags machine) }

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

---

next :: Machine -> Either Error Machine
next = pure . overReg EIP (+4)

evalDestSrc :: Arg -> Arg -> Machine -> Either Error (Loc, Int32)
evalDestSrc dest src state =
  (,) <$> evalLoc state dest <*> evalArg state src

applyDestSrc :: (Int32 -> Int32 -> Int32) -> Arg -> Arg -> Machine -> Either Error Machine
applyDestSrc f dest src machine =
  evalDestSrc dest src machine >>= \case
    (LocReg r, v) -> do
      next $ overReg r (`f` v) machine
    (LocMem i, v)  ->
      next =<< overMem i (`f` v) machine

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

setAddress :: Address -> Machine -> Either Error Machine
setAddress address machine =
  flip (setReg EIP) machine
    <$> evalArith (flip lookupAddrVar machine) address
  where
    lookupAddrVar var m = case var of
      AR r -> pure $ getReg r m
      AL l -> case M.lookup l . cLabelMap . mCode $ m of
        Nothing ->
          throwError "lookupAddrVar" m $ LabelNotFound l
        Just addr ->
          pure addr



checkStack :: Machine -> Either Error Machine
checkStack m
  | getReg ESP m >= fromIntegral (length $ mMem m) =
    throwError "stepForward:IPush" m $ StackUnderflow
  | getReg ESP m < 0 =
    throwError "stepForward:IPush" m $ StackOverflow
  | otherwise =
    pure m

secondF :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
secondF f (a, b) = (a,) <$> f b