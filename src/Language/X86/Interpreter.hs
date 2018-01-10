{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns #-}

module Language.X86.Interpreter where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as Set

import Data.Int (Int32)
import Data.Bits
import Data.Maybe
import Data.Data
import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Control.Arrow (second)

import Language.X86.Assembly

-- import Debug.Trace


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
  = Error Machine String (Maybe Line) ErrorType
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
  | UnexpectedInstruction
  | Unexpected String
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

throwError :: String -> Machine -> ErrorType -> Either Error a
throwError f m = Left . Error m f mline
  where
    mline = either (const Nothing) Just $ getInstLine m

----------------
-- Evaluators --
----------------

isBreakpoint :: Machine -> Either Error Bool
isBreakpoint m = do
  lineNum <- lineAnn <$> getInstLine m
  pure $
    let
      lineNum' = (fromIntegral (length $ mMem m) * 4 + 4 * lineNum)
    in
      lineNum' `elem` cBreakpoints (mCode m)

isHalt :: Machine -> Either Error Bool
isHalt m = (==IHalt) <$> getInstruction m

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
  arg' -> LocMem <$> evalDest arg'
    where
      evalDest :: Arg -> Either Error Int32
      evalDest = \case
        AE e -> evalArith (pure . flip getReg m) e
        Ref arg -> do
          evalDest arg


stepForward :: Machine -> Either Error Machine
stepForward machine@Machine{} =
  getInstruction machine >>= {- pure . traceShowId >>= -} \case
    Label{} -> throwError "stepForward" machine UnexpectedInstruction
    IHalt -> pure machine
    IMov dest src ->
      next =<< applyDestSrc Nothing (flip const) dest src machine
    IAdd dest src ->
      next =<< applyDestSrc (Just $ \x y -> x + y > fromIntegral (maxBound :: Int32) || (x + y < fromIntegral (minBound :: Int32))) (+) dest src machine
    ISub dest src ->
      next =<< applyDestSrc (Just $ \x y -> x - y > fromIntegral (maxBound :: Int32) || (x - y < fromIntegral (minBound :: Int32))) (-) dest src machine
    IAnd dest src ->
      next =<< applyDestSrc Nothing (.&.) dest src machine
    IOr  dest src ->
      next =<< applyDestSrc Nothing (.|.) dest src machine
    IXor dest src ->
      next =<< applyDestSrc Nothing xor dest src machine
    ISal dest src ->
      next =<< applyDestSrc Nothing (\x y -> shiftL x (fromIntegral y)) dest src machine
    ISar dest src ->
      next =<< applyDestSrc Nothing (\x y -> shiftR x (fromIntegral y)) dest src machine
    IShl dest src ->
      next =<< applyDestSrc Nothing (\x y -> shiftL (clearBit x 31) (fromIntegral y)) dest src machine
    IShr dest src ->
      next =<< applyDestSrc Nothing (\x y -> shiftR (clearBit x 31) (fromIntegral y)) dest src machine
    IMul src -> do
      v <- evalArg machine src
      next $ overReg EAX (*v) machine
    ICmp dest src -> do
      evalDestSrc dest src machine >>= \case
        (LocReg r, v) -> do
          let rv = getReg r machine
          let (rv', v') = (fromIntegral rv, fromIntegral v :: Integer)
          next
            . setFlag CF (rv < v)
            . setFlag SF (0 > rv - v)
            . setFlag OF (rv' - v' > fromIntegral (maxBound :: Int32) || (rv' - v' < fromIntegral (minBound :: Int32)))
            . setFlag ZF (rv == v)
            $ machine
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
      if getFlag ZF machine
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
    IJge address -> do
      if getFlag SF machine == getFlag OF machine
        then
          setAddress address machine
        else
          next machine

    IPop dest ->
      next
        <=< checkStack
          . overReg ESP (\v -> v + 4)
        <=< applyDestSrc Nothing (flip const) dest (Ref $ AE $ Var ESP)
          $ machine

    IPush src -> do
      next
        <=< applyDestSrc Nothing (flip const) (Ref $ AE $ Var ESP) src
        <=< checkStack
          . overReg ESP (\v -> v - 4)
          $ machine

    ICall addr -> do
      setAddress addr
        <=< applyDestSrc Nothing (flip const) (Ref $ AE $ Var ESP) (AE $ Add (Var EIP) (Lit 4))
        <=< checkStack
          . overReg ESP (\v -> v - 4)
          $ machine

    IRet -> do
      uncurry setAddress
        <=< secondF checkStack
          . second  (overReg ESP (\v -> v + 4))
        <=< secondF (applyDestSrc Nothing (flip const) (AE $ Var EIP) (Ref $ AE $ Var ESP))
          $ (Lit $ getReg EIP machine, machine)


interpretStep :: State -> Either Error State
interpretStep !state = do
  !machine <- getMachine state
  getInstruction machine >>= \case
    IHalt -> pure state
    _ -> do
      m' <- stepForward machine
      pure (m' : state)

interpret :: State -> Either Error State
interpret !state = do
  !machine <- getMachine state
  getInstruction machine >>= \case
    IHalt -> pure state
    _ -> do
      m' <- stepForward machine
      interpret (m' : state)

interpretBreak :: State -> Either Error State
interpretBreak !state = do
  !machine@Machine{mCode} <- getMachine state
  getInstruction machine >>= \case
    IHalt -> pure state
    _ -> do
      m' <- stepForward machine
      if getReg EIP m' `elem` cBreakpoints mCode
        then
          pure (m' : state)
        else
          interpretBreak (m' : state)

getLabelLineNum :: Label -> Machine -> Maybe Int32
getLabelLineNum lbl Machine{mCode} =
  M.lookup lbl (cLabelMapOrig mCode)

addBreakpoint :: Label -> Machine -> Either String Machine
addBreakpoint lbl machine@Machine{mCode} =
  case M.lookup lbl (cLabelMap mCode) of
    Just i
      | i `notElem` cBreakpoints mCode ->
        pure $ machine
        { mCode =
          mCode
          { cBreakpoints = i `Set.insert` cBreakpoints mCode
          }
        }
    _ ->
      Left "Could not find label."

addBreakpointLine :: Int32 -> Machine -> Either String Machine
addBreakpointLine lineNum machine@Machine{mCode, mMem}
  | lineNum > 0 && fromIntegral lineNum < length (cCode mCode) =
    let
      lineNum' = (fromIntegral (length mMem) * 4 + 4 * lineNum)
    in if
      | lineNum' `notElem` cBreakpoints mCode ->
        pure machine
          { mCode =
            mCode
            { cBreakpoints =
              Set.insert
                lineNum'
                (cBreakpoints mCode)
            }
          }
      | otherwise ->
        Left "Breakpoint already set."
  | otherwise = Left "Invalid line number."


removeBreakpoint :: Label -> Machine -> Machine
removeBreakpoint lbl machine@Machine{mCode} =
  case M.lookup lbl (cLabelMap mCode) of
    Just i ->
        machine
        { mCode =
          mCode
          { cBreakpoints = i `Set.delete` cBreakpoints mCode
          }
        }
    _ ->
      machine


-----------
-- Utils --
-----------

initMachine :: Code -> Machine
initMachine code = Machine
  { mRegs  = M.fromList
    [ (EIP, memSize * 4)
    , (ESP, memSize * 4 - 4*4)
    ]
  , mMem   = V.replicate memSize 0
  , mFlags = M.empty
  , mCode  = code
    { cLabelMap =
      fmap ((memSize * 4 +) . (4*)) $ cLabelMap code
    , cBreakpoints =
      Set.map ((memSize * 4 +) . (4*)) $ cBreakpoints code
    }
  }
  where
    memSize :: forall a. Integral a => a
    memSize = 100

---------------
-- Accessors --
---------------

getMachine :: State -> Either Error Machine
getMachine = \case
  m:_ -> pure m
  _ -> throwError "getMachine" undefined UnexpectedNoMachine

getInstruction :: Machine -> Either Error Instruction
getInstruction machine = lineInst <$> getInstLine machine

getInstLine :: Machine -> Either Error Line
getInstLine machine = do
  let eip = getReg EIP machine
  ip <- evalIndex (eip - fromIntegral (4 * (length $ mMem machine))) machine
  maybe
    (throwError "getInstruction" machine $ InvalidMem eip)
    pure
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

applyDestSrc
  :: Maybe (Int32 -> Int32 -> Bool)
  -> (Int32 -> Int32 -> Int32) -> Arg -> Arg -> Machine -> Either Error Machine
applyDestSrc setOFIf f dest src machine =
  let
    setFlags v1 v2 = maybe
      id
      (\test -> setFlag OF (test v1 v2))
      setOFIf
  in evalDestSrc dest src machine >>= \case
    (LocReg r, v) -> do
      let vr = getReg r machine
      pure
        . setFlags vr v
        $ overReg r (`f` v) machine
    (LocMem i, v)  -> do
      vm <- getMem i machine
      pure . setFlags vm v =<< overMem i (`f` v) machine

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
  | getReg ESP m >= 4 * fromIntegral (length $ mMem m) =
    throwError "checkStack:UF" m $ StackUnderflow
  | getReg ESP m < 0 =
    throwError "checkStack:OF" m $ StackOverflow
  | otherwise =
    pure m

secondF :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
secondF f (a, b) = (a,) <$> f b
