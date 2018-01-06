
module Simple where

import Data.Int
import Testing

import Language.X86

tests :: TestTree
tests =
  testGroup "Simple" $
    mconcat
      [ zipWith (\n t -> testProperty ("QuickCheck " ++ show n) t) [1..] qc
      , zipWith (\n t -> testCase ("Simple " ++ show n) t) [1..] simple
      ]

qc :: [(Int32 -> Bool)]
qc =
  []

simple :: [Assertion]
simple =
  [ testReg EAX 7 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 7)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 5)
    , IAdd (AE $ Reg EAX) (AE $ Lit 2)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 9)
    , ISub (AE $ Reg EAX) (AE $ Lit 2)
    ]
  , testReg EAX 4 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 5)
    , IMov (AE $ Reg EBX) (AE $ Lit 4)
    , IAnd (AE $ Reg EAX) (AE $ Reg EBX)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 5)
    , IMov (AE $ Reg EBX) (AE $ Lit 2)
    , IXor (AE $ Reg EAX) (AE $ Reg EBX)
    ]
  , testReg EAX 5 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 1)
    , IMov (AE $ Reg EBX) (AE $ Lit 5)
    , IOr  (AE $ Reg EAX) (AE $ Reg EBX)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 14)
    , ISar (AE $ Reg EAX) (AE $ Lit 1)
    ]
  , testReg EAX 14 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 7)
    , ISal (AE $ Reg EAX) (AE $ Lit 1)
    ]
  , testReg EAX 15 $
    [ IMov (AE $ Reg EAX) (AE $ Lit 3)
    , IMul (AE $ Lit 5)
    ]
  , testFlag ZF True $
    [ IMov (AE $ Reg EAX) (AE $ Lit 7)
    , IMov (AE $ Reg EBX) (AE $ Lit 7)
    , ICmp (AE $ Reg EAX) (AE $ Reg EBX)
    ]
  , testFlag ZF True $
    [ IMov  (AE $ Reg EAX) (AE $ Lit 5)
    , IMov  (AE $ Reg EBX) (AE $ Lit 2)
    , ITest (AE $ Reg EAX) (AE $ Reg EBX)
    ]
  , testFlag ZF False $
    [ IMov (AE $ Reg EAX) (AE $ Lit 7)
    , IMov (AE $ Reg EBX) (AE $ Lit 8)
    , ICmp (AE $ Reg EAX) (AE $ Reg EBX)
    ]
  ]

testReg reg val code =
  assertEq
    (pure . getReg reg =<< getMachine =<< interpret [initMachine $ toCode code])
    (pure val)

testFlag flag val code =
  assertEq
    (pure . getFlag flag =<< getMachine =<< interpret [initMachine $ toCode code])
    (pure val)
