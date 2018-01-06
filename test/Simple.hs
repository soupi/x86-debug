
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
      , zipWith (\n t -> testCase ("Jumps  " ++ show n) t) [1..] jumps
      ]

qc :: [(Int32 -> Bool)]
qc =
  []

simple :: [Assertion]
simple =
  [ testReg EAX 7 $
    [ IMov (AE $ Var EAX) (AE $ Lit 7)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Var EAX) (AE $ Lit 5)
    , IAdd (AE $ Var EAX) (AE $ Lit 2)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Var EAX) (AE $ Lit 9)
    , ISub (AE $ Var EAX) (AE $ Lit 2)
    ]
  , testReg EAX 4 $
    [ IMov (AE $ Var EAX) (AE $ Lit 5)
    , IMov (AE $ Var EBX) (AE $ Lit 4)
    , IAnd (AE $ Var EAX) (AE $ Var EBX)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Var EAX) (AE $ Lit 5)
    , IMov (AE $ Var EBX) (AE $ Lit 2)
    , IXor (AE $ Var EAX) (AE $ Var EBX)
    ]
  , testReg EAX 5 $
    [ IMov (AE $ Var EAX) (AE $ Lit 1)
    , IMov (AE $ Var EBX) (AE $ Lit 5)
    , IOr  (AE $ Var EAX) (AE $ Var EBX)
    ]
  , testReg EAX 7 $
    [ IMov (AE $ Var EAX) (AE $ Lit 14)
    , ISar (AE $ Var EAX) (AE $ Lit 1)
    ]
  , testReg EAX 14 $
    [ IMov (AE $ Var EAX) (AE $ Lit 7)
    , ISal (AE $ Var EAX) (AE $ Lit 1)
    ]
  , testReg EAX 15 $
    [ IMov (AE $ Var EAX) (AE $ Lit 3)
    , IMul (AE $ Lit 5)
    ]
  , testFlag ZF True $
    [ IMov (AE $ Var EAX) (AE $ Lit 7)
    , IMov (AE $ Var EBX) (AE $ Lit 7)
    , ICmp (AE $ Var EAX) (AE $ Var EBX)
    ]
  , testFlag ZF True $
    [ IMov  (AE $ Var EAX) (AE $ Lit 5)
    , IMov  (AE $ Var EBX) (AE $ Lit 2)
    , ITest (AE $ Var EAX) (AE $ Var EBX)
    ]
  , testFlag ZF False $
    [ IMov (AE $ Var EAX) (AE $ Lit 7)
    , IMov (AE $ Var EBX) (AE $ Lit 8)
    , ICmp (AE $ Var EAX) (AE $ Var EBX)
    ]
  ]


jumps :: [Assertion]
jumps =
  [ testReg EAX 7 $
    [ IMov (AE $ Var EAX) (AE $ Lit 6)
    , IJmp $ Var $ AL "after"
    , IMov (AE $ Var EAX) (AE $ Lit 1)
    , Label "after"
    , IAdd (AE $ Var EAX) (AE $ Lit 1)
    ]
  ]


-- Utils --

testReg reg val code =
  assertEq
    (pure . getReg reg =<< getMachine =<< interpret [initMachine $ toCode code])
    (pure val)

testFlag flag val code =
  assertEq
    (pure . getFlag flag =<< getMachine =<< interpret [initMachine $ toCode code])
    (pure val)
