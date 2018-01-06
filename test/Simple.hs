
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
  ]

testReg reg val code =
  assertEq
    (pure . getReg reg =<< getMachine =<< interpret [initMachine $ toCode code])
    (pure val)
