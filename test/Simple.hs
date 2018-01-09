
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
      , zipWith (\n t -> testCase ("Stack  " ++ show n) t) [1..] stack
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
    , IJmp $ Var $ AL "end"
    , Label "after"
    , IAdd (AE $ Var EAX) (AE $ Lit 1)
    , Label "end"
    ]
  , testReg EBX 7 $
    [ ICmp (AE $ Var EAX) (AE $ Lit 0)
    , IJz $ Var $ AL "after"
    , IMov (AE $ Var EBX) (AE $ Lit 6)
    , IJmp $ Var $ AL "end"
    , Label "after"
    , IMov (AE $ Var EBX) (AE $ Lit 7)
    , Label "end"
    ]

  -- JGE
  , testReg EBX 7 $
    [ ICmp (AE $ Var EAX) (AE $ Lit 0)
    , IJge $ Var $ AL "after"
    , IMov (AE $ Var EBX) (AE $ Lit 6)
    , IJmp $ Var $ AL "end"
    , Label "after"
    , IMov (AE $ Var EBX) (AE $ Lit 7)
    , Label "end"
    ]
  , testReg EBX 7 $
    [ ICmp (AE $ Var EAX) (AE $ Lit (-1))
    , IJge $ Var $ AL "after"
    , IMov (AE $ Var EBX) (AE $ Lit 6)
    , IJmp $ Var $ AL "end"
    , Label "after"
    , IMov (AE $ Var EBX) (AE $ Lit 7)
    , Label "end"
    ]
  , testReg EBX 6 $
    [ ICmp (AE $ Var EAX) (AE $ Lit 1)
    , IJge $ Var $ AL "after"
    , IMov (AE $ Var EBX) (AE $ Lit 6)
    , IJmp $ Var $ AL "end"
    , Label "after"
    , IMov (AE $ Var EBX) (AE $ Lit 7)
    , Label "end"
    ]
  , testReg EAX 2 $
    [ IMov (AE (Var EAX)) (AE (Lit (-2147483647)))
    , ICmp (AE (Var EAX)) (AE (Lit 1))
    , IJe (Var (AL "if_false__0"))
    , IMov (AE (Var EAX)) (AE (Lit 2))
    , IJmp (Var (AL "if_done__0"))
    , Label "if_false__0"
    , IMov (AE (Var EAX)) (AE (Lit 7))
    , Label "if_done__0"
    ]
  ]


stack :: [Assertion]
stack =
  [ testReg EAX 7 $
    [ IMov  (AE $ Var EBX) (AE $ Lit 8)
    , IMov  (AE $ Var EAX) (AE $ Lit 1)
    , IPush (AE $ Var EAX)
    , IPush (AE $ Var EBX)
    , IPop (AE $ Var EAX)
    , IPop (AE $ Var EBX)
    , ISub (AE $ Var EAX) (AE $ Var EBX)
    ]
  ]

-- Utils --

testReg reg val code =
  assertEq'
    (getReg reg)
    (getMachine =<< interpret [initMachine $ toCode code])
    (pure val)

testFlag flag val code =
  assertEq'
    (getFlag flag)
    (getMachine =<< interpret [initMachine $ toCode code])
    (pure val)
