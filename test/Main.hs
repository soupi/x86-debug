module Main where

import Testing
import qualified Simple

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Simple.tests
    ]
