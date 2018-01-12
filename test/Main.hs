module Main where

import Testing
import qualified Simple
import qualified Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Simple.tests
    , Parser.tests
    ]
