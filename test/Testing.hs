module Testing
  ( module Testing
  ) where

import Data.Function as Testing
import Test.Tasty as Testing
import Test.Tasty.HUnit as Testing hiding ((@=?))
import Test.Tasty.QuickCheck as Testing
import Text.Groom

assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq x y =
  if y == x
    then
      pure ()
    else do
      errorWithoutStackTrace $ unlines
        [ ""
        , ""
        , "Expected:"
        , "========="
        , ""
        , "" ++ groom y
        , ""
        , "But got:"
        , "========"
        , ""
        , "" ++ groom x
        ]

