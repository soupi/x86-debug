module Testing
  ( module Testing
  ) where

import Language.X86
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


assertEq' :: (Show m, Eq a, Show a) => (m -> a) -> Either Error m -> (Either Error a) -> Assertion
assertEq' f m y =
  if y == fmap f m
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
        , "" ++ groom (f <$> m)
        , ""
        , "========"
        , "machine:"
        , ""
        , groom m
        ]
