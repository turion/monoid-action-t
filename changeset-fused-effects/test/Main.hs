module Main (main) where

-- base
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit

-- falsify
-- import Test.Tasty.Falsify

-- monoid-extras
import Data.Monoid.Action (Action (..))

-- changeset

data Count = Increment

instance Action [Count] Int where
  act counts n = foldl' (\n' Increment -> n' + 1) n counts

main :: IO ()
main =
  defaultMain $
    testGroup
      "fused-effects"
      []
