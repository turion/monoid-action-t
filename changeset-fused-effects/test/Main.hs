module Main (main) where

-- base
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- falsify
-- import Test.Tasty.Falsify

-- monoid-extras
import Data.Monoid.Action (Action (..))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

data Count = Increment

instance Action [Count] Int where
  act counts n = foldl' (\n' Increment -> n' + 1) n counts

main :: IO ()
main =
  defaultMain $
    testGroup
      "ChangesetT"
      [ testGroup
          "Changeset"
          [ testGroup
              "Order of change and current matters"
              [ testCase "change, current" $
                  evalChangeset (change [Increment] >> current) 0 @?= (1 :: Int)
              , testCase "current, change" $
                  evalChangeset (current >>= (\n -> change [Increment] >> return n)) 0 @?= (0 :: Int)
              ]
          ]
      ]
