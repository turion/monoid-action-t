module Main (main) where

-- base
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit

-- monoid-extras
import Data.Monoid.Action (Action (..))
import Control.Effect.Changeset (sendChange, sendCurrent, Changeset)
import Control.Monad.Trans.Changeset (Count(Increment), getChangeset, singleChange)
import Control.Monad.Changeset.Class (MonadChangeset(..))
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset


fusedEffects :: Has (Changeset Int (Changes Counter)) sig m => m Int
fusedEffects = do
  sendChange $ singleChange Increment
  n <- sendCurrent
  sendChange  $ singleChange Increment
  return n

transformer :: MonadChangeset Int (Changes Counter) m => m Int
transformer = do
  change $ singleChange Increment
  n <- current
  change $ singleChange Increment
  return n

main :: IO ()
main =
  defaultMain $
    testCase
      "fused-effects" $
      let
      in getChangeset fusedEffects (0 :: Int) @?= getChangeset transformer (0 :: Int)
