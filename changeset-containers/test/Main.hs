-- FIXME remove if we can upstream the [w] instance
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- base
import Data.Foldable (foldl')
import Prelude hiding (Foldable (..))

-- containers
import Data.IntMap.Strict (singleton)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- falsify
-- import Test.Tasty.Falsify

-- monoid-extras
import Data.Monoid.RightAction (RightAction (..))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.IntMap

-- instance (Foldable f, RightAction w s) => RightAction (f w) s where
--   act ws s = foldl' (flip act) s ws
instance (RightAction w s) => RightAction [w] s where
  actRight = foldl' actRight

main :: IO ()
main =
  defaultMain $
    testGroup
      "IntMap"
      [ testCase "Can insert" $
          runChangeset (change [Insert 0 True :: IntMapChange Bool]) mempty @?= ((), singleton 0 True)
      , testCase "Can read after insert" $
          let action = do
                change [Insert 0 True :: IntMapChange Bool]
                m <- current
                change [Insert 0 False]
                return m
           in runChangeset action mempty @?= (singleton 0 True, singleton 0 False)
      , testCase "Can delete after insert" $
          let action = do
                change [Insert 0 True :: IntMapChange Bool]
                change [Insert 1 False]
                change [Delete 0]
           in execChangeset action mempty @?= singleton 1 False
      ]
