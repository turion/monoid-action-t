-- FIXME remove if we can upstream the [w] instance
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- base
import Data.Foldable (foldl')
import Data.Monoid (Endo (..))
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
import Data.Monoid.Action (Action (..))

-- monoid-action-t
import Control.Monad.MonoidAction.Class
import Control.Monad.Trans.MonoidAction

-- monoid-action-t-containers
import Data.Monoid.Action.IntMap

-- instance (Foldable f, Action w s) => Action (f w) s where
--   act ws s = foldl' (flip act) s ws
instance (Action w s) => Action [w] s where
  act ws s = foldl' (flip act) s ws

main :: IO ()
main =
  defaultMain $
    testGroup
      "IntMap"
      [ testCase "Can insert" $
          runMonoidAction (append [Insert 0 True :: IntMapChange' Bool]) mempty @?= ((), singleton 0 True)
      , testCase "Can read after insert" $
          let action = do
                append [Insert 0 True :: IntMapChange' Bool]
                m <- current
                append [Insert 0 False]
                return m
           in runMonoidAction action mempty @?= (singleton 0 True, singleton 0 False)
      , testCase "Can delete after insert" $
          let action = do
                append [Insert 0 True :: IntMapChange' Bool]
                append [Insert 1 False]
                append [Delete 0]
           in execMonoidAction action mempty @?= singleton 1 False
      , testCase "Can adjust after insert" $
          let action = do
                append [Insert 0 True :: IntMapChange Bool (Endo Bool)]
                append [Adjust 0 (Endo not)]
           in execMonoidAction action mempty @?= singleton 0 False
      , testCase "adjust only affects same key" $
          let action = do
                append [Insert 0 True :: IntMapChange Bool (Endo Bool)]
                append [Adjust 1 (Endo not)]
           in execMonoidAction action mempty @?= singleton 0 True
      ]
