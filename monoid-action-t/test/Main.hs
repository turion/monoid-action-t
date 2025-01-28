{-# OPTIONS_GHC -Wno-orphans #-}

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

-- monoid-action-t
import Control.Monad.MonoidAction.Class
import Control.Monad.Trans.MonoidAction

type M = MonoidAction Int [Count]

data Count = Increment

instance Action Count Int where
  act Increment count = count + 1

-- instance Action [Count] Int where
--   act counts n = foldl' (\n' Increment -> n' + 1) n counts
instance (Action w s) => Action [w] s where
  act ws s = foldl' (flip act) s ws

main :: IO ()
main =
  defaultMain $
    testGroup
      "MonoidActionT"
      [ testGroup
          "MonoidAction"
          [ testGroup
              "Order of append and current matters"
              [ testCase "append, current" $
                  evalMonoidAction (append [Increment] >> current) 0 @?= (1 :: Int)
              , testCase "current, append" $
                  evalMonoidAction (current >>= (\n -> append [Increment] >> return n)) 0 @?= (0 :: Int)
              ]
          , testGroup
              "execMonoidAction"
              [ testCase "pure doesn't change state" $ execMonoidAction (pure () :: M ()) 0 @?= 0
              , testCase "append changes state" $ execMonoidAction (append [Increment] :: M ()) 0 @?= 1
              ]
          ]
      ]
