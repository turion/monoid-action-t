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

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset
import Control.Monad.Trans.Reader (ReaderT (..), ask)

type M = Changeset Int [Count]

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
      "Changeset"
      [ testGroup
          "Changeset"
          [ testGroup
              "Order of change and current matters"
              [ testCase "change, current" $
                  evalChangeset (change [Increment] >> current) 0 @?= (1 :: Int)
              , testCase "current, change" $
                  let action = flip evalChangeset 0 $ do
                        n <- current
                        change [Increment]
                        return n
                   in action @?= (0 :: Int)
              ]
          , testGroup
              "execChangeset"
              [ testCase "pure doesn't change state" $ execChangeset (pure () :: M ()) 0 @?= 0
              , testCase "change changes state" $ execChangeset (change [Increment] :: M ()) 0 @?= 1
              ]
          ]
      , testGroup
          "MonadChangeset"
          [ testCase "ReaderT lifts changeset operations" $
              let action = flip execChangeset (0 :: Int) $ flip runReaderT (100 :: Int) $ do
                    env <- ask
                    change $ replicate env Increment
               in action @?= 100
          ]
      ]
