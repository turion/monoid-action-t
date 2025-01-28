-- FIXME remove if we can upstream the [w] instance
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- base
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Lens', lens)

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

-- monoid-action-t-lens
import Control.Monad.MonoidAction.Lens

data Count = Increment

data Big = Big
  { irrelevant :: String
  , counter :: Int
  }
  deriving (Eq, Show)

lensCounter :: Lens' Big Int
lensCounter = lens counter $ \big counter -> big {counter}

instance Action Count Int where
  act Increment counter = counter + 1

-- act counts n = foldl' (\n' Increment -> n' + 1) n counts

instance (Action w s) => Action [w] s where
  act ws s = foldl' (flip act) s ws

main :: IO ()
main =
  defaultMain $
    testGroup
      ""
      [ testGroup
          ""
          [ testCase "" $ execMonoidAction (append (lensUpdate lensCounter [Increment])) (Big "foo" 0) @?= Big "foo" 1
          ]
      ]
