module Main (main) where

-- base
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit
-- import Test.Tasty.HUnit (testCase, (@?=))

-- falsify
-- import Test.Tasty.Falsify

-- monoid-extras

-- changeset
-- import Control.Monad.Trans.Changeset
import Data.Monoid.RightAction

-- type M = Changeset Int (Changes Count)

data Count = Increment

instance RightAction Count Int where
  actRight count Increment = count + 1

main :: IO ()
main =
  defaultMain $
    testGroup
      "examples"
      []

data ListChange a = Cons a | Pop

instance RightAction (ListChange a) [a] where
  actRight as (Cons a) = a : as
  actRight as Pop = drop 1 as
