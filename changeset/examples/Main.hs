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

-- changeset-examples

import Control.Monad.Trans.Changeset.AccumExample as AccumExample
import Control.Monad.Trans.Changeset.Examples as Examples

-- type M = Changeset Int (Changes Count)

main :: IO ()
main =
  defaultMain $
    testGroup
      "examples"
      [ Examples.tests
      , AccumExample.tests
      ]
