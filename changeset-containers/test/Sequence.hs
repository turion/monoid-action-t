module Sequence where

-- base
import Prelude hiding (Foldable (..))

-- containers
import Data.Sequence (fromList, singleton)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset (changeSingle, execChangeset, runChangeset)

-- changeset-containers
import Data.Monoid.RightAction.Sequence

tests :: TestTree
tests =
  testGroup
    "Map"
    [ testCase "Can insert" $
        runChangeset (changeSingle (Cons 0 :: SeqChange Int)) mempty @?= ((), singleton (0 :: Int))
    , testCase "Can read after insert" $
        let action = do
              changeSingle (Cons 1 :: SeqChange Int)
              m <- current
              changeSingle $ Cons 0
              return m
         in runChangeset action mempty @?= (singleton (1 :: Int), fromList [0, 1])
    , testCase "Can delete after insert" $
        let action = do
              changeSingle (Cons 1 :: SeqChange Int)
              m <- current
              changeSingle $ Cons 0
              changeSingle $ Snoc 3
              changeSingle $ Snoc 99
              changeSingle Unsnoc
              return m
         in execChangeset action (singleton (2 :: Int)) @?= fromList [0, 1, 2, 3]
    ]
