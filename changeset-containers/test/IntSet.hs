module IntSet where

-- base
import Prelude hiding (Foldable (..))

-- containers
import Data.IntSet (fromList, singleton)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.IntSet

tests :: TestTree
tests =
  testGroup
    "IntSet"
    [ testCase "Can insert" $
        runChangeset (changeSingle (Insert 0)) mempty @?= ((), singleton 0)
    , testCase "Can read after insert" $
        let action = do
              changeSingle $ Insert 0
              m <- current
              changeSingle $ Insert 1
              return m
         in runChangeset action mempty @?= (singleton 0, fromList [0, 1])
    , testCase "Can delete after insert" $
        let action = do
              changeSingle $ Insert 0
              changeSingle $ Insert 1
              changeSingle $ Delete 0
         in execChangeset action mempty @?= singleton 1
    ]
