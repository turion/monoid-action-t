module IntMap where

-- base
import Prelude hiding (Foldable (..))

-- containers
import Data.IntMap.Strict (singleton)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.IntMap

tests :: TestTree
tests =
  testGroup
    "IntMap"
    [ testCase "Can insert" $
        runChangeset (changeSingle (Insert 0 True :: IntMapChange Bool)) mempty @?= ((), singleton 0 True)
    , testCase "Can read after insert" $
        let action = do
              changeSingle (Insert 0 True :: IntMapChange Bool)
              m <- current
              changeSingle $ Insert 0 False
              return m
         in runChangeset action mempty @?= (singleton 0 True, singleton 0 False)
    , testCase "Can delete after insert" $
        let action = do
              changeSingle (Insert 0 True :: IntMapChange Bool)
              changeSingle $ Insert 1 False
              changeSingle $ Delete 0
         in execChangeset action mempty @?= singleton 1 False
    ]
