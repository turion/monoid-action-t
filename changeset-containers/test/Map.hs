module Map where

-- base
import Prelude hiding (Foldable (..))

-- containers
import Data.Map.Strict (singleton)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.Map

tests :: TestTree
tests =
  testGroup
    "Map"
    [ testCase "Can insert" $
        runChangeset (changeSingle (Insert "0" True :: MapChange String Bool)) mempty @?= ((), singleton "0" True)
    , testCase "Can read after insert" $
        let action = do
              changeSingle (Insert "0" True :: MapChange String Bool)
              m <- current
              changeSingle $ Insert "0" False
              return m
         in runChangeset action mempty @?= (singleton "0" True, singleton "0" False)
    , testCase "Can delete after insert" $
        let action = do
              changeSingle (Insert "0" True :: MapChange String Bool)
              changeSingle $ Insert "1" False
              changeSingle $ Delete "0"
         in execChangeset action mempty @?= singleton "1" False
    ]
