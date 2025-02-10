module Set where

-- base
import Prelude hiding (Foldable (..))

-- containers
import Data.Set (fromList, singleton)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.Set

tests :: TestTree
tests =
  testGroup
    "Set"
    [ testCase "Can insert" $
        runChangeset (changeSingle (Insert True)) mempty @?= ((), singleton True)
    , testCase "Can read after insert" $
        let action = do
              changeSingle $ Insert True
              m <- current
              changeSingle $ Insert False
              return m
         in runChangeset action mempty @?= (singleton True, fromList [True, False])
    , testCase "Can delete after insert" $
        let action = do
              changeSingle $ Insert True
              changeSingle $ Insert False
              changeSingle $ Delete True
         in execChangeset action mempty @?= singleton False
    ]
