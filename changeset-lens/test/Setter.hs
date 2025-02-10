module Setter where

-- base

import Data.Char (toLower, toUpper)
import Data.Monoid (Dual (..), Endo (..), Last (..))
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Lens', lens)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- containers
import Data.IntMap (singleton)
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- changeset
import Control.Monad.Changeset.Class
import Data.Monoid.RightAction (RightAction (..))
import Data.Monoid.RightAction.Coproduct (inL, inR, normaliseCoproduct, (:+:))

import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.IntMap

-- changeset-lens
import Control.Monad.Changeset.Lens

tests :: TestTree
tests =
  testGroup
    "Setter"
    [ testGroup "nested IxedChangeset with SetterChangeset" []
    -- FIXME something with IntMapChange (SetterChangeset Big Count)
    ]
