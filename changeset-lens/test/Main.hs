module Main (main) where

-- base
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Lens', lens)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- changeset
import Control.Monad.Changeset.Class

import Control.Monad.Trans.Changeset

-- changeset-lens
import Control.Monad.Changeset.Lens

-- changeset-lens-test
import qualified Ixed
import qualified Setter
import qualified At

data Big = Big
  { irrelevant :: String
  , counter :: Int
  }
  deriving (Eq, Show)

lensCounter :: Lens' Big Int
lensCounter = lens counter $ \big counter -> big {counter}

main :: IO ()
main =
  defaultMain $
    testGroup
      "changeset-lens"
      [ testGroup
          "Small change on big data structure"
          [ testCase "single change" $ execChangeset (change (lensChangeset lensCounter (singleChange Increment))) (Big "foo" 0) @?= Big "foo" 1
          ]
      , Ixed.tests
      , Setter.tests
      , At.tests
      ]
