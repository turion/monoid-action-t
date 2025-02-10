module Setter where

-- base
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Lens', lens)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- containers
import qualified Data.IntMap as IM

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-lens
import Control.Monad.Changeset.Lens
import Control.Monad.Changeset.Lens.Ixed ((|>!))
import Control.Monad.Changeset.Lens.Setter (setterChangeset)

data Big = Big
  { irrelevant :: String
  , counter :: Int
  }
  deriving (Eq, Show)

lensCounter :: Lens' Big Int
lensCounter = lens counter $ \big counter -> big {counter}

tests :: TestTree
tests =
  testGroup
    "Setter"
    [ testGroup
        "Small change on big data structure"
        [ testCase "single change" $ execChangeset (change (lensChangeset lensCounter (singleChange Increment))) (Big "foo" 0) @?= Big "foo" 1
        ]
    , testGroup
        "nested IxedChangeset with SetterChangeset"
        [ testCase "Can apply setter changeset inside indexed structure" $
            let initialState = IM.fromList [(0, Big "" 0), (1, Big "" 0)]
                action = do
                  2 |>! setterChangeset lensCounter (singleChange Increment)
                  s <- current
                  1 |>! setterChangeset lensCounter (singleChange Increment)
                  2 |>! setterChangeset lensCounter (singleChange Increment)
                  return s
             in runChangeset action initialState
                  @?= ( initialState
                      , IM.fromList
                          [ (0, Big "" 0)
                          , (1, Big "" 1)
                          ]
                      )
        ]
    ]
