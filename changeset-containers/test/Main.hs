module Main (main) where

-- base
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- changeset-containers-test
import qualified IntMap
import qualified IntSet
import qualified Map
import qualified Sequence
import qualified Set

main :: IO ()
main =
  defaultMain $
    testGroup
      "changeset-containers"
      [ IntMap.tests
      , Map.tests
      , IntSet.tests
      , Set.tests
      , Sequence.tests
      ]
