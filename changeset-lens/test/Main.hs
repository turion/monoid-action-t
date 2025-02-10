module Main (main) where

-- base
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- changeset-lens-test
import qualified At
import qualified Ixed
import qualified Setter

main :: IO ()
main =
  defaultMain $
    testGroup
      "changeset-lens"
      [ Ixed.tests
      , Setter.tests
      , At.tests
      ]
