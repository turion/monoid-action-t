module Main (main) where

-- tasty
import Test.Tasty

-- changeset-examples
import Control.Monad.Trans.Changeset.AccumExample as AccumExample
import Control.Monad.Trans.Changeset.Examples as Examples

-- type M = Changeset Int (Changes Count)

main :: IO ()
main =
  defaultMain $
    testGroup
      "examples"
      [ Examples.tests
      , AccumExample.tests
      ]
