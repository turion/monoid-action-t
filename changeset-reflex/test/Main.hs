-- FIXME remove if we can upstream the [w] instance
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- base
import Data.Foldable (foldl')
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- monoid-extras
import Data.Monoid.Action (Action (..))

-- changeset

-- changeset-reflex

-- instance (Foldable f, Action w s) => Action (f w) s where
--   act ws s = foldl' (flip act) s ws
instance (Action w s) => Action [w] s where
  act ws s = foldl' (flip act) s ws

main :: IO ()
main =
  defaultMain $
    testGroup
      "Reflex"
      []

-- FIXME This seems like a lot of effort
