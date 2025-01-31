module Data.Monoid.Action.IntSet where

import Data.IntSet
import Data.Monoid.Action (Action (..))

data IntSetChange
  = Insert Int
  | Delete Int

instance Action IntSetChange IntSet where
  act (Insert k) = insert k
  act (Delete k) = delete k
