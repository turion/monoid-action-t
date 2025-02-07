module Data.Monoid.RightAction.IntSet where

import Data.IntSet
import Data.Monoid.RightAction (RightAction (..))

data IntSetChange
  = Insert Int
  | Delete Int
  deriving (Show, Read, Eq, Ord)

instance RightAction IntSetChange IntSet where
  actRight s (Insert k) = insert k s
  actRight s (Delete k) = delete k s
