module Data.Monoid.RightAction.IntMap where

import Data.IntMap
import Data.Monoid.RightAction (RightAction (..))

data IntMapChange a
  = Insert Int a
  | Delete Int
  deriving (Show, Read, Eq, Ord, Functor)

instance RightAction (IntMapChange a) (IntMap a) where
  actRight m (Insert k a) = insert k a m
  actRight m (Delete k) = delete k m
