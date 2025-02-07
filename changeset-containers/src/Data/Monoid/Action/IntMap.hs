module Data.Monoid.Action.IntMap where

import Data.IntMap
import Data.Monoid.Action (Action (..))

data IntMapChange a
  = Insert Int a
  | Delete Int
  deriving (Show, Read, Eq, Ord, Functor)

instance Action (IntMapChange a) (IntMap a) where
  act (Insert k a) = insert k a
  act (Delete k) = delete k
