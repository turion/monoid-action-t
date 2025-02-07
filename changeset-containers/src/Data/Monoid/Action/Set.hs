module Data.Monoid.Action.Set where

import Data.Monoid.Action (Action (..))
import Data.Set

data SetChange k
  = Insert k
  | Delete k
  deriving (Show, Read, Eq, Ord)

instance (Ord k) => Action (SetChange k) (Set k) where
  act (Insert k) = insert k
  act (Delete k) = delete k
