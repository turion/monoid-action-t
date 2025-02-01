module Data.Monoid.Action.Map where

import Data.Map
import Data.Monoid.Action (Action (..))

data MapChange k a
  = Insert k a
  | Delete k

instance (Ord k) => Action (MapChange k a) (Map k a) where
  act (Insert k a) = insert k a
  act (Delete k) = delete k
