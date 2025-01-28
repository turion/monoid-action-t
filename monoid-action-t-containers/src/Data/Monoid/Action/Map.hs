module Data.Monoid.Action.Map where

import Data.Map
import Data.Monoid.Action (Action (..))

data MapChange k a w
  = Insert k a
  | Delete k
  | Adjust k w

instance (Action w a, Ord k) => Action (MapChange k a w) (Map k a) where
  act (Insert k a) = insert k a
  act (Delete k) = delete k
  act (Adjust k w) = adjust (act w) k

type MapChange' k a = MapChange k a ()
