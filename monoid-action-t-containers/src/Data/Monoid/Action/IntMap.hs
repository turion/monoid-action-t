module Data.Monoid.Action.IntMap where

import Data.IntMap
import Data.Monoid.Action (Action (..))

data IntMapChange a w
  = Insert Int a
  | Delete Int
  | Adjust Int w

instance (Action w a) => Action (IntMapChange a w) (IntMap a) where
  act (Insert k a) = insert k a
  act (Delete k) = delete k
  act (Adjust k w) = adjust (act w) k

type IntMapChange' a = IntMapChange a ()
