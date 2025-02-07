module Data.Monoid.RightAction.Map where

import Data.Map
import Data.Monoid.RightAction (RightAction (..))

data MapChange k a
  = Insert k a
  | Delete k
  deriving (Show, Read, Eq, Ord, Functor)

instance (Ord k) => RightAction (MapChange k a) (Map k a) where
  actRight m (Insert k a) = insert k a m
  actRight m (Delete k) = delete k m
