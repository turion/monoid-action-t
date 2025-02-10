module Data.Monoid.RightAction.Set where

-- containers
import Data.Set

-- changeset
import Data.Monoid.RightAction (RightAction (..))

-- | Insert or delete an element in a 'Set'.
data SetChange k
  = Insert k
  | Delete k
  deriving (Show, Read, Eq, Ord)

instance (Ord k) => RightAction (SetChange k) (Set k) where
  actRight s (Insert k) = insert k s
  actRight s (Delete k) = delete k s
