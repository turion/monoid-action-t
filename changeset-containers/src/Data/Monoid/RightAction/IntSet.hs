module Data.Monoid.RightAction.IntSet where

-- containers
import Data.IntSet

-- changeset
import Data.Monoid.RightAction (RightAction (..))

-- | Insert or delete an element in an 'IntSet'.
data IntSetChange
  = Insert Int
  | Delete Int
  deriving (Show, Read, Eq, Ord)

instance RightAction IntSetChange IntSet where
  actRight s (Insert k) = insert k s
  actRight s (Delete k) = delete k s
