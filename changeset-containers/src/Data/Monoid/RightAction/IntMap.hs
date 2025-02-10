module Data.Monoid.RightAction.IntMap where

-- containers
import Data.IntMap

-- changeset
import Data.Monoid.RightAction (RightAction (..))

{- | Insert or delete an element in an 'IntMap'.

To change an element in an 'IntMap', see the indexed changes in [@changeset-lens@](hackage.haskell.org/package/changeset-lens).
-}
data IntMapChange a
  = Insert Int a
  | Delete Int
  deriving (Show, Read, Eq, Ord, Functor)

instance RightAction (IntMapChange a) (IntMap a) where
  actRight m (Insert k a) = insert k a m
  actRight m (Delete k) = delete k m
