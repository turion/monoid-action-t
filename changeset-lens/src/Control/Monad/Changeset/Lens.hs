{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore lensChangeset "Eta reduce" #-}
{-# HLINT ignore traversalChangeset "Eta reduce" #-}

module Control.Monad.Changeset.Lens where

-- base
import Data.Function ((&))
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- transformers
import Control.Monad.Trans.Class (MonadTrans)

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- lens
import Control.Lens (Getting, Index, IxValue, Ixed (..), Lens', Setter', Traversal', view, (%~), Prism', review)

-- containers
import Data.Map.Strict (Map)
import Data.Sequence (Seq)

-- monoidal-containers
import Data.Map.Monoidal (MonoidalMap, foldlWithKey', singleton)

-- monoid-extras
import Data.Monoid.Action

-- changeset
import Control.Monad.Changeset.Class (
  MonadChangeset (..),
 )
import Control.Monad.Trans.Changeset

-- We assume that all indices edit different parts, so we can merge them in a map
newtype IxedChangeset s w = IxedChangeset
  {getIxedChangeset :: MonoidalMap (Index s) w}

deriving instance (Ord (Index s), Semigroup w) => Semigroup (IxedChangeset s w)
deriving instance (Ord (Index s), Monoid w) => Monoid (IxedChangeset s w)
deriving instance (Eq (Index s), Eq w) => Eq (IxedChangeset s w)
deriving instance (Ord (Index s), Ord w) => Ord (IxedChangeset s w)
deriving instance (Show (Index s), Show w) => Show (IxedChangeset s w)
deriving instance (Ord (Index s), Read (Index s), Read w) => Read (IxedChangeset s w)

instance (Action w (IxValue s), Ixed s) => Action (IxedChangeset s w) s where
  act IxedChangeset {getIxedChangeset} s = foldlWithKey' (\s' i w -> s' & ix i %~ act w) s getIxedChangeset

ixedChangeset :: Index s -> w -> IxedChangeset s w
ixedChangeset i = IxedChangeset . singleton i

type MapChangeset k a = IxedChangeset (Map k a) a

data SetterChange s a w = SetterChange
  { setterChangeSetter :: Setter' s a
  , setterChangeChange :: w
  }

newtype SetterChangeset s a w = SetterChangeset
  {getSetterChangeset :: Seq (SetterChange s a w)}
  deriving newtype (Semigroup, Monoid)

instance (Action w a) => Action (SetterChangeset s a w) s where
  act SetterChangeset {getSetterChangeset} s = foldl' (\s' SetterChange {setterChangeSetter, setterChangeChange} -> s' & setterChangeSetter %~ act setterChangeChange) s getSetterChangeset

setterChangeset :: Setter' s a -> w -> SetterChangeset s a w
setterChangeset setterChangeSetter setterChangeChange = SetterChangeset $ pure $ SetterChange {setterChangeSetter, setterChangeChange}

lensChangeset :: Lens' s a -> w -> SetterChangeset s a w
lensChangeset l w = setterChangeset l w

traversalChangeset :: Traversal' s a -> w -> SetterChangeset s a w
traversalChangeset t w = setterChangeset t w

class (MonadChangeset a w m, MonadChangeset s w n) => Focus m n a s w where
  focus :: Getting a s a -> m x -> n x

instance (Action w a, Action w s, Monoid w, Monad m) => Focus (ChangesetT a w m) (ChangesetT s w m) a s w where
  focus g ChangesetT {getChangesetT} = ChangesetT $ \s -> getChangesetT $ view g s

instance (Monad m, MonadTrans t, MFunctor t, Focus m n s a w, Monad (t m)) => Focus (t m) (t n) s a w where
  focus getting = hoist $ focus getting

class (MonadChangeset s v m, MonadChangeset s w n) => Specify s m n v w where
  specify :: Prism' w v -> m x -> n x

instance (Monad m, Monoid w, Monoid v, Action w s, Action v s) => Specify s (ChangesetT s v m) (ChangesetT s w m) v w where
  specify prism = mapChange $ review prism

(<>|>) :: (MonadChangeset s (SetterChangeset s a w) m) => Setter' s a -> w -> m ()
setter <>|> w = change $ setterChangeset setter w

(<>@|>) :: (MonadChangeset s (IxedChangeset s w) m) => Index s -> w -> m ()
index <>@|> w = change $ ixedChangeset index w
