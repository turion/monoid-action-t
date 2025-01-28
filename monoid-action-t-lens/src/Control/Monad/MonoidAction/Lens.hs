{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MonoidAction.Lens where

-- base
import Data.Function ((&))
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- transformers
import Control.Monad.Trans.Class (MonadTrans (lift))

-- lens
import Control.Lens (Getting, Index, IxValue, Ixed (..), Lens', Setter', Traversal', view, (%~))

-- containers
import Data.Map.Strict (Map)
import Data.Sequence (Seq)

-- monoidal-containers
import Data.Map.Monoidal (MonoidalMap, foldlWithKey', singleton)

-- monoid-extras
import Data.Monoid.Action

-- monoid-action-t
import Control.Monad.MonoidAction.Class (MonadMonoidAction)
import Control.Monad.Trans.MonoidAction

-- We assume that all indices edit different parts, so we can merge them in a map
newtype IxedUpdates s w = IxedUpdates
  {getIxedUpdates :: MonoidalMap (Index s) w}

deriving instance (Ord (Index s), Semigroup w) => Semigroup (IxedUpdates s w)
deriving instance (Ord (Index s), Monoid w) => Monoid (IxedUpdates s w)
deriving instance (Eq (Index s), Eq w) => Eq (IxedUpdates s w)
deriving instance (Ord (Index s), Ord w) => Ord (IxedUpdates s w)
deriving instance (Show (Index s), Show w) => Show (IxedUpdates s w)
deriving instance (Ord (Index s), Read (Index s), Read w) => Read (IxedUpdates s w)

instance (Action w (IxValue s), Ixed s) => Action (IxedUpdates s w) s where
  act IxedUpdates {getIxedUpdates} s = foldlWithKey' (\s' i w -> s' & ix i %~ act w) s getIxedUpdates

ixedUpdate :: Index s -> w -> IxedUpdates s w
ixedUpdate i = IxedUpdates . singleton i

type MapUpdates k a = IxedUpdates (Map k a) a

data SetterUpdate s a w = SetterUpdate
  { setterUpdateSetter :: Setter' s a
  , setterUpdateUpdate :: w
  }

newtype SetterUpdates s a w = SetterUpdates
  {getSetterUpdates :: Seq (SetterUpdate s a w)}
  deriving newtype (Semigroup, Monoid)

instance (Action w a) => Action (SetterUpdates s a w) s where
  act SetterUpdates {getSetterUpdates} s = foldl' (\s' SetterUpdate {setterUpdateSetter, setterUpdateUpdate} -> s' & setterUpdateSetter %~ act setterUpdateUpdate) s getSetterUpdates

setterUpdate :: Setter' s a -> w -> SetterUpdates s a w
setterUpdate setterUpdateSetter setterUpdateUpdate = SetterUpdates $ pure $ SetterUpdate {setterUpdateSetter, setterUpdateUpdate}

lensUpdate :: Lens' s a -> w -> SetterUpdates s a w
lensUpdate = setterUpdate

traversalUpdate :: Traversal' s a -> w -> SetterUpdates s a w
traversalUpdate = setterUpdate

instance (Action w a, Action w s, Monoid w, Monad m) => Focus s a w (MonoidActionT s w m) (MonoidActionT a w m) where
  focus g MonoidActionT {getMonoidActionT} = MonoidActionT $ \s -> getMonoidActionT $ view g s

class (MonadMonoidAction s w m, MonadMonoidAction a w n) => Focus s a w m n where
  focus :: Getting a s a -> n x -> m x

instance (MonadTrans t, Focus s a w m n, MonadMonoidAction s w (t m)) => Focus s a w (t m) n where
  focus getting = lift . focus getting
