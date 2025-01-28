module Control.Monad.MonoidAction where

-- transformers
import Control.Monad.Trans.Class

-- mtl

-- monoid-extras
import Data.Monoid.Action (Action, act)

-- | Hold a state of type @s@, which is allowed to be mutated by an action of a monoid @w@.
newtype MonoidActionT s w m a = MonoidActionT
  { runMonoidActionT :: s -> m (w, a)
  }
  deriving (Functor)

instance (Monoid w, Applicative m) => Applicative (MonoidActionT s w m) where
  pure a = MonoidActionT $ const $ pure (mempty, a)
  MonoidActionT ma <*> MonoidActionT mf = MonoidActionT $ \s -> mappendAndApply <$> ma s <*> mf s
    where
      mappendAndApply :: (w, a -> b) -> (w, a) -> (w, b)
      mappendAndApply (w1, f) (w2, a) = (w1 <> w2, f a)

instance (Action w s, Monoid w, Monad m) => Monad (MonoidActionT s w m) where
  MonoidActionT ma >>= f = MonoidActionT $ \s -> do
    (w, a) <- ma s
    let !s' = act w s
    runMonoidActionT (f a) s'

instance (Action w s, Monoid w) => MonadTrans (MonoidActionT s w) where
  lift = MonoidActionT . const . fmap (mempty,)

-- FIXME lift mtl classes
-- FIXME fused-effects
-- FIXME time example
-- FIXME CRDT/DB example
