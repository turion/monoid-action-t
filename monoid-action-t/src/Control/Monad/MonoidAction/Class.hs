module Control.Monad.MonoidAction.Class where

import Data.Monoid.Action

-- FIXME document laws, like MonadAccum (see mmorph)
class (Monad m, Monoid w, Action w s) => MonadMonoidAction s w m | m -> s, m -> w where
  acting :: (s -> (a, w)) -> m a
  append :: w -> m ()
  current :: m s
