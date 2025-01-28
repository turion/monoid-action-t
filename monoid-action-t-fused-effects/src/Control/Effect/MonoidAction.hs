{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Effect.MonoidAction where

-- base
import Data.Kind (Type)

-- monoid-extras
import Data.Monoid.Action

-- fused-effects
import Control.Algebra

-- monoid-action-t

import Control.Monad.MonoidAction.Class (MonadMonoidAction (..))
import Control.Monad.Trans.MonoidAction (MonoidActionT (..))
import Data.Bifunctor (first)

data MonoidAction s w (m :: Type -> Type) k where
  Append :: w -> MonoidAction s w m ()
  Current :: MonoidAction s w m s

instance (Action w s, Monoid w, Algebra sig m) => Algebra (MonoidAction s w :+: sig) (MonoidActionT s w m) where
  alg handler sig ctx = case sig of
    L (Append w) -> ctx <$ append w
    L Current -> (<$ ctx) <$> current
    R other -> MonoidActionT $ \s ->
      thread ((\(w, x) -> first (mappend w) <$> getMonoidActionT x s) ~<~ handler) other (mempty, ctx)
