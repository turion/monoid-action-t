{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Trans.MonoidAction.Orphan where

-- transformers
import Control.Monad.Trans.Class (lift)

-- monoid-extras
import Data.Monoid.Action (Action, Regular (getRegular))

-- monoid-action-t
import Control.Monad.Trans.MonoidAction

-- mtl
import Control.Monad.Accum (MonadAccum (..))
import Control.Monad.MonoidAction.Class (MonadMonoidAction (acting))
import Control.Monad.Select (MonadSelect (..))

instance (MonadAccum w m, Action w' s, Monoid w') => MonadAccum w (MonoidActionT s w' m) where
  accum = lift . accum

instance (MonadSelect r m, Action w s, Monoid w) => MonadSelect r (MonoidActionT s w m) where
  select = lift . select

type RegularAccumT w = MonoidActionT (Regular w) w

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadAccum w (RegularAccumT w m) where
  accum = acting . (. getRegular)
