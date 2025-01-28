{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Orphan instances for 'MonadAccum' and 'MonadSelect'.

Unfortunately @mtl-2.3@ is not very compatible with GHC < 9.6.
Therefore these instances are only defined for GHC >= 9.6.
-}
module Control.Monad.Trans.Changeset.Orphan where

-- transformers
import Control.Monad.Trans.Class (lift)

-- changeset
import Control.Monad.Trans.Changeset

-- mtl
import Control.Monad.Accum (MonadAccum (..))
import Control.Monad.Select (MonadSelect (..))
import Data.Monoid.RightAction

instance (MonadAccum w m, RightAction w' s, Monoid w') => MonadAccum w (ChangesetT s w' m) where
  accum = lift . accum

instance (MonadSelect r m, RightAction w s, Monoid w) => MonadSelect r (ChangesetT s w m) where
  select = lift . select
