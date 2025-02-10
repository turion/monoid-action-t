{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Effect.Changeset where

-- base
import Data.Bifunctor (first)
import Data.Kind (Type)

-- fused-effects
import Control.Algebra

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (ChangesetT (..))
import Data.Monoid.RightAction (RightAction)

data Changeset s w (m :: Type -> Type) k where
  Change :: w -> Changeset s w m ()
  Current :: Changeset s w m s

instance (RightAction w s, Monoid w, Algebra sig m) => Algebra (Changeset s w :+: sig) (ChangesetT s w m) where
  alg handler sig ctx = case sig of
    L (Change w) -> ctx <$ change w
    L Current -> (<$ ctx) <$> current
    R other -> ChangesetT $ \s ->
      thread ((\(w, x) -> first (mappend w) <$> getChangesetT x s) ~<~ handler) other (mempty, ctx)

sendChange :: forall s w sig m . Has (Changeset s w) sig m => w -> m ()
sendChange = send . Change

sendCurrent :: Has (Changeset s w) sig m => m w
sendCurrent = send Current
