{-# LANGUAGE UndecidableInstances #-}

-- | A type class generalising the API of 'Control.Monad.Trans.Changeset.ChangesetT'.
module Control.Monad.Changeset.Class where

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- changeset
import Data.Monoid.RightAction (RightAction)

{- | Monads containing changeset state.

This usually implies that the 'Control.Monad.Trans.Changeset.ChangesetT' monad transformer is part of the monad transformer stack of @m@.
See its documentation for details.
-}
class (Monad m, Monoid w, RightAction w s) => MonadChangeset s w m | m -> s, m -> w where
  -- | Apply a changeset to the state.
  changeset ::
    -- | Receives the current state and has to output a value and a change.
    (s -> (a, w)) ->
    m a

  -- | Apply a change to the state.
  -- The 'Action' instance is used to mutate the state.
  change :: w -> m ()

  -- | Observe the current state.
  current :: m s

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MFunctor t, MonadChangeset s w m) => MonadChangeset s w (t m) where
  changeset = lift . changeset
  change = lift . change
  current = lift current
