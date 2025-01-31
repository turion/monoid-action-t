{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Trans.Changeset.Examples where

-- base
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..), Last (..))
import Data.Tuple (swap)

-- monoid-extras
import Data.Monoid.Action

-- mtl
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer (MonadWriter (..))

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset

-- | 'ReaderT' is a special case of 'ChangesetT' when the changes are trivial.
type TrivialChangeReaderT r = ChangesetT r ()

instance {-# OVERLAPPING #-} (Monad m) => MonadReader r (TrivialChangeReaderT r m) where
  ask = current
  local = withCurrent

-- | 'WriterT' is a special case of 'ChangesetT' when the current state is trivial.
type TrivialActionWriterT w = ChangesetT () w

instance Action w () where
  act _ _ = ()

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadWriter w (TrivialActionWriterT w m) where
  writer = ChangesetT . pure . pure . swap
  listen = ChangesetT . fmap (fmap (\(w, a) -> (w, (a, w)))) . getChangesetT
  pass = ChangesetT . fmap (fmap (\(w, (a, f)) -> (f w, a))) . getChangesetT

{- | 'StateT' is a special case of 'ChangesetT' when the changes are whole state values,
and only the last write matters.
-}
type LastWriteT s = ChangesetT s (Last s)

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (LastWriteT s m) where
  state f = ChangesetT $ \s -> return $ first pure $ swap $ f s

-- TODO PR to monoid-extras, once merged remove no-orphans
instance Action (Last s) s where
  act (Last m) s = fromMaybe s m

{- | Endomorphism state monad.

There is a further, not so much studied state monad by choosing any state type @s@ and the @Endo s@ monoid.
It behaves a lot like @'State' s@ with one subtle difference:
When combining two computations, the modifications are both applied,
but they will both read from the initial state.
So for example, @modify (+ 1) >> modify (+ 1) = modify (+ 2)@,
but if we define @inc = get >>= (\n -> put n + 1)@, we have @inc >> inc = inc@.
This is because all the calls to 'get' receive the same initial state, and 'put' unconditionally writes the state.
In other words, 'modify' (or 'state' for that matter) is more powerful than 'get' and 'put' combined.
-}
type EndoStateT s = ChangesetT s (Endo s)

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (EndoStateT s m) where
  state f = ChangesetT $ \s -> return (Endo $ snd <$> f, fst $ f s)
