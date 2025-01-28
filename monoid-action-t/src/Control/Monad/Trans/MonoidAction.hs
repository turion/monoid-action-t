{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{- | A general state monad transformer with separate types for the state and the possible updates.

'MonoidActionT' is the most general state monad transformer.
It has all the standard state monads from @transformers@ as special cases:
+--------------------------+-------------+-------------+
| Transformer special case | State type  | Monoid type |
+==========================+=============+=============+
| @'WriterT' w@            | '()'        | @w@         |
+--------------------------+-------------+-------------+
| @'AccumT' w@             | 'Regular w' | @w@         |
+--------------------------+-------------+-------------+
| @'StateT' s@             | @s@         | @Last s@    |
+--------------------------+-------------+-------------+

Orphan instances for newer (2.3) @mtl@ classes such as 'MonadAccum' and 'MonadSelect' can be found in "Control.Monad.Trans.MonoidAction.Orphan".
These are only provided for GHC >= 9.6.
-}
module Control.Monad.Trans.MonoidAction where

-- base
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..), First, Last (..))
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Morph (MFunctor (..), MMonad (..))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Writer.Class (MonadWriter (..))

-- monoid-extras
import Data.Monoid.Action (Action, act)

-- monoid-action-t

import Control.Monad (MonadPlus)
import Control.Monad.MonoidAction.Class

{- Naming
\* AppendT
  * Name clash with tuple-sop
  * append clashes with NonEmpty
\* CommitT
  * Potential misunderstanding with version control?
  * commit clashes with many db libraries
\* UpdateT
  * update clashes with containers
\* ChangeT
  * clashes with Agda (probably irrelevant)
  * change clashes with jsaddle-dom
  * Change clashes with many libraries (shake, ..)
\* ChangesetT
  * No name clashes
  * potential misunderstanding with Set from containers?
-}

{- | Hold a state of type @s@, which is allowed to be mutated by an action of a monoid @w@.

The state @s@ has the role of the "current state".
An @a@ is computed while performing a side effect in @m@,
and these can depend on the current state.

The type @w@ encodes "updates", "edits" or "diffs" to the state @s@.
This relation is captured by the 'Action' type class from @monoid-extras@.
It contains a method, @'act' :: w -> s -> s@,
which implements the semantics of @w@ as the type of updates to @s@.

For example, if @s@ is 'UTCTime', then @w@ can be 'NominalDiffTime',
which would act via @addUTCTime@:
Two timestamps cannot be added, but two time differences can.
(The instance 'Action NominalDiffTime UTCTime' unfortunately does not exist, but it could be implemented as an orphan instance.)
A computation in @'MonoidActionT' UTCTime NominalDiffTime m a@ could then have access to some notion of "current time",
while adding symbolic "delays".

Another class of examples arises operation based or commutative Conflict-free Replicated Data Type (CRDT).
Then @s@ is the internal state (the "payload") of the CRDT, and @w@ is the update operation.
For example @s = Int@, and for @w@ we would define @data Count = Increment | Decrement@.

The 'Monad' and 'Applicative' classes are defined by performing the first action,
then 'act'ing with the monoid output onto the state, and then perform the second action with the updated state.
So for example, @'append' Increment >> 'current'@ is different from @'current' >>= (\n -> 'append' Increment >> return n)@:
If we apply @'flip' 'evalMonoidAction' 0@ to each,
the first one would return 1, while the second returns 0.
-}
newtype MonoidActionT s w m a = MonoidActionT
  { getMonoidActionT :: s -> m (w, a)
  }
  deriving (Functor)

-- | Run the action with an initial state and apply all resulting changes to it.
runMonoidActionT :: (Functor m, Action w s) => MonoidActionT s w m a -> s -> m (a, s)
runMonoidActionT MonoidActionT {getMonoidActionT} s = getMonoidActionT s <&> \(w, a) -> (a, act w s)

-- | Run the action with an initial state and extract only the value.
evalMonoidActionT :: (Functor m, Action w s) => MonoidActionT s w m a -> s -> m a
evalMonoidActionT = fmap (fmap fst) . runMonoidActionT

-- | Run the action with an initial state and extract only the state.
execMonoidActionT :: (Functor m, Action w s) => MonoidActionT s w m a -> s -> m s
execMonoidActionT = fmap (fmap snd) . runMonoidActionT

-- FIXME naming "update"?

-- | Append an edit
appendA :: (Applicative m) => w -> MonoidActionT s w m ()
appendA w = MonoidActionT $ const $ pure (w, ())

currentA :: (Applicative m, Monoid w) => MonoidActionT s w m s
currentA = MonoidActionT $ \s -> pure (mempty, s)

actingA :: (Applicative m) => (s -> (a, w)) -> MonoidActionT s w m a
actingA = MonoidActionT . fmap (pure . swap)

withCurrent :: (s2 -> s1) -> MonoidActionT s1 w m a -> MonoidActionT s2 w m a
withCurrent f = MonoidActionT . (. f) . getMonoidActionT

mapUpdate :: (Functor m) => (w1 -> w2) -> MonoidActionT s w1 m a -> MonoidActionT s w2 m a
mapUpdate f = MonoidActionT . fmap (fmap (first f)) . getMonoidActionT

instance (Action w s, Monoid w, Monad m) => MonadMonoidAction s w (MonoidActionT s w m) where
  append = appendA
  current = currentA
  acting = actingA

-- | The 'Monad m' constraint is indeed necessary, since we need the log from the first action to append it to the state for the second action.
instance (Monoid w, Action w s, Monad m) => Applicative (MonoidActionT s w m) where
  pure a = MonoidActionT $ const $ pure (mempty, a)

  MonoidActionT mf <*> MonoidActionT ma = MonoidActionT $ \s -> do
    (w1, f) <- mf s
    let !s' = act w1 s
    (w2, a) <- ma s'
    pure (w1 <> w2, f a)

instance (Action w s, Monoid w, Monad m) => Monad (MonoidActionT s w m) where
  MonoidActionT ma >>= f = MonoidActionT $ \s -> do
    (w1, a) <- ma s
    let !s' = act w1 s
    (w2, b) <- getMonoidActionT (f a) s'
    return (w1 <> w2, b)

instance (Action w s, Monoid w) => MonadTrans (MonoidActionT s w) where
  lift = liftF

liftF :: (Functor m, Monoid w) => m a -> MonoidActionT s w m a
liftF = MonoidActionT . const . fmap (mempty,)

instance (Alternative m, Monoid w, Action w s, Monad m) => Alternative (MonoidActionT s w m) where
  empty = liftF empty
  MonoidActionT ma1 <|> MonoidActionT ma2 = MonoidActionT $ \s -> ma1 s <|> ma2 s

instance (Alternative m, Monoid w, Action w s, Monad m) => MonadPlus (MonoidActionT s w m)

instance MFunctor (MonoidActionT s w) where
  hoist = hoistF

hoistF :: (forall x. m x -> n x) -> MonoidActionT s w m a -> MonoidActionT s w n a
hoistF morph ma = MonoidActionT $ morph . getMonoidActionT ma

instance (Action w s, Monoid w) => MMonad (MonoidActionT s w) where
  embed f (MonoidActionT g) = MonoidActionT $ \s ->
    s
      & g
      & f
      & flip getMonoidActionT s
      <&> \(w1, (w2, b)) -> (w1 <> w2, b)

instance (MonadError e m, Action w s, Monoid w) => MonadError e (MonoidActionT s w m) where
  throwError = lift . throwError
  catchError ma handler = MonoidActionT $ \s -> getMonoidActionT ma s `catchError` (\e -> getMonoidActionT (handler e) s)

instance (MonadReader r m, Action w s, Monoid w) => MonadReader r (MonoidActionT s w m) where
  ask = lift ask
  local f = hoist $ local f

instance (MonadRWS r w s m, Action w' s', Monoid w') => MonadRWS r w s (MonoidActionT s' w' m)

instance (MonadState s m, Action w' s', Monoid w') => MonadState s (MonoidActionT s' w' m) where
  state = lift . state

instance (MonadWriter w m, Action w' s, Monoid w') => MonadWriter w (MonoidActionT s w' m) where
  writer = lift . writer
  listen = MonoidActionT . fmap (fmap (\((w', a), w) -> (w', (a, w))) . listen) . getMonoidActionT
  pass = MonoidActionT . fmap (pass . fmap (\(w', (a, f)) -> ((w', a), f))) . getMonoidActionT

type MonoidAction s w = MonoidActionT s w Identity

runMonoidAction :: (Action w s) => MonoidAction s w a -> s -> (a, s)
runMonoidAction swa s = runIdentity $ runMonoidActionT swa s

evalMonoidAction :: (Action w s) => MonoidAction s w a -> s -> a
evalMonoidAction swa s = runIdentity $ evalMonoidActionT swa s

execMonoidAction :: (Action w s) => MonoidAction s w a -> s -> s
execMonoidAction swa s = runIdentity $ execMonoidActionT swa s

-- * Special cases

type TrivialActionWriterT w = MonoidActionT () w

instance Action w () where
  act _ _ = ()

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadWriter w (TrivialActionWriterT w m) where
  writer = MonoidActionT . pure . pure . swap
  listen = MonoidActionT . fmap (fmap (\(w, a) -> (w, (a, w)))) . getMonoidActionT
  pass = MonoidActionT . fmap (fmap (\(w, (a, f)) -> (f w, a))) . getMonoidActionT

type LastWriteT s = MonoidActionT s (First s)

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (LastWriteT s m) where
  state f = MonoidActionT $ \s -> return $ first pure $ swap $ f s

-- TODO PR to monoid-extras, once merged remove no-orphans
instance Action (Last s) s where
  act (Last m) s = fromMaybe s m

{- |

There is a further, not so much studied state monad by choosing any state type @s@ and the @Endo s@ monoid.
It behaves a lot like @'State' s@ with one subtle difference:
When combining two computations, the modifications are both applied,
but they will both read from the initial state.
So for example, @modify (+ 1) >> modify (+ 1) = modify (+ 2)@,
but if we define @inc = get >>= (\n -> put n + 1)@, we have @inc >> inc = inc@.
This is because all the calls to 'get' receive the same initial state, and 'put' unconditionally writes the state.
In other words, 'modify' (or 'state' for that matter) is more powerful than 'get' and 'put' combined.
-}
type EndoStateT s = MonoidActionT s (Endo s)

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (EndoStateT s m) where
  state f = MonoidActionT $ \s -> return (Endo $ snd <$> f, fst $ f s)

-- FIXME time example, separate package?
-- FIXME CRDT/DB example, separate package?
