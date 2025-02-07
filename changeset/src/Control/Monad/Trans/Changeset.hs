{-# LANGUAGE UndecidableInstances #-}

{- | A general state monad transformer with separate types for the state and the possible changes, updates, commits, or diffs.

'ChangesetT' is a very general state monad transformer.
It has all the standard state monads from @transformers@ as special cases:

+--------------------------+-------------+-------------+---------------------------------------------+
| Transformer special case | State type  | Monoid type | Intuition                                   |
+==========================+=============+=============+=============================================+
| @'WriterT' w@            | '()'        | @w@         | No possibility to observe the current state |
+--------------------------+-------------+-------------+---------------------------------------------+
| @'AccumT' w@             | 'Regular w' | @w@         | The state is the same type as the changes   |
+--------------------------+-------------+-------------+---------------------------------------------+
| @'StateT' s@             | @s@         | @First s@   | The change overwrites all previous changes  |
+--------------------------+-------------+-------------+---------------------------------------------+

But there are very useful state change applications not covered by these.

A typical example is a large state type (e.g., a user entry in a database of a webshop)
which only allows small changes (e.g., adding or deleting a delivery address).
When we want to be able to /restrict/ to specific changes (e.g., only the addresses should be changed),
and we want to be able to /inspect/ the changes,
then 'ChangesetT' is a good choice.
In our example, a general function on addresses, or even on the whole user, cannot be inspected.
But if we restrict to only adding or deleting addresses,
we can define a custom datatype such as:

@
data ChangeAddress
  -- | Add an address under a given key
  = Add Text Address
  -- | Delete the address for the given key
  | Delete Text
@

Changes for such a type (or rather, for the monoid @[ChangeAddress]@) can be inspected.

Orphan instances for newer (2.3) @mtl@ classes such as 'Control.Monad.Accum.MonadAccum' and 'Control.Monad.Selet.MonadSelect' can be found in "Control.Monad.Trans.Changeset.Orphan".
These are only provided for GHC >= 9.6.
-}
module Control.Monad.Trans.Changeset where

-- base
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Data.Bifunctor (Bifunctor (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Tuple (swap)
import Data.Foldable (foldr')

-- containers
import Data.Sequence (Seq, fromList, (|>))

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

-- changeset
import Control.Monad.Changeset.Class

-- * The 'ChangesetT' monad transformer

{- | Hold a state of type @s@, which is allowed to be mutated by an action of a monoid @w@.

The state @s@ has the role of the current state.
An @a@ is computed while performing a side effect in @m@,
and these can depend on the current state.

The type @w@ encodes /changes/ (or updates, edits, commits, diffs, patches ...) to the state @s@.
This relation is captured by the 'Action' type class from @monoid-extras@.
It contains a method, @'act' :: w -> s -> s@,
which implements the semantics of @w@ as the type of updates to @s@.

The standard example is that of a big record where we only want to change a small portion:

@
data User = User
  { name :: Text
  , password :: Hash
  , ...
  , addresses :: Map Text Address
  , ...
  }
@

If all changes that our business logic should be able to perform are adding or deleting an address,
it would be cumbersome to work in a @'State' User@ monad, since we only want to modify a small portion.
Instead, we define a type of /changes/ to @User@:

@
data ChangeAddress
  -- | Add an address under a given key
  = Add Text Address
  -- | Delete the address for the given key
  | Delete Text

instance Action ChangeAddress User where
  act = ...
@

Now we can conveniently work in the monad @'ChangesetT' User [ChangeAddress] m@.
(Note the list type which gives us a free 'Monoid' instance.)
Here we can perform operations like @'change' [Add "home" homeAddress]@ or @'change' [Delete "work"]@ to modify the addresses,
'current' to view the current state (containing all changes so far),
or apply a more complex function like @'revise' $ const $ filter (/= Delete "default")@ which would remove all changes that attempt to delete the @"default"@ address.

As a further example, if @s@ represents some type of time stamps, then @w@ can be a type of durations:
Two timestamps cannot be added, but two durations can.
A computation in @'ChangesetT' s w@ could then have access to some simulated notion of "current time",
while being able to add symbolic "delays".

Another class of examples arises operation based or commutative Conflict-free Replicated Data Type (CRDT).
Then @s@ is the internal state (the "payload") of the CRDT, and @w@ is the update operation.
For example @s = Int@, and for @w@ we would define @data Count = Increment | Decrement@.

The 'Monad' and 'Applicative' classes are defined by performing the first action,
then 'act'ing with the monoid output onto the state, and then perform the second action with the updated state.
So for example, @'change' Increment >> 'current'@ is different from @'current' >>= (\n -> 'change' Increment >> return n)@:
If we apply @'flip' 'evalChangeset' 0@ to each,
the first one would return 1, while the second returns 0.
-}
newtype ChangesetT s w m a = ChangesetT
  { getChangesetT :: s -> m (w, a)
  -- ^ Extract the changeset function without applying it to the state.
  }
  deriving (Functor)

-- ** Running a 'ChangesetT' action

-- | Extract the changes that would be applied.
getChangeT :: (Functor m) => ChangesetT s w m a -> s -> m w
getChangeT ChangesetT {getChangesetT} s = getChangesetT s <&> fst

-- | Run the action with an initial state and apply all resulting changes to it.
runChangesetT :: (Functor m, Action w s) => ChangesetT s w m a -> s -> m (a, s)
runChangesetT ChangesetT {getChangesetT} s = getChangesetT s <&> \(w, a) -> (a, act w s)

-- | Run the action with an initial state and extract only the value.
evalChangesetT :: (Functor m, Action w s) => ChangesetT s w m a -> s -> m a
evalChangesetT = fmap (fmap fst) . runChangesetT

-- | Run the action with an initial state and extract only the state.
execChangesetT :: (Functor m, Action w s) => ChangesetT s w m a -> s -> m s
execChangesetT = fmap (fmap snd) . runChangesetT

-- * 'ChangesetT' API with relaxed constraints

{- | See 'changeset'.

The @A@ suffix means that only 'Applicative' is required, not 'Monad'.
-}
changesetA :: (Applicative m) => (s -> (a, w)) -> ChangesetT s w m a
changesetA = ChangesetT . fmap (pure . swap)

{- | See 'change'.

The @A@ suffix means that only 'Applicative' is required, not 'Monad'.
-}
changeA :: (Applicative m) => w -> ChangesetT s w m ()
changeA w = ChangesetT $ const $ pure (w, ())

{- | See 'current'.

The @A@ suffix means that only 'Applicative' is required, not 'Monad'.
-}
currentA :: (Applicative m, Monoid w) => ChangesetT s w m s
currentA = ChangesetT $ \s -> pure (mempty, s)

instance (Action w s, Monoid w, Monad m) => MonadChangeset s w (ChangesetT s w m) where
  change = changeA
  current = currentA
  changeset = changesetA

-- | Like 'lift' from the 'MonadTrans' class, but with fewer constraints.
liftF :: (Functor m, Monoid w) => m a -> ChangesetT s w m a
liftF = ChangesetT . const . fmap (mempty,)

instance (Action w s, Monoid w) => MonadTrans (ChangesetT s w) where
  lift = liftF

-- ** Transforming 'ChangesetT' operations

{- | Change the action that would be applied.

The function in the second position of the tuple receives the initial state and the change that would be applied.
It has to output the action that will be applied instead.
-}
revise :: (Functor m) => ChangesetT s w m (a, s -> w -> w) -> ChangesetT s w m a
revise ChangesetT {getChangesetT} = ChangesetT $ \s -> getChangesetT s <&> \(w, (a, f)) -> (f s w, a)

-- | Adds the to-be-applied changes to the foreground value.
changelog :: (Functor m) => ChangesetT s w m a -> ChangesetT s w m (a, w)
changelog ChangesetT {getChangesetT} = ChangesetT $ fmap (\(w, a) -> (w, (a, w))) . getChangesetT

-- | Precomposes the current state with a function to  before computing the change.
withCurrent :: (s2 -> s1) -> ChangesetT s1 w m a -> ChangesetT s2 w m a
withCurrent f = ChangesetT . (. f) . getChangesetT

-- | Apply a function to the change.
mapChange :: (Functor m) => (w1 -> w2) -> ChangesetT s w1 m a -> ChangesetT s w2 m a
mapChange f = ChangesetT . fmap (fmap (first f)) . getChangesetT

-- ** Combining 'ChangesetT' operations

{- | Like '(<*>)' from 'Applicative', but ignore the change from the first action in the initial state for the second action.

This only needs an 'Applicative' constraint on @m@, not 'Monad'.
-}
(|*>) :: (Semigroup w, Applicative m) => ChangesetT s w m (a -> b) -> ChangesetT s w m a -> ChangesetT s w m b
ChangesetT mf |*> ChangesetT ma = ChangesetT $ \s -> (\(w1, f) (w2, a) -> (w1 <> w2, f a)) <$> mf s <*> ma s

-- | The @'Monad' m@ constraint is indeed necessary, since we need the log from the first action to change it to the state for the second action.
instance (Monoid w, Action w s, Monad m) => Applicative (ChangesetT s w m) where
  pure a = ChangesetT $ const $ pure (mempty, a)

  ChangesetT mf <*> ChangesetT ma = ChangesetT $ \s -> do
    (w1, f) <- mf s
    let !s' = act w1 s
    (w2, a) <- ma s'
    pure (w1 <> w2, f a)

instance (Action w s, Monoid w, Monad m) => Monad (ChangesetT s w m) where
  ChangesetT ma >>= f = ChangesetT $ \s -> do
    (w1, a) <- ma s
    let !s' = act w1 s
    (w2, b) <- getChangesetT (f a) s'
    return (w1 <> w2, b)

instance (Alternative m, Monoid w, Action w s, Monad m) => Alternative (ChangesetT s w m) where
  empty = liftF empty
  ChangesetT ma1 <|> ChangesetT ma2 = ChangesetT $ \s -> ma1 s <|> ma2 s

instance (Alternative m, Monoid w, Action w s, Monad m) => MonadPlus (ChangesetT s w m)

instance MFunctor (ChangesetT s w) where
  hoist = hoistF

-- | Like 'hoist' from the @mmorph@ package, but with no constraints.
hoistF :: (forall x. m x -> n x) -> ChangesetT s w m a -> ChangesetT s w n a
hoistF morph ma = ChangesetT $ morph . getChangesetT ma

instance (Action w s, Monoid w) => MMonad (ChangesetT s w) where
  embed f (ChangesetT g) = ChangesetT $ \s ->
    s
      & g
      & f
      & flip getChangesetT s
      <&> \(w1, (w2, b)) -> (w1 <> w2, b)

instance (MonadError e m, Action w s, Monoid w) => MonadError e (ChangesetT s w m) where
  throwError = lift . throwError
  catchError ma handler = ChangesetT $ \s -> getChangesetT ma s `catchError` (\e -> getChangesetT (handler e) s)

instance (MonadReader r m, Action w s, Monoid w) => MonadReader r (ChangesetT s w m) where
  ask = lift ask
  local f = hoist $ local f

instance (MonadRWS r w s m, Action w' s', Monoid w') => MonadRWS r w s (ChangesetT s' w' m)

instance (MonadState s m, Action w' s', Monoid w') => MonadState s (ChangesetT s' w' m) where
  state = lift . state

instance (MonadWriter w m, Action w' s, Monoid w') => MonadWriter w (ChangesetT s w' m) where
  writer = lift . writer
  listen = ChangesetT . fmap (fmap (\((w', a), w) -> (w', (a, w))) . listen) . getChangesetT
  pass = ChangesetT . fmap (pass . fmap (\(w', (a, f)) -> ((w', a), f))) . getChangesetT

-- * Pure changesets

{- | A pure changeset acts in the 'Identity' monad.
The only effects it has are inspecting the currentg state, and adding a change.

@'Changeset' s w a@ is isomorphic to @s -> (w, a)@.
-}
type Changeset s w = ChangesetT s w Identity

-- | Like 'getChangesetT'.
getChangeset :: Changeset s w a -> s -> (w, a)
getChangeset swa s = runIdentity $ getChangesetT swa s

-- | Like 'getChangeT'.
getChange :: Changeset s w a -> s -> w
getChange swa s = runIdentity $ getChangeT swa s

-- | Like 'runChangesetT'.
runChangeset :: (Action w s) => Changeset s w a -> s -> (a, s)
runChangeset swa s = runIdentity $ runChangesetT swa s

-- | Like 'evalChangesetT'.
evalChangeset :: (Action w s) => Changeset s w a -> s -> a
evalChangeset swa s = runIdentity $ evalChangesetT swa s

-- | Like 'execChangesetT'.
execChangeset :: (Action w s) => Changeset s w a -> s -> s
execChangeset swa s = runIdentity $ execChangesetT swa s

-- * 'Changes': container for changes that don't have a 'Monoid' instance

-- FIXME delete containers dep again?

-- | Collect changes
newtype Changes w = Changes {getChanges :: Seq w}
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Semigroup, Monoid, Foldable, Functor)

changes :: [w] -> Changes w
changes = Changes . fromList

addChange :: w -> Changes w -> Changes w
addChange w = Changes . (|> w) . getChanges

-- FIXME operator? shorthand?
singleChange :: w -> Changes w
singleChange = Changes . pure

changeSingle :: (MonadChangeset s (Changes w) m) => w -> m ()
changeSingle = change . singleChange

instance (Action w s) => Action (Changes w) s where
  act Changes {getChanges} s = foldr' act s getChanges
