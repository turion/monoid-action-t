module Control.Monad.Changeset.Lens.Setter where

-- base
import Data.Function ((&))
import Data.List (foldl')
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Setter', (%~))

-- containers
import Data.Sequence (Seq)

-- monoid-extras
import Data.Monoid.RightAction

-- changeset
import Control.Monad.Changeset.Class (
  MonadChangeset (..),
 )
import Data.Monoid (First (..))

data SetterChange s a w = SetterChange
  { setterChangeSetter :: Setter' s a
  , setterChangeChange :: w
  }

newtype SetterChangeset s a w = SetterChangeset
  {getSetterChangeset :: Seq (SetterChange s a w)}
  deriving newtype (Semigroup, Monoid)

instance (RightAction w a) => RightAction (SetterChangeset s a w) s where
  actRight s SetterChangeset {getSetterChangeset} = foldl' (\s' SetterChange {setterChangeSetter, setterChangeChange} -> s' & setterChangeSetter %~ flip actRight setterChangeChange) s getSetterChangeset

setterChangeset :: Setter' s a -> w -> SetterChangeset s a w
setterChangeset setterChangeSetter setterChangeChange = SetterChangeset $ pure $ SetterChange {setterChangeSetter, setterChangeChange}

(<>|>) :: (MonadChangeset s (SetterChangeset s a w) m) => Setter' s a -> w -> m ()
setter <>|> w = change $ setterChangeset setter w

(.|>) :: (MonadChangeset s (SetterChangeset s a (First a)) m) => Setter' s a -> a -> m ()
setter .|> a = setter <>|> First (Just a)
