module Data.Monoid.RightAction where
import Data.Monoid (Last (..), Dual (..))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Monoid.Action (Action (..))
import Data.Typeable (Typeable)
import Data.Foldable (Foldable(foldl'))

class RightAction m s where
  actRight :: s -> m -> s
  actRight s _ = s

instance RightAction () s

instance RightAction m ()

instance RightAction Void s

instance RightAction (Last s) s where
  actRight s (Last ms) = fromMaybe s ms

instance Action m s => RightAction (Dual m) s where
  actRight s (Dual m) = act m s

instance RightAction m s => RightAction (Maybe m) s where
  actRight s = maybe s (actRight s)

newtype (:+:) m n = Coproduct {getCoproduct :: [Either m n]}
  deriving (Typeable, Semigroup, Monoid)

normaliseCoproduct :: (Semigroup m, Semigroup n) => m :+: n -> [Either m n]
normaliseCoproduct = normaliseCoproduct' . getCoproduct
  where
    normaliseCoproduct' (Left m1 : Left m2 : emns) = normaliseCoproduct' $ Left (m1 <> m2) : emns
    normaliseCoproduct' (Right n1 : Right n2 : emns) = normaliseCoproduct' $ Right (n1 <> n2) : emns
    normaliseCoproduct' [] = []
    normaliseCoproduct' (emn : emns) = emn : normaliseCoproduct' emns

instance (Eq m, Eq n, Semigroup m, Semigroup n) => Eq (m :+: n) where
  mns1 == mns2 = normaliseCoproduct mns1 == normaliseCoproduct mns2

inL :: m -> m :+: n
inL = Coproduct . pure . Left

inR :: n -> m :+: n
inR = Coproduct . pure . Right

instance (RightAction m s, RightAction n s) => RightAction (m :+: n) s where
  actRight s mns = foldl' (flip $ either (flip actRight) (flip actRight)) s (getCoproduct mns)
