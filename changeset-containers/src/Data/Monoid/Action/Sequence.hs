module Data.Monoid.Action.Sequence where

import Data.Monoid.Action (Action (..))
import Data.Sequence

data SeqChange a w
  = Cons a
  | Snoc a
  | Adjust Int w

instance (Action w a) => Action (SeqChange a w) (Seq a) where
  act (Cons a) = (a <|)
  act (Snoc a) = (|> a)
  act (Adjust k w) = adjust (act w) k

type SeqChange' a = SeqChange a ()
