module Data.Monoid.Action.Sequence where

import Data.Monoid.Action (Action (..))
import Data.Sequence

data SeqChange a
  = Cons a
  | Snoc a

instance Action (SeqChange a) (Seq a) where
  act (Cons a) = (a <|)
  act (Snoc a) = (|> a)
