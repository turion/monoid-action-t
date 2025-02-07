module Data.Monoid.Action.Sequence where

import Data.Monoid.Action (Action (..))
import Data.Sequence

data SeqChange a
  = Cons a
  | Snoc a
  | Uncons
  | Unsnoc
  deriving (Show, Read, Eq, Ord, Functor)

instance Action (SeqChange a) (Seq a) where
  act (Cons a) = (a <|)
  act (Snoc a) = (|> a)
  act Uncons = \as -> case viewl as of
    EmptyL -> empty
    _ :< as' -> as'
  act Unsnoc = \as -> case viewr as of
    EmptyR -> empty
    as' :> _ -> as'
