module Data.Monoid.RightAction.Sequence where

import Data.Monoid.RightAction (RightAction (..))
import Data.Sequence

data SeqChange a
  = Cons a
  | Snoc a
  | Uncons
  | Unsnoc
  deriving (Show, Read, Eq, Ord, Functor)

instance RightAction (SeqChange a) (Seq a) where
  actRight s (Cons a) = a <| s
  actRight s (Snoc a) = s |> a
  actRight s Uncons = case viewl s of
    EmptyL -> empty
    _ :< as' -> as'
  actRight s Unsnoc = case viewr s of
    EmptyR -> empty
    as' :> _ -> as'
