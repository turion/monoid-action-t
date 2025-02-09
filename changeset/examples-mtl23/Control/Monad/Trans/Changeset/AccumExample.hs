{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Trans.Changeset.AccumExample where

-- base
import Data.Monoid (Dual (Dual, getDual))

-- mtl
import Control.Monad.Accum (MonadAccum (..))
import Control.Monad.Changeset.Class (MonadChangeset (changeset))

-- monoid-extras
import Data.Monoid.Action (Regular (getRegular, Regular))

-- changeset
import Control.Monad.Trans.Changeset

-- tasty
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Monad.Trans.Accum (runAccum)
import Data.Functor.Identity (Identity)
import Data.Bifunctor (Bifunctor(first))
import Data.Tuple (swap)


  -- | The 'AccumT' monad transformer is a special case of 'ChangesetT' when both state and change are the same type.
type RegularAccumT w = ChangesetT (Regular w) (Dual w)
-- FIXME is it right that we have Dual here? Test!


instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadAccum w (RegularAccumT w m) where
  accum = changeset . fmap (fmap Dual) . (. getRegular)


exampleProgram :: MonadAccum (Changes (ListChange Int)) m => m (Changes (ListChange Int))
exampleProgram = do
  add $ singleChange (Cons 1)
  add $ singleChange (Cons 2)
  ns <- look
  add $ singleChange (Cons 3)
  pure ns

initialState :: Changes (ListChange Int)
initialState = singleChange $ Cons 0

tests :: TestTree
tests = testGroup "Accum" [

  testCase "" $ runAccum exampleProgram initialState @?= swap (first getDual (getChangeset (exampleProgram :: RegularAccumT (Changes (ListChange Int)) Identity (Changes (ListChange Int))) (Regular initialState)))
  ]
