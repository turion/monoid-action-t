{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Trans.Changeset.AccumExample where

-- base
import Data.Functor.Identity (Identity)
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Accum (runAccum)

-- mtl
import Control.Monad.Accum (MonadAccum (..))
import Control.Monad.Changeset.Class (MonadChangeset (changeset))

-- monoid-extras
import Data.Monoid.Action (Regular (Regular, getRegular))

-- changeset
import Control.Monad.Trans.Changeset

-- tasty
import Test.Tasty (TestTree)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- | The 'AccumT' monad transformer is a special case of 'ChangesetT' when both state and change are the same type.
type RegularAccumT w = ChangesetT (Regular w) w

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadAccum w (RegularAccumT w m) where
  accum = changeset . (. getRegular)

exampleProgram :: (MonadAccum (Changes (ListChange Int)) m) => m (Changes (ListChange Int))
exampleProgram = do
  add $ singleChange (Cons 1)
  add $ singleChange (Cons 2)
  ns <- look
  add $ singleChange (Cons 3)
  pure ns

initialState :: Changes (ListChange Int)
initialState = singleChange $ Cons 0

tests :: TestTree
tests = testCase "Accum" $ runAccum exampleProgram initialState @?= swap (getChangeset (exampleProgram :: RegularAccumT (Changes (ListChange Int)) Identity (Changes (ListChange Int))) (Regular initialState))
