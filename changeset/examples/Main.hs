module Main (main) where

-- base
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- falsify
-- import Test.Tasty.Falsify

-- monoid-extras
import Data.Monoid.Action (Action (..))

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad (replicateM_)

type M = Changeset Int (Changes Count)

data Count = Increment

instance Action Count Int where
  act Increment count = count + 1

main :: IO ()
main =
  defaultMain $
    testGroup
      "examples"
      [ testGroup
          "Changeset"
          [ testGroup "commutative monoids"
          [ testGroup
              "Order of change and current matters"
              [ testCase "change, current" $
                  evalChangeset (change (singleChange Increment) >> current) 0 @?= (1 :: Int)
              , testCase "current, change" $
                  let action = flip evalChangeset 0 $ do
                        n <- current
                        changeSingle Increment
                        return n
                   in action @?= (0 :: Int)
              ]
          , testGroup
              "execChangeset"
              [ testCase "pure doesn't change state" $ execChangeset (pure () :: M ()) 0 @?= 0
              , testCase "change changes state" $ execChangeset (change (singleChange Increment) :: M ()) 0 @?= 1
              ]
          ]
          , testGroup "noncommutative monoids" [
            testGroup "Changes"[
              testCase "change is monoid homomorphism" $ do
                execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (changeSingle (singleChange (Cons True) <> singleChange (Cons False))) ([] :: [Bool])
                execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (changes [Cons True, Cons False])) ([] :: [Bool])
                execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (changes [Cons True, Cons False])) ([] :: [Bool])
                execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= ([True, False] :: [Bool])

            ]
          ]]
      , testGroup
          "MonadChangeset"
          [ testCase "ReaderT lifts changeset operations" $
              let action = flip execChangeset (0 :: Int) $ flip runReaderT (100 :: Int) $ do
                    env <- ask
                    replicateM_ env $ changeSingle Increment
               in action @?= 100
          ]
      ]

data ListChange a = Cons a | Pop

instance Action (ListChange a) [a] where
  act (Cons a) = (a :)
  act Pop = drop 1
