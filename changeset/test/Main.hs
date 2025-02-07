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
import Data.Monoid.Action (Action (..), Regular (..))

-- changeset

import Control.Monad (replicateM_)
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Monoid (Endo(..))

type M = Changeset Int (Changes Count)

data Count = Increment

instance Action Count Int where
  act Increment count = count + 1

main :: IO ()
main =
  defaultMain $
    testGroup
      "changeset"
      [ testGroup
          "Changeset"
          [ testGroup
              "commutative monoids"
              [ testGroup
                  "Order of change and current matters"
                  [ testCase "change, current" $
                      evalChangeset (changeSingle Increment >> current) 0 @?= (1 :: Int)
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
                  , testCase "change changes state" $ execChangeset (changeSingle Increment :: M ()) 0 @?= 1
                  ]
              ]
          , testGroup
              "noncommutative monoids"
              [ testGroup
                  "Changes"
                  [ testCase "change is monoid homomorphism" $ do
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (singleChange (Cons True) <> singleChange (Cons False))) ([] :: [Bool])
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (addChange (Cons False) (singleChange (Cons True)))) ([] :: [Bool])
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (changes [Cons True, Cons False])) ([] :: [Bool])
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= ([True, False] :: [Bool])
                  , testCase "execChangeset is monoid homomorphism" $
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (changeSingle (Cons True)) (execChangeset (changeSingle (Cons False)) ([] :: [Bool]))
                  ]
              , testGroup "Regular" [
                testCase "change is monoid homomorphism" $
                  getRegular (execChangeset (change [True] >> change [False]) (Regular ([] :: [Bool]))) @?= getRegular (execChangeset (change [True, False]) (Regular ([] :: [Bool])))
                  , testCase "execChangeset is monoid homomorphism" $
                    getRegular (execChangeset (change [True] >> change [False]) (Regular ([] :: [Bool]))) @?= getRegular (appEndo (Endo (execChangeset (change [True] :: Changeset (Regular [Bool]) [Bool] ())) <> Endo (execChangeset (change [False]))) (Regular ([] :: [Bool])))
                  , testCase "execChangeset works in same order as do notation" $
                      let action = do
                            change [True] :: Changeset (Regular [Bool]) [Bool] ()
                            change [False]
                          stepByStep =
                            let
                              s = execChangeset (change [True]) (Regular ([] :: [Bool]))
                            in execChangeset (change [False]) s
                      in getRegular (execChangeset action (Regular [])) @?= getRegular stepByStep
              ]
              ]
          ]
      , testGroup
          "Changes"
          [ testCase "is lawful monoid action" $ do
              act (singleChange (Cons True)) (act (singleChange (Cons False)) []) @?= act (singleChange (Cons True) <> singleChange (Cons False)) ([] :: [Bool])
          ]
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
