module Main (main) where

-- base

import Data.Char (toLower, toUpper)
import Data.Monoid (Endo (..), Last (..), Dual (..), Last (Last))
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Lens', lens)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- falsify
-- import Test.Tasty.Falsify

-- monoid-extras
import Data.Monoid.RightAction (RightAction (..), (:+:), inL, inR, normaliseCoproduct)

-- containers
import Data.IntMap (singleton)
import qualified Data.IntMap as IM
import qualified Data.Map as M


-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset

-- changeset-containers
import Data.Monoid.RightAction.IntMap

-- changeset-lens
import Control.Monad.Changeset.Lens

data Count = Increment

data Big = Big
  { irrelevant :: String
  , counter :: Int
  }
  deriving (Eq, Show)

lensCounter :: Lens' Big Int
lensCounter = lens counter $ \big counter -> big {counter}

instance RightAction Count Int where
  actRight counter Increment = counter + 1

-- act counts n = foldl' (\n' Increment -> n' + 1) n counts

main :: IO ()
main =
  defaultMain $
    testGroup
      "lens"
      [ testGroup
          "Small change on big data structure"
          [ testCase "single change" $ execChangeset (change (lensChangeset lensCounter (singleChange Increment))) (Big "foo" 0) @?= Big "foo" 1
          ]
      , testGroup
          "IxedChangeset"
          [ testCase "IntMap" $
              let action = do
                    0 <>@|> Dual (Endo not)
                    1 <>@|> Dual (Endo not)
               in execChangeset action (IM.singleton 0 True) @?= singleton 0 False
          , testCase "Map" $
              let action = do
                    "hello" <>@|> Dual (Endo (map toUpper))
                    "world" <>@|> Dual (Endo (map (toLower . toUpper)))
               in execChangeset action (M.fromList [("hello", "hello"), ("world", "wOrLd")])
                    @?= M.fromList [("hello", "HELLO"), ("world", "world")]
          , testCase "is inspectable" $
              let action = 0 <>@|> Last (Just False)
               in getChange action (IM.singleton 0 True) @?= ixedChangeset 0 (Last (Just False))
          , testCase "Last" $
              let action = 0 <>@|> Last (Just False)
               in execChangeset action (IM.singleton 0 True) @?= IM.singleton 0 False
          ]
      , testGroup
          "AtChangeset"
          [] -- FIXME
      , testGroup
          "containers :+: IxedChangeset"
          [ testCase "Can change after insert" $
              let action = do
                    change $ inL $ singleChange (Insert 0 True :: IntMapChange Bool)
                    mapChange inR $ 0 <>@|> Last (Just False)
               in do
                    -- TODO monoid-extras doesn't export constructor nor define Eq instance
                    -- (https://github.com/diagrams/monoid-extras/issues/59)
                    normaliseCoproduct (getChange action IM.empty) @?= [
                        Left $ singleChange $ Insert 0 True
                        , Right $ ixedChangeset 0 $ Last $ Just False
                        ]
                    -- normaliseCoproduct (getChange action IM.empty) @?= [Left (Changes {getChanges = fromList [Insert 0 True]}),Right (IxedChangeset {getIxedChangeset = MonoidalMap {getMonoidalMap = fromList [(0,Last {getLast = Just False})]}})]
                    execChangeset action mempty @?= IM.singleton 0 False
          , testCase ":+: is monoid morphism" $
              actRight (0 :: Int) (inL (Last (Just 1)) <> inL (Last (Just 2)) :: Last Int :+: Last Int) @?= actRight 0 (inL (Last (Just (1 :: Int)) <> Last (Just 2)) :: Last Int :+: Last Int)
          , testCase "adjust only affects same key" $
              let action = do
                    changeSingle (Insert 0 True :: IntMapChange Bool)
               in -- change [Adjust 1 (Endo not)]
                  execChangeset action mempty @?= singleton 0 True
          ]
      , testGroup "nested IxedChangeset with SetterChangeset" []
      -- FIXME something with IntMapChange (SetterChangeset Big Count)
      ]
