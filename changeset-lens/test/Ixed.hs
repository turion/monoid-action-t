module Ixed where

-- base
import Data.Char (toLower, toUpper)
import Data.Monoid (Dual (..), Endo (..), Last (..))
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- containers
import Data.IntMap (singleton)
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- changeset
import Control.Monad.Trans.Changeset

-- changeset-lens
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Changeset.Lens.Ixed
import Data.Monoid.RightAction (RightAction (..), rEndo)
import Data.Monoid.RightAction.Coproduct (inL, inR, normaliseCoproduct, (:+:))

-- changeset-containers
import Data.Monoid.RightAction.IntMap

tests :: TestTree
tests =
  testGroup
    "IxedChangeset"
    [ testCase "IntMap" $
        let action = do
              0 !<> rEndo not
              1 !<> rEndo not
         in execChangeset action (IM.singleton 0 True) @?= singleton 0 False
    , testCase "Map" $
        let action = do
              "hello" !<> rEndo (map toUpper)
              "world" !<> rEndo (map toUpper)
              "world" !<> rEndo (map toLower)
         in execChangeset action (M.fromList [("hello", "hello"), ("world", "wOrLd")])
              @?= M.fromList [("hello", "HELLO"), ("world", "world")]
    , testCase "is inspectable" $
        let action = 0 !<> Last (Just False)
         in getChange action (IM.singleton 0 True) @?= ixedChangeset 0 (Last (Just False))
    , testCase "Last" $
        let action = 0 !<> Last (Just False)
         in execChangeset action (IM.singleton 0 True) @?= IM.singleton 0 False
    , testGroup
        "containers :+: IxedChangeset"
        [ testCase "Can change after insert" $
            let action = do
                  change $ inL $ singleChange (Insert 0 True :: IntMapChange Bool)
                  mapChange inR $ 0 !<> Last (Just False)
             in do
                  normaliseCoproduct (getChange action IM.empty)
                    @?= [ Left $ singleChange $ Insert 0 True
                        , Right $ ixedChangeset 0 $ Last $ Just False
                        ]
                  execChangeset action mempty @?= IM.singleton 0 False
        , testCase "!<> only affects same key" $
            let action = do
                  mapChange inL $ changeSingle (Insert 0 True :: IntMapChange Bool)
                  mapChange inR $ 1 !<> rEndo not
             in execChangeset action mempty @?= singleton 0 True
        ]
    ]
