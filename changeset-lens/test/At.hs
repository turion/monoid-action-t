module At where

-- base
import Data.Char (toLower, toUpper)
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- containers
import Data.IntMap (IntMap, singleton)
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- changeset
import Control.Monad.Trans.Changeset
import Data.Monoid.RightAction (rEndo)
import Data.Monoid.RightAction.Coproduct (inL, inR, normaliseCoproduct)
import Data.Monoid.RightAction.IntMap

-- changeset-lens
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Changeset.Lens.At

tests :: TestTree
tests =
  testGroup
    "AtChangeset"
    [ testGroup
        "IntMap"
        [ testCase "Changing elements" $
            let action = do
                  0 <>@ justChange (rEndo not)
                  1 <>@ justChange (rEndo not)
             in execChangeset action (IM.singleton 0 True) @?= singleton 0 False
        , testCase "Adding and removing elements" $
            let action = do
                  0 <>@ setJust True
                  1 <>@ setJust False
                  1 <>@ setJust True
                  2 <>@ setJust False
                  2 <>@ setNothing
                  3 <>@ setNothing
                  4 <>@ setJust True
             in execChangeset action (IM.fromList [(0, False), (3, True)]) @?= IM.fromList [(0, True), (1, True), (4, True)]
        ]
    , testCase "Map" $
        let action = do
              "hello" <>@ justChange (rEndo (map toUpper))
              "world" <>@ justChange (rEndo (map (toLower . toUpper)))
         in execChangeset action (M.fromList [("hello", "hello"), ("world", "wOrLd")])
              @?= M.fromList [("hello", "HELLO"), ("world", "world")]
    , testCase "is inspectable" $
        let action = 0 @~ False :: Changeset (IntMap Bool) (IntMapAtChangeset Bool) ()
         in getChange action (IM.singleton 0 True) @?= atChangeset 0 (setJust False)
    , testCase "Last" $
        let action = 0 @~ False
         in execChangeset action (IM.singleton 0 True) @?= IM.singleton 0 False
    , testGroup
        "containers :+: AtChangeset"
        [ testCase "Can change after insert" $
            let action = do
                  change $ inL $ singleChange (Insert 0 True :: IntMapChange Bool)
                  -- Adding type signatures so GHC < 9 doesn't freak out
                  mapChange inR (0 @~ False :: Changeset (IntMap Bool) (IntMapAtChangeset Bool) ())
             in do
                  normaliseCoproduct (getChange action IM.empty)
                    @?= [ Left $ singleChange $ Insert 0 True
                        , Right $ atChangeset 0 $ setJust False
                        ]
                  execChangeset action mempty @?= IM.singleton 0 False
        , testCase "<>@ only affects same key" $
            let action = do
                  mapChange inL $ changeSingle (Insert 0 True :: IntMapChange Bool)
                  mapChange inR $ 1 <>@ justChange (rEndo not)
             in execChangeset action mempty @?= singleton 0 True
        ]
    ]
