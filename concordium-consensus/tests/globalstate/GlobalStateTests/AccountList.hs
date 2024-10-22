module GlobalStateTests.AccountList where

import Control.Monad
import Data.Foldable
import Test.Hspec
import Test.QuickCheck

import Concordium.Types

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cooldown

-- | Run an action in the 'MemBlobStoreT' monad transformer from an empty store.
runBlobStore :: MemBlobStoreT IO a -> IO a
runBlobStore a = do
    mbs <- newMemBlobStore
    runMemBlobStoreT a mbs

data ListAction
    = -- | Add an account to the list.
      Cons AccountIndex
    | -- | Remove the first instance an account from the list.
      Remove AccountIndex
    | -- | Flush the list to the blob store.
      StoreUpdate
    | -- | Store and reload the list from the blob store
      StoreLoad
    deriving (Show)

instance Arbitrary ListAction where
    arbitrary =
        frequency
            [ (5, Cons . AccountIndex <$> arbitrary),
              (1, Remove . AccountIndex <$> arbitrary),
              (1, pure StoreUpdate),
              (1, pure StoreLoad)
            ]

-- | Determine the list that should result from a sequence of list actions.
ofListActions :: [ListAction] -> [AccountIndex]
ofListActions = foldl' go []
  where
    go acc (Cons x) = x : acc
    go acc (Remove x) = removeFirst x acc
    go acc _ = acc
    removeFirst _ [] = []
    removeFirst x (y : ys)
        | x == y = ys
        | otherwise = y : removeFirst x ys

-- | Test that a sequence of list actions results in the expected list.
testActions :: [ListAction] -> Property
testActions actions = ioProperty $ do
    list <- runBlobStore $ loadAccountList =<< foldM act Null actions
    return $ ofListActions actions === list
  where
    act al (Cons x) = consAccountList x al
    act al (Remove x) = removeAccountFromAccountList x al
    act al StoreUpdate = snd <$> storeUpdate al
    act al StoreLoad = loadRef =<< storeRef al

tests :: Spec
tests = describe "AccountList" $ do
    it "arbitrary list actions" $ withMaxSuccess 10000 $ property testActions
