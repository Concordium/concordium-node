{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module GlobalStateTests.Trie where

import Data.Word
import Control.Monad.IO.Class
import Data.Serialize

import Test.QuickCheck
import Test.Hspec

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie

-- | Newtype providing a @BlobStorable@ reference for every wrapped type
--  that is an instance of @Serialize@
newtype SerializeStorable v = SerStore v
  deriving newtype (Eq, Ord, Show, Serialize)

-- Every @SerializeStorable@ value will be serialized with the default implementation
instance (Serialize v, MonadBlobStore m) => BlobStorable m (SerializeStorable v)

genBranchList :: Gen [Nullable Int]
genBranchList = fmap (maybe Null Some) <$> vector 256

testBranchesFromToList :: Property
testBranchesFromToList = forAll genBranchList $ \v ->
     v === Trie.branchesToList (Trie.branchesFromList v)

testBranchAtFromList :: Property
testBranchAtFromList = forAll genBranchList $ \v ->
    let branches = Trie.branchesFromList v
     in conjoin [Trie.branchAt branches i === x | (i, x) <- zip [0 ..] v]

testUpdateBranchSome :: Property
testUpdateBranchSome = forAll genBranchList $ \v i x ->
    let v' = Trie.branchesToList $ Trie.updateBranch i (Some x) $ Trie.branchesFromList v
        (h, _ : r) = splitAt (fromIntegral i) v
     in conjoin (zipWith (===) v' (h ++ Some x : r))

testUpdateBranchNull :: Property
testUpdateBranchNull = forAll genBranchList $ \v i ->
    let v' = Trie.branchesToList $ Trie.updateBranch i Null $ Trie.branchesFromList v
        (h, _ : r) = splitAt (fromIntegral i) v
     in conjoin (zipWith (===) v' (h ++ Null : r))


tests :: Spec
tests = describe "GlobalStateTests.Trie" $ do
    it "simple test" $
        runBlobStoreTemp "." $ do
            let e = Trie.empty :: Trie.TrieN BufferedFix Word64 (SerializeStorable String)
            e0 <- Trie.insert 27 (SerStore "Hello") e
            e1 <- Trie.insert 13 (SerStore "World") e0
            (p, _e2) <- storeUpdate e1
            let (Right me2') = runGet load (runPut p)
            (e2' :: Trie.TrieN BufferedFix Word64 (SerializeStorable String)) <- me2'
            r <- Trie.lookup 27 e2'
            liftIO $ r `shouldBe` Just (SerStore "Hello")
    it "branchesFromToList" $ withMaxSuccess 10000 testBranchesFromToList
    it "branchAtFromList" $ withMaxSuccess 10000 testBranchAtFromList
    it "updateBranchSome" $ withMaxSuccess 10000 testUpdateBranchSome
    it "updateBranchNull" $ withMaxSuccess 10000 testUpdateBranchNull    