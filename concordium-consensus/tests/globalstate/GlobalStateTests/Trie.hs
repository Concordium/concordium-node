{-# LANGUAGE ScopedTypeVariables #-}
module GlobalStateTests.Trie where

import Data.Word
import Control.Monad.IO.Class
import Data.Proxy
import Data.Serialize

import Test.QuickCheck
import Test.Hspec

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie


tests :: Spec
tests = describe "GlobalStateTests.Trie" $ do
    it "simple test" $ do
        runBlobStoreTemp "." $ do
            let e = Trie.empty :: Trie.TrieN (BufferedBlobbed BlobRef) Word64 (SerializeStorable String)
            e0 <- Trie.insert 27 (SerStore "Hello") e
            e1 <- Trie.insert 13 (SerStore "World") e0
            (p, e2) <- storeUpdate (Proxy :: Proxy BlobRef) e1
            let (Right me2') = runGet (load (Proxy :: Proxy BlobRef)) (runPut p)
            (e2' :: Trie.TrieN (CachedBlobbed BlobRef) Word64 (SerializeStorable String)) <- me2'
            liftIO $ do
                print e0
                print e1
                print e2
                print e2'
            r <- Trie.lookup 27 e2'
            liftIO $ r `shouldBe` (Just (SerStore "Hello"))

        