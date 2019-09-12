module GlobalStateTests.Trie where

import Data.Word
import Control.Monad.IO.Class

import Test.QuickCheck
import Test.Hspec

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie


tests :: Spec
tests = describe "GlobalStateTests.Trie" $ do
    it "simple test" $ do
        runBlobStoreTemp "." $ do
            let e = Trie.empty :: Trie.TrieN (Blobbed BlobRef) Word64 String
            e' <- Trie.insert 27 "Hello" e
            e'' <- Trie.insert 13 "World" e'
            r <- Trie.lookup 27 e''
            liftIO $ r `shouldBe` (Just "Hello")
            e3 <- Trie.delete 27 e''
            r2 <- Trie.lookup 27 e3
            liftIO $ r2 `shouldBe` Nothing

        