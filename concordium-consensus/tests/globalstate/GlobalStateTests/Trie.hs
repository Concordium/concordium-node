{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module GlobalStateTests.Trie where

import Data.Word
import Control.Monad.IO.Class
import Data.Serialize

-- import Test.QuickCheck
import Test.Hspec

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie

-- | Newtype providing a @BlobStorable@ reference for every wrapped type
--  that is an instance of @Serialize@
newtype SerializeStorable v = SerStore v
  deriving newtype (Eq, Ord, Show, Serialize)

-- Every @SerializeStorable@ value will be serialized with the default implementation
instance (Serialize v, MonadBlobStore m) => BlobStorable m (SerializeStorable v)

tests :: Spec
tests = describe "GlobalStateTests.Trie" $
    it "simple test" $
        runBlobStoreTemp "." $ do
            let e = Trie.empty :: Trie.TrieN (BufferedBlobbed BlobRef) Word64 (SerializeStorable String)
            e0 <- Trie.insert 27 (SerStore "Hello") e
            e1 <- Trie.insert 13 (SerStore "World") e0
            (p, _e2) <- storeUpdate e1
            let (Right me2') = runGet load (runPut p)
            (e2' :: Trie.TrieN (CachedBlobbed BlobRef) Word64 (SerializeStorable String)) <- me2'
            r <- Trie.lookup 27 e2'
            liftIO $ r `shouldBe` Just (SerStore "Hello")
