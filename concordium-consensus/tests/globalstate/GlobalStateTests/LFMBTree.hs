{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{-
Creates a new LFMBTree using HashedBufferedRefs and BufferedRefs and check it is well formed and that items are accessible.

In the case of the tree that uses HashedBufferedRefs we also check that the hash of the structure is the correct one even after
updating one of the nodes.
-}
module GlobalStateTests.LFMBTree where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LFMBTree
import Concordium.Types.HashableTo
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Word
import Test.Hspec
import Prelude hiding (lookup)

abcHash, abcorrectHash :: H.Hash
abcHash = H.hashOfHashes (H.hashOfHashes (H.hash "A") (H.hash "B")) (H.hash "C") -- "dbe11e36aa89a963103de7f8ad09c1100c06ccd5c5ad424ca741efb0689dc427"
abcorrectHash = H.hashOfHashes (H.hashOfHashes (H.hash "A") (H.hash "B")) (H.hash "Correct") -- "084aeef37cbdb2e19255853cbae6d22c78aaaea7273aa39af4db96cc62c9bdac"

testingFunction :: IO ()
testingFunction = do
    runBlobStoreTemp
        "."
        ( do
            tree <- foldM (\acc v -> snd <$> append v acc) (empty :: LFMBTree Word64 HashedBufferedRef BS.ByteString) ["A", "B", "C"]
            testElements <- mapM (`lookup` tree) [0 .. 3]
            liftIO $ testElements `shouldBe` map Just ["A", "B", "C"] ++ [Nothing]
            h <- getHashM tree :: BlobStoreM H.Hash
            liftIO $ h `shouldBe` abcHash
            tree' <- loadRef =<< (storeRef tree :: BlobStoreM (BlobRef (LFMBTree Word64 HashedBufferedRef BS.ByteString)))
            testElements' <- mapM (`lookup` tree') [0 .. 3]
            liftIO $ testElements' `shouldBe` map Just ["A", "B", "C"] ++ [Nothing]
            h' <- getHashM tree' :: BlobStoreM H.Hash
            liftIO $ h' `shouldBe` abcHash
            Just (_, tree'') <- update (\v -> return ((), v `BS.append` "orrect")) 2 tree'
            testElements'' <- mapM (`lookup` tree'') [0 .. 3]
            liftIO $ testElements'' `shouldBe` map Just ["A", "B", "Correct"] ++ [Nothing]
            h'' <- getHashM tree'' :: BlobStoreM H.Hash
            liftIO $ h'' `shouldBe` abcorrectHash
        )

testingFunction2 :: IO ()
testingFunction2 = do
    runBlobStoreTemp
        "."
        ( do
            tree <- foldM (\acc v -> snd <$> append v acc) (empty :: LFMBTree Word64 BufferedRef BS.ByteString) ["A", "B", "C"]
            testElements <- mapM (`lookup` tree) [0 .. 3]
            liftIO $ testElements `shouldBe` map Just ["A", "B", "C"] ++ [Nothing]
            tree' <- loadRef =<< (storeRef tree :: BlobStoreM (BlobRef (LFMBTree Word64 BufferedRef BS.ByteString)))
            testElements' <- mapM (`lookup` tree') [0 .. 3]
            liftIO $ testElements' `shouldBe` map Just ["A", "B", "C"] ++ [Nothing]
            Just (_, tree'') <- update (\v -> return ((), v `BS.append` "orrect")) 2 tree'
            testElements'' <- mapM (`lookup` tree'') [0 .. 3]
            liftIO $ testElements'' `shouldBe` map Just ["A", "B", "Correct"] ++ [Nothing]
        )

tests :: Spec
tests =
    describe "GlobalStateTests.Trie" $ do
        it
            "Using HashedBufferedRef"
            testingFunction
        it
            "Using BufferedRef"
            testingFunction2
