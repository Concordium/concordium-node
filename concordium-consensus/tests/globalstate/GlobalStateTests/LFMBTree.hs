{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{-
Creates a new LFMBTree using HashedBufferedRefs and BufferedRefs and check it is well formed and that items are accessible.

In the case of the tree that uses HashedBufferedRefs we also check that the hash of the structure is the correct one even after
updating one of the nodes.
-}
module GlobalStateTests.LFMBTree where

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMBT
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LFMBTree
import Concordium.Types.HashableTo
import Concordium.Types.ProtocolVersion
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (lookup)

abcHash, abcorrectHash :: LFMBT.LFMBTreeHashV0
abcHash = LFMBT.LFMBTreeHash $ H.hashOfHashes (H.hashOfHashes (H.hash "A") (H.hash "B")) (H.hash "C") -- "dbe11e36aa89a963103de7f8ad09c1100c06ccd5c5ad424ca741efb0689dc427"
abcorrectHash = LFMBT.LFMBTreeHash $ H.hashOfHashes (H.hashOfHashes (H.hash "A") (H.hash "B")) (H.hash "Correct") -- "084aeef37cbdb2e19255853cbae6d22c78aaaea7273aa39af4db96cc62c9bdac"

abcHashV1, abcorrectHashV1 :: LFMBT.LFMBTreeHashV1
abcHashV1 = LFMBT.LFMBTreeHash . H.hashLazy . S.runPutLazy $ do
    S.putWord64be 3
    S.put abcHash
abcorrectHashV1 = LFMBT.LFMBTreeHash . H.hashLazy . S.runPutLazy $ do
    S.putWord64be 3
    S.put abcorrectHash

testingFunction :: IO ()
testingFunction = do
    runBlobStoreTemp
        "."
        ( do
            tree <- foldM (\acc v -> snd <$> append v acc) (empty :: LFMBTree Word64 HashedBufferedRef BS.ByteString) ["A", "B", "C"]
            testElements <- mapM (`lookup` tree) [0 .. 3]
            liftIO $ testElements `shouldBe` map Just ["A", "B", "C"] ++ [Nothing]
            h <- getHashM tree
            liftIO $ h `shouldBe` abcHash
            tree' <- loadRef =<< (storeRef tree :: BlobStoreM (BlobRef (LFMBTree Word64 HashedBufferedRef BS.ByteString)))
            testElements' <- mapM (`lookup` tree') [0 .. 3]
            liftIO $ testElements' `shouldBe` map Just ["A", "B", "C"] ++ [Nothing]
            h' <- getHashM tree'
            liftIO $ h' `shouldBe` abcHash
            Just (_, tree'') <- update (\v -> return ((), v `BS.append` "orrect")) 2 tree'
            testElements'' <- mapM (`lookup` tree'') [0 .. 3]
            liftIO $ testElements'' `shouldBe` map Just ["A", "B", "Correct"] ++ [Nothing]
            h'' <- getHashM tree''
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

testHashAsLFMBTV0 :: Property
testHashAsLFMBTV0 = forAll (fmap BS.pack <$> listOf (vector 10)) $ \bs ->
    LFMBT.hashAsLFMBTV0 (H.hash "EmptyLFMBTree") (getHash <$> bs)
        === LFMBT.theLFMBTreeHash @'BlockHashVersion0 (getHash (LFMBT.fromFoldable @Word64 bs))

testHashAsLFMBTV1 :: Property
testHashAsLFMBTV1 = forAll (fmap BS.pack <$> listOf (vector 10)) $ \bs ->
    LFMBT.hashAsLFMBTV1 (H.hash "EmptyLFMBTree") (getHash <$> bs)
        === LFMBT.theLFMBTreeHash @'BlockHashVersion1 (getHash (LFMBT.fromFoldable @Word64 bs))

tests :: Spec
tests =
    describe "GlobalStateTests.LFMBTree" $ do
        it
            "Using HashedBufferedRef"
            testingFunction
        it
            "Using BufferedRef"
            testingFunction2
        it
            "testHashAsLFMBTV0"
            testHashAsLFMBTV0
        it
            "testHashAsLFMBTV1"
            testHashAsLFMBTV1
