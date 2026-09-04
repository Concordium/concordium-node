{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
import qualified Data.ByteString.Base16.Lazy as B16
import Data.Either
import Data.IORef
import qualified Data.Serialize as S
import Data.String
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (lookup)

newtype TestVal = TestVal Word64
    deriving (Eq, Ord, Show, S.Serialize, Arbitrary, Num, Integral, Real, Enum)

instance HashableTo H.Hash TestVal where
    getHash = H.hash . S.encode

instance (Monad m) => MHashableTo m H.Hash TestVal

instance (MonadBlobStore m) => BlobStorable m TestVal

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

testTraverseWhileDescRef :: [TestVal] -> Property
testTraverseWhileDescRef ws = forAll (chooseBoundedIntegral (0, length ws)) $ \n ->
    ioProperty $ do
        runBlobStoreTemp
            "."
            ( do
                (t :: LFMBTree Word64 BufferedRef TestVal) <- fromAscList ws
                expect <- liftIO $ newIORef $ Just $ take n $ reverse (zip [0 ..] ws)
                let f i vref = do
                        expect' <- liftIO $ readIORef expect
                        case expect' of
                            Just ((i', v') : rest) -> do
                                liftIO $ i `shouldBe` i'
                                v <- refLoad vref
                                liftIO $ v `shouldBe` v'
                                liftIO $ writeIORef expect $ Just rest
                                return True
                            Just [] -> do
                                liftIO $ writeIORef expect Nothing
                                return False
                            Nothing -> error "Traversed too many elements"
                traverseWhileDescRef f t
                expect' <- liftIO $ readIORef expect
                if n == length ws
                    then liftIO $ expect' `shouldBe` Just []
                    else liftIO $ expect' `shouldBe` Nothing
            )

newtype StringValue = StringValue String
    deriving newtype (S.Serialize, Show, IsString, Eq)

instance (MonadBlobStore m) => BlobStorable m StringValue

instance (Monad m) => MHashableTo m H.Hash StringValue

instance HashableTo H.Hash StringValue where
    getHash val = H.hashLazy $ S.runPutLazy $ do
        S.put val

-- | Asserts "shapshots" of hashes to make sure they don't change.
snapshotTestHash :: IO ()
snapshotTestHash = do
    mbs <- newMemBlobStore
    flip
        runMemBlobStoreT
        mbs
        ( do
            -- Empty tree
            let emptyTree = empty :: LFMBTree Word64 HashedBufferedRef StringValue
            (h1 :: LFMBTreeHashV1) <- getHashM emptyTree
            liftIO $ show h1 `shouldBe` "c423f9e91ee218b2b5303485dd87a3093a653ddb9bdb839d30aa1924de1dbf05"

            -- Tree with values A, B, C
            simpleTree <- foldM (\acc v -> snd <$> append v acc) (empty :: LFMBTree Word64 HashedBufferedRef StringValue) ["A", "B", "C"]
            (h2 :: LFMBTreeHashV1) <- getHashM simpleTree
            liftIO $ show h2 `shouldBe` "b9cac19f6048ef301f586e7e0faa6c08b6012d4b100703eef5dc1fcb26c1ecd5"
        )

-- | Load trees from storage fixtures, to make sure we stay compatible.
fixtureTestLoad :: IO ()
fixtureTestLoad = do
    mbs1 <- newMemBlobStoreWithBytes $ fromRight undefined $ B16.decode "00000000000000080000000000000000"
    flip
        runMemBlobStoreT
        mbs1
        ( do
            -- Empty tree
            (emptyTree :: LFMBTree Word64 HashedBufferedRef StringValue) <-
                loadDirect $ BlobRef 0
            liftIO $ size emptyTree `shouldBe` 0
        )
    mbs2 <- newMemBlobStoreWithBytes $ fromRight undefined $ B16.decode "0000000000000009000000000000000141000000000000000900000000000000000000000000000000090000000000000001420000000000000009000000000000000022000000000000001901000000000000000000000000000000110000000000000033000000000000000900000000000000014300000000000000090000000000000000650000000000000021000000000000000301000000000000000100000000000000440000000000000076"
    flip
        runMemBlobStoreT
        mbs2
        ( do
            -- Tree with values A, B, C
            (simpleTree :: LFMBTree Word64 HashedBufferedRef StringValue) <-
                loadDirect $ BlobRef 135
            liftIO $ size simpleTree `shouldBe` 3
            val0 <- lookup 0 simpleTree
            liftIO $ val0 `shouldBe` Just "A"
            val1 <- lookup 1 simpleTree
            liftIO $ val1 `shouldBe` Just "B"
            val2 <- lookup 2 simpleTree
            liftIO $ val2 `shouldBe` Just "C"
        )

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
        it
            "snapshotTestHash"
            snapshotTestHash
        it
            "fixtureTestLoad"
            fixtureTestLoad
        it "testTraverseWhileDescRef" $ property testTraverseWhileDescRef
