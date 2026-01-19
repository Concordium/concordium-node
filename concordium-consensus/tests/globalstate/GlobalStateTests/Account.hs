{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GlobalStateTests.Account where

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.Types.HashableTo
import Concordium.Types.Tokens
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Serialize
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

-- Blobstore testing infrastructure
-----------------------------------

-- | Run an action in the 'MemBlobStoreT' monad transformer from an empty store.
runBlobStore :: MemBlobStoreT IO a -> IO a
runBlobStore a = do
    mbs <- newMemBlobStore
    runMemBlobStoreT a mbs

-- Arbitrary data
-----------------

instance Arbitrary BS.ByteString where
    arbitrary = sized $ \n -> do
        len <- choose (0, n)
        bytes <- vectorOf len arbitrary
        return $ BS.pack bytes

    shrink bs
        | BS.null bs = []
        | otherwise = BS.pack <$> shrink (BS.unpack bs)

instance Arbitrary SBS.ShortByteString where
    arbitrary = SBS.toShort <$> arbitrary
    shrink sbs = SBS.toShort <$> shrink (SBS.fromShort sbs)

arbitraryTokenAccountState :: Gen TokenAccountState
arbitraryTokenAccountState = do
    balance <- TokenRawAmount <$> arbitrary
    return $ TokenAccountState{tasBalance = balance}

arbitraryInMemoryTokenStateTable :: Gen InMemoryTokenStateTable
arbitraryInMemoryTokenStateTable = sized $ \n -> do
    k <- choose (0, n)
    kvs <- vectorOf k $ (,) <$> (TokenIndex <$> arbitrary) <*> arbitraryTokenAccountState
    return $ InMemoryTokenStateTable{inMemoryTokenStateTable = Map.fromList kvs}

-- | Helper functions to translate between in-memory to persistent account state table.
inMemoryToPersistentTST :: (MonadIO m) => InMemoryTokenStateTable -> m TokenAccountStateTable
inMemoryToPersistentTST InMemoryTokenStateTable{..} =
    TokenAccountStateTable <$> traverse makeHashedBufferedRef inMemoryTokenStateTable

-- | Helper functions to translate between persistent to in-memory account state table.
persistentToInMemoryTST :: (MonadBlobStore m) => TokenAccountStateTable -> m InMemoryTokenStateTable
persistentToInMemoryTST TokenAccountStateTable{..} = InMemoryTokenStateTable <$> traverse refLoad tokenAccountStateTable

-- TokenAccountStateTable tests
-------------------------------

-- | Test the BlobStorable instance for TokenAccountStateTable.
-- Verifies that a TokenAccountStateTable can be serialized to blob storage and
-- then deserialized back to its original state without any data loss or corruption.
testBlobStorableTokenAccountState :: Word -> Spec
testBlobStorableTokenAccountState lvl =
    it "BlobStorable instance for TokenAccountStateTable" $
        withMaxSuccess (1000 * fromIntegral lvl) $
            property $ do
                inMemoryTST <- arbitraryInMemoryTokenStateTable
                return $ ioProperty $ runBlobStore $ do
                    tst <- inMemoryToPersistentTST inMemoryTST
                    (ref, _tst) <- storeUpdateDirect tst
                    tst' <- loadDirect ref
                    inMemoryTST' <- persistentToInMemoryTST tst'
                    liftIO $ assertEqual "stored then loaded token account state table should be equal" inMemoryTST' inMemoryTST

-- | Verify hash consistency between in-memory and persisted TokenStateTable.
testInMemoryEqualsPersistentHash :: Word -> Spec
testInMemoryEqualsPersistentHash lvl =
    it "Hash of in-memory token account state table equals hash of persisted token account state table" $
        withMaxSuccess (1000 * fromIntegral lvl) $
            property $ do
                inMemoryTST <- arbitraryInMemoryTokenStateTable
                return $ ioProperty $ runBlobStore $ do
                    let h :: TokenStateTableHash = getHash inMemoryTST
                    tst <- inMemoryToPersistentTST inMemoryTST
                    h' :: TokenStateTableHash <- getHashM tst
                    liftIO $ assertEqual "Hash of in-memory/persisted account state table should be equal" h h'

-- | Verify consistency between serialization/deserialization of TokenAccountState.
testSerializeTokenAccountState :: Word -> Spec
testSerializeTokenAccountState lvl =
    it "Serialize, then deserialize of TokenAccountState results in identity" $
        withMaxSuccess (10000 * fromIntegral lvl) $
            forAll arbitraryTokenAccountState $ \state -> property $ do
                let stateOrErr = runGet get $ runPut $ put state
                case stateOrErr of
                    Left err -> assertFailure $ "Deserialization failed: " ++ show err
                    Right state' ->
                        assertEqual "serialize . deserialize for token account state should be the identity" state state'

tests :: Word -> Spec
tests lvl = do
    describe "GlobalStateTests.Account" $
        do
            testSerializeTokenAccountState lvl
            -- tests that need access to an in-memory blockstate
            testInMemoryEqualsPersistentHash lvl
            testBlobStorableTokenAccountState lvl
