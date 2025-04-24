{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GlobalStateTests.Account where

import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Persistent.Account as PA
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as M
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Exception (bracket)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Serialize
import System.FilePath
import System.IO.Temp
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

type PV = 'P9

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

arbitraryTokenModuleState :: Gen (Map.Map TokenStateKey TokenStateValue)
arbitraryTokenModuleState = sized $ \n -> do
    k <- choose (0, n)
    kvs <- vectorOf k $ (,) <$> arbitrary <*> arbitrary
    return $ Map.fromList kvs

arbitraryTokenAccountState :: Gen TokenAccountState
arbitraryTokenAccountState = do
    balance <- TokenRawAmount <$> arbitrary
    state <- arbitraryTokenModuleState
    return $ TokenAccountState{tasBalance = balance, tasModuleState = state}

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
testBlobStorableTokenAccountState :: Word -> SpecWith (PersistentBlockStateContext PV)
testBlobStorableTokenAccountState lvl =
    it "BlobStorable instance for TokenAccountStateTable" $ \blobstore ->
        withMaxSuccess (1000 * fromIntegral lvl) $
            property $ do
                inMemoryTST <- arbitraryInMemoryTokenStateTable
                return $ ioProperty $ flip runBlobStoreT blobstore $ do
                    tst <- inMemoryToPersistentTST inMemoryTST
                    (ref, _tst) <- storeUpdateDirect tst
                    tst' <- loadDirect ref
                    inMemoryTST' <- persistentToInMemoryTST tst'
                    liftIO $ assertEqual "stored then loaded token account state table should be equal" inMemoryTST' inMemoryTST

-- | Verify hash consistency between in-memory and persisted TokenStateTable.
testInMemoryEqualsPersistentHash :: Word -> SpecWith (PersistentBlockStateContext PV)
testInMemoryEqualsPersistentHash lvl =
    it "Hash of in-memory token account state table equals hash of persisted token account state table" $ \blobstore ->
        withMaxSuccess (1000 * fromIntegral lvl) $
            property $ do
                inMemoryTST <- arbitraryInMemoryTokenStateTable
                return $ ioProperty $ flip runBlobStoreT blobstore $ do
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
            -- tests that need access to a temporary blockstate
            around
                ( \kont ->
                    withTempDirectory "." "blockstate" $ \dir ->
                        bracket
                            ( do
                                pbscBlobStore <- createBlobStore (dir </> "blockstate.dat")
                                pbscAccountCache <- PA.newAccountCache 100
                                pbscModuleCache <- M.newModuleCache 100
                                pbscAccountMap <- LMDBAccountMap.openDatabase (dir </> "accountmap")
                                return PersistentBlockStateContext{..}
                            )
                            ( \PersistentBlockStateContext{..} -> do
                                closeBlobStore pbscBlobStore
                                LMDBAccountMap.closeDatabase pbscAccountMap
                            )
                            kont
                )
                $ do
                    testBlobStorableTokenAccountState lvl
                    testInMemoryEqualsPersistentHash lvl
