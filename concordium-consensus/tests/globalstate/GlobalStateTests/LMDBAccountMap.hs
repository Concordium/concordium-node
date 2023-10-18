{-# LANGUAGE OverloadedStrings #-}

--  Tests of the LMDB account map related operations.

-- | Tests for the LMDB account map
--  This module tests the following:
--  * Accounts can be inserted.
--  * Accounts can be looked up.
--  * Accounts can be rolled back.
module GlobalStateTests.LMDBAccountMap where

import Control.Exception (bracket)
import Control.Monad.Reader
import System.IO.Temp
import System.Random
import Prelude hiding (lookup)

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types (randomAccountAddress)
import Concordium.Logger
import Concordium.Types

import Concordium.GlobalState.AccountMap.LMDB

import Test.HUnit
import Test.Hspec

-- | Create a pair consisting of an account address and an account index based on the provided seed.
dummyPair :: Int -> (AccountAddress, AccountIndex)
dummyPair seed = (fst $ randomAccountAddress (mkStdGen seed), AccountIndex $ fromIntegral seed)

-- | A dummy block hash
dummyBlockHash :: BlockHash
dummyBlockHash = BlockHash $ Hash.hash "a dummy block hash"

-- | Another dummy block hash
anotherDummyBlockHash :: BlockHash
anotherDummyBlockHash = BlockHash $ Hash.hash "another dummy block hash"

-- | Helper function for running a test in a context which has access to a temporary lmdb store.
runTest ::
    String ->
    AccountMapStoreMonad (ReaderT DatabaseHandlers LogIO) a ->
    IO a
runTest dirName action = withTempDirectory "" dirName $ \path ->
    bracket
        (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers))
        closeDatabase
        (\dbhandlers -> runSilentLogger $ runReaderT (runAccountMapStoreMonad action) dbhandlers)

-- | Test that a database is not initialized.
testCheckNotInitialized :: Assertion
testCheckNotInitialized = runTest "notinitialized" $ do
    dbh <- ask
    liftIO $ do
        mMetadata <- isInitialized dbh
        assertEqual "Database should not have been initialized" Nothing mMetadata

-- | Test that a database is initialized.
testCheckDbInitialized :: Assertion
testCheckDbInitialized = runTest "initialized" $ do
    -- initialize the database
    void $ insert dummyBlockHash (BlockHeight 1) [dummyPair 1]
    dbh <- ask
    liftIO $ do
        isInitialized dbh >>= \case
            Nothing -> assertFailure "database should have been initialized"
            Just (blockHash, blockHeight) -> liftIO $ do
                assertEqual "block hash should correspond to the one used when last inserting" dummyBlockHash blockHash
                assertEqual "block height should correspond to the one used when last inserting" (BlockHeight 1) blockHeight

-- | Test that inserts a set of accounts and afterwards asserts that they are present.
testInsertAndLookupAccounts :: Assertion
testInsertAndLookupAccounts = runTest "insertandlookups" $ do
    let accounts = [acc | acc <- dummyPair <$> [1 .. 42]]
    void $ insert dummyBlockHash (BlockHeight 1) accounts

    forM_ accounts $ \(accAddr, accIndex) -> do
        lookup accAddr >>= \case
            Nothing -> liftIO $ assertFailure $ "account was not present " <> show accAddr <> " account index " <> show accIndex
            Just foundAccountIndex -> liftIO $ assertEqual "account indices should be the same" accIndex foundAccountIndex

-- | Test that inserting twice will yield the most recent block.
testMetadataIsUpdated :: Assertion
testMetadataIsUpdated = runTest "metadataupdated" $ do
    -- initialize the database
    void $ insert dummyBlockHash (BlockHeight 1) [dummyPair 1]
    void $ insert anotherDummyBlockHash (BlockHeight 2) [dummyPair 2]
    dbh <- ask
    liftIO $ do
        isInitialized dbh >>= \case
            Nothing -> assertFailure "database should have been initialized"
            Just (blockHash, blockHeight) -> liftIO $ do
                assertEqual "block hash should correspond to the one used when last inserting" anotherDummyBlockHash blockHash
                assertEqual "block height should correspond to the one used when last inserting" (BlockHeight 2) blockHeight

-- | Test that accounts can be rolled back i.e. deleted from the LMDB store and that
--  the metadata is updated also.
testRollback :: Assertion
testRollback = runTest "rollback" $ do
    -- initialize the database.
    void $ insert dummyBlockHash (BlockHeight 1) [dummyPair 1]
    void $ insert anotherDummyBlockHash (BlockHeight 2) [dummyPair 2]
    -- roll back one block.
    lookup (fst $ dummyPair 2) >>= \case
        Nothing -> liftIO $ assertFailure "account should be present"
        Just _ -> do
            void $ unsafeRollback [(fst $ dummyPair 2)] dummyBlockHash (BlockHeight 1)
            lookup (fst $ dummyPair 2) >>= \case
                Just _ -> liftIO $ assertFailure "account should have been deleted"
                Nothing ->
                    lookup (fst $ dummyPair 1) >>= \case
                        Nothing -> liftIO $ assertFailure "Accounts from first block should still remain in the lmdb store"
                        Just accIdx -> liftIO $ assertEqual "The account index of the first account should be the same" (snd $ dummyPair 1) accIdx

tests :: Spec
tests = describe "AccountMap.LMDB" $ do
    it "Test checking db is not initialized" testCheckNotInitialized
    it "Test checking db is initialized" testCheckDbInitialized
    it "Test inserts and lookups" testInsertAndLookupAccounts
    it "Test metadata is updated when accounts are added" testMetadataIsUpdated
    it "Test rollback accounts" testRollback
