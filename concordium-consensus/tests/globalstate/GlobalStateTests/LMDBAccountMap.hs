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
import Data.Maybe (isJust)
import System.IO.Temp
import System.Random

import Concordium.ID.Types (randomAccountAddress)
import Concordium.Logger
import Concordium.Types

import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap

import Test.HUnit
import Test.Hspec

-- | Create a pair consisting of an account address and an account index based on the provided seed.
dummyPair :: Int -> (AccountAddress, AccountIndex)
dummyPair seed = (fst $ randomAccountAddress (mkStdGen seed), AccountIndex $ fromIntegral seed)

-- | Helper function for running a test in a context which has access to a temporary lmdb store.
runTest ::
    String ->
    LMDBAccountMap.AccountMapStoreMonad (ReaderT LMDBAccountMap.DatabaseHandlers LogIO) a ->
    IO a
runTest dirName action = withTempDirectory "" dirName $ \path ->
    bracket
        (LMDBAccountMap.makeDatabaseHandlers path False :: IO LMDBAccountMap.DatabaseHandlers)
        LMDBAccountMap.closeDatabase
        (\dbh -> runSilentLogger $ runReaderT (LMDBAccountMap.runAccountMapStoreMonad action) dbh)

-- | Test that a database is not initialized.
testCheckNotInitialized :: Assertion
testCheckNotInitialized = runTest "notinitialized" $ do
    isInitialized <- LMDBAccountMap.isInitialized
    liftIO $ assertBool "database should not have been initialized" $ not isInitialized

-- | Test that a database is initialized.
testCheckDbInitialized :: Assertion
testCheckDbInitialized = runTest "initialized" $ do
    -- initialize the database
    void $ LMDBAccountMap.insertAccount [dummyPair 1]
    isInitialized <- LMDBAccountMap.isInitialized
    liftIO $ assertBool "database should have been initialized" isInitialized

-- | Test that inserts a set of accounts and afterwards asserts that they are present.
testInsertAndLookupAccounts :: Assertion
testInsertAndLookupAccounts = runTest "insertandlookups" $ do
    let accounts = dummyPair <$> [1 .. 42]
    void $ LMDBAccountMap.insertAccount accounts

    forM_ accounts $ \(accAddr, accIndex) -> do
        LMDBAccountMap.lookupAccountIndex accAddr >>= \case
            Nothing -> liftIO $ assertFailure $ "account was not present " <> show accAddr <> " account index " <> show accIndex
            Just foundAccountIndex -> liftIO $ assertEqual "account indices should be the same" accIndex foundAccountIndex

-- | Test for looking up an account via an alias
testLookupAccountViaAlias :: Assertion
testLookupAccountViaAlias = runTest "lookupviaalias" $ do
    -- initialize the database
    void $ LMDBAccountMap.insertAccount [acc]
    LMDBAccountMap.lookupAccountIndex (createAlias (fst acc) 42) >>= \case
        Nothing -> liftIO $ assertFailure "account could not be looked up via alias"
        Just accIndex -> liftIO $ assertEqual "account indices should match" (snd acc) accIndex
  where
    acc = dummyPair 1

-- | Test for retrieving all accounts present in the LMDB store.
testGetAllAccounts :: Assertion
testGetAllAccounts = runTest "allaccounts" $ do
    -- initialize the database
    void $ LMDBAccountMap.insertAccount $ dummyPair <$> [0 .. 42]
    void $ LMDBAccountMap.insertAccount $ dummyPair <$> [42 .. 84]
    allAccounts <- LMDBAccountMap.getAllAccounts
    when (length allAccounts /= 85) $
        liftIO $
            assertFailure $
                "unexpected number of accounts: " <> (show . length) allAccounts <> " should be " <> show (85 :: Int)
    forM_ (dummyPair <$> [0 .. 84]) $ \(accAddr, _) -> do
        isPresent <- isJust <$> LMDBAccountMap.lookupAccountIndex accAddr
        liftIO $ assertBool "account should be present" isPresent

-- | Test that accounts can be rolled back i.e. deleted from the LMDB store and that
--  the metadata is updated also.
testRollback :: Assertion
testRollback = runTest "rollback" $ do
    -- initialize the database.
    void $ LMDBAccountMap.insertAccount [dummyPair 1]
    void $ LMDBAccountMap.insertAccount [dummyPair 2]
    -- roll back one block.
    LMDBAccountMap.lookupAccountIndex (fst $ dummyPair 2) >>= \case
        Nothing -> liftIO $ assertFailure "account should be present"
        Just _ -> do
            void $ LMDBAccountMap.unsafeRollback [fst $ dummyPair 2]
            LMDBAccountMap.lookupAccountIndex (fst $ dummyPair 2) >>= \case
                Just _ -> liftIO $ assertFailure "account should have been deleted"
                Nothing ->
                    LMDBAccountMap.lookupAccountIndex (fst $ dummyPair 1) >>= \case
                        Nothing -> liftIO $ assertFailure "Accounts from first block should still remain in the lmdb store"
                        Just accIdx -> liftIO $ assertEqual "The account index of the first account should be the same" (snd $ dummyPair 1) accIdx

tests :: Spec
tests = describe "AccountMap.LMDB" $ do
    it "Test checking db is not initialized" testCheckNotInitialized
    it "Test checking db is initialized" testCheckDbInitialized
    it "Test inserts and lookups" testInsertAndLookupAccounts
    it "Test getting all accounts" testGetAllAccounts
    it "Test looking up account via alias" testLookupAccountViaAlias
    it "Test rollback accounts" testRollback
