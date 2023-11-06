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
import Data.Maybe (isJust, isNothing)
import System.IO.Temp
import System.Random
import Test.HUnit
import Test.Hspec

import Concordium.ID.Types (randomAccountAddress)
import Concordium.Logger
import Concordium.Types

import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap

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

-- | Test for looking up both via equivalence class and by exactness.
--  Precondition: The provided @AccountAddress@ MUST be the canonical address,
--  and it should be present in the underlying store.
--  The equivalence lookup always goes through an alias.
testDoLookup :: (MonadIO m, LMDBAccountMap.MonadAccountMapStore m) => AccountAddress -> m (Maybe AccountIndex)
testDoLookup accAddr = do
    res1 <- LMDBAccountMap.lookupAccountIndexViaExactness accAddr
    res2 <- LMDBAccountMap.lookupAccountIndexViaEquivalence (accountAddressEmbed $ createAlias accAddr 42)
    liftIO $ assertEqual "Results should be the same" res1 res2
    return res1

-- | Test that a database is not initialized.
testCheckNotInitialized :: Assertion
testCheckNotInitialized = runTest "notinitialized" $ do
    isInitialized <- LMDBAccountMap.isInitialized
    liftIO $ assertBool "database should not have been initialized" $ not isInitialized

-- | Test that a database is initialized.
testCheckDbInitialized :: Assertion
testCheckDbInitialized = runTest "initialized" $ do
    -- initialize the database
    void $ LMDBAccountMap.insertAccounts [dummyPair 1]
    isInitialized <- LMDBAccountMap.isInitialized
    liftIO $ assertBool "database should have been initialized" isInitialized

-- | Test that inserts a set of accounts and afterwards asserts that they are present.
testInsertAndLookupAccounts :: Assertion
testInsertAndLookupAccounts = runTest "insertandlookups" $ do
    let accounts = dummyPair <$> [1 .. 42]
    void $ LMDBAccountMap.insertAccounts accounts

    forM_ accounts $ \(accAddr, accIndex) -> do
        testDoLookup accAddr >>= \case
            Nothing -> liftIO $ assertFailure $ "account was not present " <> show accAddr <> " account index " <> show accIndex
            Just foundAccountIndex -> liftIO $ assertEqual "account indices should be the same" accIndex foundAccountIndex

-- | Test for looking up an account via an alias
testLookupAccountViaAlias :: Assertion
testLookupAccountViaAlias = runTest "lookupviaalias" $ do
    -- initialize the database
    void $ LMDBAccountMap.insertAccounts [acc]
    let alias = createAlias (fst acc) 42
    exactLookup <- isNothing <$> LMDBAccountMap.lookupAccountIndexViaExactness alias
    liftIO $ assertBool "Alias lookup should've failed" exactLookup
    LMDBAccountMap.lookupAccountIndexViaEquivalence (accountAddressEmbed alias) >>= \case
        Nothing -> liftIO $ assertFailure "account could not be looked up via alias"
        Just accIndex -> liftIO $ assertEqual "account indices should match" (snd acc) accIndex
  where
    acc = dummyPair 1

-- | Test for retrieving all accounts present in the LMDB store.
testGetAllAccounts :: Assertion
testGetAllAccounts = runTest "allaccounts" $ do
    -- initialize the database
    void $ LMDBAccountMap.insertAccounts $ dummyPair <$> [0 .. 42]
    void $ LMDBAccountMap.insertAccounts $ dummyPair <$> [42 .. 84]
    allAccounts <- LMDBAccountMap.getAllAccounts (AccountIndex 85)
    when (length allAccounts /= 85) $
        liftIO $
            assertFailure $
                "unexpected number of accounts: " <> (show . length) allAccounts <> " should be " <> show (85 :: Int)
    forM_ (dummyPair <$> [0 .. 84]) $ \(accAddr, _) -> do
        isPresent <- isJust <$> testDoLookup accAddr
        liftIO $ assertBool "account should be present" isPresent

tests :: Spec
tests = describe "AccountMap.LMDB" $ do
    it "Test checking db is not initialized" testCheckNotInitialized
    it "Test checking db is initialized" testCheckDbInitialized
    it "Test inserts and lookups" testInsertAndLookupAccounts
    it "Test getting all accounts" testGetAllAccounts
    it "Test looking up account via alias" testLookupAccountViaAlias
