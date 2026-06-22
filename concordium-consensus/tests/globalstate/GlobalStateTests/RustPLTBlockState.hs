{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the Rust-maintained PLT block state primarily to smoke test
-- the FFI interface..
module GlobalStateTests.RustPLTBlockState (tests) where

import Control.Monad.IO.Class
import Test.HUnit
import Test.Hspec

import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as RustPLT

-- | Run an action in the 'MemBlobStoreT' monad transformer from an empty store.
runWithNewMemBlobStore :: MemBlobStoreT IO a -> IO a
runWithNewMemBlobStore a = do
    mbs <- newMemBlobStore
    runMemBlobStoreT a mbs

-- | Test store, load, cache and hash operations
testStoreLoadHashCache :: Assertion
testStoreLoadHashCache = runWithNewMemBlobStore $ do
    -- Create empty state
    (state :: ForeignPLTBlockStatePtr 'P11) <- RustPLT.empty
    (hashBefore :: ProtocolLevelTokensHash) <- getHashM state
    -- Store and load state
    (loaded :: ForeignPLTBlockStatePtr 'P11) <- loadRef =<< storeRef state
    (hashAfter :: ProtocolLevelTokensHash) <- getHashM loaded
    liftIO $ assertEqual "Hash should be preserved across store/load" hashBefore hashAfter
    -- Cache state
    (_cached :: ForeignPLTBlockStatePtr 'P11) <- cache state
    return ()

-- | Test migrate state.
testMigrate :: Assertion
testMigrate = do
    sourceStore <- newMemBlobStore
    flip runMemBlobStoreT sourceStore $ do
        -- Create empty
        (state :: ForeignPLTBlockStatePtr 'P10) <- RustPLT.empty

        -- Migrate
        targetStore <- liftIO newMemBlobStore
        flip runMemBlobStoreT targetStore $ do
            (_migrated :: ForeignPLTBlockStatePtr 'P11) <- migrate state
            return ()

tests :: Spec
tests = describe "GlobalStateTests.RustPLTBlockState" $ do
    it "storeLoadHashCache" testStoreLoadHashCache
    it "migrate" testMigrate
