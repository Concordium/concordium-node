{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.PersistentChainParameters (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data.P11 as P11
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Persistent.BlockState.Parameters as PCP
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as PU
import qualified Concordium.GlobalState.Persistent.Migration as Migration
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import qualified Concordium.Types.UpdateQueues as UQ
import Concordium.Types.Updates (AccessStructure (..))

import GlobalStateTests.BlockStateHelpers (dummySeedState, runTestBlockState)

-- | Run an action in the 'MemBlobStoreT' monad transformer from an empty store.
runWithNewMemBlobStore :: MemBlobStoreT IO a -> IO a
runWithNewMemBlobStore a = do
    mbs <- newMemBlobStore
    runMemBlobStoreT a mbs

-- | The historical chain-parameter layout used for persistent storage before
-- node-owned persistent chain parameters were split from the public view.
--
-- This deliberately omits '_cpMaxLockDuration'.
newtype OldPersistentChainParametersLayout cpv = OldPersistentChainParametersLayout (ChainParameters' cpv)

-- | Serialize the historical persistent chain-parameter layout.
putOldPersistentChainParametersLayout :: forall cpv. (IsChainParametersVersion cpv) => S.Putter (ChainParameters' cpv)
putOldPersistentChainParametersLayout ChainParameters{..} = do
    withIsConsensusParametersVersionFor (chainParametersVersion @cpv) $ S.put _cpConsensusParameters
    S.put _cpExchangeRates
    putCooldownParameters _cpCooldownParameters
    S.put _cpTimeParameters
    S.put _cpAccountCreationLimit
    S.put _cpRewardParameters
    S.put _cpFoundationAccount
    putPoolParameters _cpPoolParameters
    S.put _cpFinalizationCommitteeParameters
    S.put _cpValidatorScoreParameters

instance (MonadBlobStore m, IsChainParametersVersion cpv) => BlobStorable m (OldPersistentChainParametersLayout cpv) where
    storeUpdate (OldPersistentChainParametersLayout chainParameters) =
        return (putOldPersistentChainParametersLayout chainParameters, OldPersistentChainParametersLayout chainParameters)
    load = error "OldPersistentChainParametersLayout is only used for writing compatibility test blobs"

-- | Store bytes in the historical layout and load them as the new persistent type.
loadOldLayoutAsPersistent ::
    forall pv.
    (IsProtocolVersion pv) =>
    ChainParameters pv ->
    MemBlobStoreT IO (PCP.PersistentChainParameters pv)
loadOldLayoutAsPersistent chainParameters = do
    oldRef <- storeRef (OldPersistentChainParametersLayout chainParameters)
    loadRef (BlobRef (theBlobRef oldRef))

-- | Assert that old-layout bytes load as persistent chain parameters and convert
-- back to the public view without changing ordinary fields.
assertOldLayoutLoadsAsPersistent ::
    forall pv.
    (IsProtocolVersion pv) =>
    ChainParameters pv ->
    Assertion
assertOldLayoutLoadsAsPersistent chainParameters = runWithNewMemBlobStore $ do
    persistent <- loadOldLayoutAsPersistent @pv chainParameters
    let publicView = PCP.persistentChainParametersToChainParameters persistent
    liftIO $ assertEqual "old persistent layout should load as the new persistent type" chainParameters publicView

-- | Assert that hashing pre-P11 persistent chain parameters is unchanged from
-- hashing the historical persistent byte layout.
assertOldLayoutHashCompatible ::
    forall pv.
    (IsProtocolVersion pv) =>
    ChainParameters pv ->
    Assertion
assertOldLayoutHashCompatible chainParameters = runWithNewMemBlobStore $ do
    persistent <- loadOldLayoutAsPersistent @pv chainParameters
    persistentHash <- getHashM persistent
    let oldHash = H.hash (S.runPut (putOldPersistentChainParametersLayout chainParameters))
    liftIO $ assertEqual "persistent chain-parameter hash should match the historical layout hash" oldHash persistentHash

p10ChainParameters :: ChainParameters' 'ChainParametersV3
p10ChainParameters = dummyChainParameters' & cpMaxLockDuration .~ SomeParam Nothing

p11ChainParameters :: Duration -> ChainParameters' 'ChainParametersV3
p11ChainParameters duration = dummyChainParameters' & cpMaxLockDuration .~ SomeParam (Just duration)

p11ProtocolUpdateData :: Duration -> P11.ProtocolUpdateData
p11ProtocolUpdateData duration =
    P11.ProtocolUpdateData
        { P11.updateTokenParametersAccessStructure = AccessStructure (Set.singleton 0) 1,
          P11.updateMaxLockDuration = duration
        }

assertP10P11MigrationExposesMaxLockDuration :: Assertion
assertP10P11MigrationExposesMaxLockDuration = runWithNewMemBlobStore $ do
    let duration = Duration 12345
        migration = StateMigrationParametersP10ToP11 (P11.StateMigrationData (p11ProtocolUpdateData duration))
    persistent0 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'P10 p10ChainParameters
    migratedMaybe <- runMaybeT $ Migration.migrateChainParameters migration persistent0
    migrated <- case migratedMaybe of
        Nothing -> liftIO $ assertFailure "P10-to-P11 chain-parameter migration unexpectedly failed"
        Just migrated -> return migrated
    publicView <- PCP.persistentChainParametersToChainParametersM migrated
    liftIO $ assertEqual "P10-to-P11 migration should expose protocol-update maxLockDuration" (SomeParam (Just duration)) (publicView ^. cpMaxLockDuration)

assertP11InitialPersistentStateExposesMaxLockDuration :: Assertion
assertP11InitialPersistentStateExposesMaxLockDuration = runTestBlockState @'P11 $ do
    let duration = Duration 67890
    hpbs <-
        PBS.initialPersistentState @'P11
            (dummySeedState SP11)
            dummyCryptographicParameters
            []
            dummyIdentityProviders
            dummyArs
            (dummyKeyCollection @'AuthorizationsVersion3)
            (p11ChainParameters duration)
    bsp <- PBS.loadPBS (PBS.hpbsPointers hpbs)
    updates <- refLoad (PBS.bspUpdates bsp)
    basicUpdates <- PU.makeBasicUpdates updates
    liftIO $ assertEqual "P11 initial state should expose genesis maxLockDuration" (SomeParam (Just duration)) (UQ._currentParameters basicUpdates ^. cpMaxLockDuration)

assertP11InitialPersistentStateRequiresMaxLockDuration :: Assertion
assertP11InitialPersistentStateRequiresMaxLockDuration = do
    result <- try $ runTestBlockState @'P11 $ do
        hpbs <-
            PBS.initialPersistentState @'P11
                (dummySeedState SP11)
                dummyCryptographicParameters
                []
                dummyIdentityProviders
                dummyArs
                (dummyKeyCollection @'AuthorizationsVersion3)
                p10ChainParameters
        liftIO $ evaluate hpbs
    case result of
        Left (_ :: ErrorCall) -> return ()
        Right _ -> assertFailure "P11 initial persistent state should require maxLockDuration"

assertP11RoundtripExposesMaxLockDuration :: Assertion
assertP11RoundtripExposesMaxLockDuration = runWithNewMemBlobStore $ do
    let duration = Duration 42
    persistent0 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'P11 (p11ChainParameters duration)
    (hash0 :: H.Hash) <- getHashM persistent0
    persistent1 <- loadRef =<< storeRef persistent0
    (hash1 :: H.Hash) <- getHashM persistent1
    persistent2 <- cache persistent1
    publicView <- PCP.persistentChainParametersToChainParametersM persistent2
    liftIO $ do
        assertEqual "P11 maxLockDuration should survive store/load/cache" (SomeParam (Just duration)) (publicView ^. cpMaxLockDuration)
        assertEqual "P11 persistent chain-parameter hash should survive store/load" hash0 hash1

assertP11HashIncludesExternalChainParameters :: Assertion
assertP11HashIncludesExternalChainParameters = runWithNewMemBlobStore $ do
    persistent1 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'P11 (p11ChainParameters (Duration 1))
    persistent2 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'P11 (p11ChainParameters (Duration 2))
    (hash1 :: H.Hash) <- getHashM persistent1
    (hash2 :: H.Hash) <- getHashM persistent2
    liftIO $ assertBool "P11 persistent chain-parameter hash should include external maxLockDuration" (hash1 /= hash2)

assertP11ConstructionRequiresMaxLockDuration :: Assertion
assertP11ConstructionRequiresMaxLockDuration = do
    result <- try $ runWithNewMemBlobStore $ do
        persistent <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'P11 p10ChainParameters
        liftIO $ evaluate persistent
    case result of
        Left (_ :: ErrorCall) -> return ()
        Right _ -> assertFailure "P11 persistent chain-parameter construction should require maxLockDuration"

tests :: Spec
tests = describe "GlobalStateTests.PersistentChainParameters" $ do
    it "loads old CPV0 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'P1 dummyChainParameters'
    it "loads old CPV1 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'P4 dummyChainParameters'
    it "loads old CPV2 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'P6 dummyChainParameters'
    it "loads old pre-P11 CPV3 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'P10 p10ChainParameters
    it "keeps old pre-P11 persistent chain-parameter hashes unchanged" $ do
        assertOldLayoutHashCompatible @'P1 dummyChainParameters'
        assertOldLayoutHashCompatible @'P4 dummyChainParameters'
        assertOldLayoutHashCompatible @'P6 dummyChainParameters'
        assertOldLayoutHashCompatible @'P10 p10ChainParameters
    it "migrates P10 chain parameters to P11 with protocol-update maxLockDuration" $
        assertP10P11MigrationExposesMaxLockDuration
    it "initializes P11 persistent state with genesis maxLockDuration" $
        assertP11InitialPersistentStateExposesMaxLockDuration
    it "rejects P11 initial persistent state without maxLockDuration" $
        assertP11InitialPersistentStateRequiresMaxLockDuration
    it "stores, loads, caches, hashes, and exposes P11 external maxLockDuration" $
        assertP11RoundtripExposesMaxLockDuration
    it "includes P11 external maxLockDuration in the persistent chain-parameter hash" $
        assertP11HashIncludesExternalChainParameters
    it "rejects P11 persistent construction without maxLockDuration" $
        assertP11ConstructionRequiresMaxLockDuration
