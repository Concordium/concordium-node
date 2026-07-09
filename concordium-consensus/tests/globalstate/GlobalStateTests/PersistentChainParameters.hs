{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.PersistentChainParameters (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad.IO.Class
import qualified Data.Serialize as S
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.Parameters as PCP
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

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
    forall cpv auv.
    (IsChainParametersVersion cpv, IsAuthorizationsVersion auv) =>
    ChainParameters' cpv ->
    MemBlobStoreT IO (PCP.PersistentChainParameters' cpv auv)
loadOldLayoutAsPersistent chainParameters = do
    oldRef <- storeRef (OldPersistentChainParametersLayout chainParameters)
    loadRef (BlobRef (theBlobRef oldRef))

-- | Assert that old-layout bytes load as persistent chain parameters and convert
-- back to the public view without changing ordinary fields.
assertOldLayoutLoadsAsPersistent ::
    forall cpv auv.
    (IsChainParametersVersion cpv, IsAuthorizationsVersion auv) =>
    ChainParameters' cpv ->
    Assertion
assertOldLayoutLoadsAsPersistent chainParameters = runWithNewMemBlobStore $ do
    persistent <- loadOldLayoutAsPersistent @cpv @auv chainParameters
    let publicView = PCP.persistentChainParametersToChainParameters persistent
    liftIO $ assertEqual "old persistent layout should load as the new persistent type" chainParameters publicView

-- | Assert that hashing pre-P11 persistent chain parameters is unchanged from
-- hashing the historical persistent byte layout.
assertOldLayoutHashCompatible ::
    forall cpv auv.
    (IsChainParametersVersion cpv, IsAuthorizationsVersion auv) =>
    ChainParameters' cpv ->
    Assertion
assertOldLayoutHashCompatible chainParameters = runWithNewMemBlobStore $ do
    persistent <- loadOldLayoutAsPersistent @cpv @auv chainParameters
    persistentHash <- getHashM persistent
    let oldHash = H.hash (S.runPut (putOldPersistentChainParametersLayout chainParameters))
    liftIO $ assertEqual "persistent chain-parameter hash should match the historical layout hash" oldHash persistentHash

p10ChainParameters :: ChainParameters' 'ChainParametersV3
p10ChainParameters = dummyChainParameters' & cpMaxLockDuration .~ SomeParam Nothing

p11ChainParameters :: Duration -> ChainParameters' 'ChainParametersV3
p11ChainParameters duration = dummyChainParameters' & cpMaxLockDuration .~ SomeParam (Just duration)

assertP11RoundtripExposesMaxLockDuration :: Assertion
assertP11RoundtripExposesMaxLockDuration = runWithNewMemBlobStore $ do
    let duration = Duration 42
    persistent0 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'ChainParametersV3 @'AuthorizationsVersion3 (p11ChainParameters duration)
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
    persistent1 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'ChainParametersV3 @'AuthorizationsVersion3 (p11ChainParameters (Duration 1))
    persistent2 <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'ChainParametersV3 @'AuthorizationsVersion3 (p11ChainParameters (Duration 2))
    (hash1 :: H.Hash) <- getHashM persistent1
    (hash2 :: H.Hash) <- getHashM persistent2
    liftIO $ assertBool "P11 persistent chain-parameter hash should include external maxLockDuration" (hash1 /= hash2)

assertP11ConstructionRequiresMaxLockDuration :: Assertion
assertP11ConstructionRequiresMaxLockDuration = do
    result <- try $ runWithNewMemBlobStore $ do
        persistent <- PCP.makePersistentChainParameters @(MemBlobStoreT IO) @'ChainParametersV3 @'AuthorizationsVersion3 p10ChainParameters
        liftIO $ evaluate persistent
    case result of
        Left (_ :: ErrorCall) -> return ()
        Right _ -> assertFailure "P11 persistent chain-parameter construction should require maxLockDuration"

tests :: Spec
tests = describe "GlobalStateTests.PersistentChainParameters" $ do
    it "loads old CPV0 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'ChainParametersV0 @'AuthorizationsVersion0 dummyChainParameters'
    it "loads old CPV1 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'ChainParametersV1 @'AuthorizationsVersion1 dummyChainParameters'
    it "loads old CPV2 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'ChainParametersV2 @'AuthorizationsVersion1 dummyChainParameters'
    it "loads old pre-P11 CPV3 persistent bytes as node-owned persistent chain parameters" $
        assertOldLayoutLoadsAsPersistent @'ChainParametersV3 @'AuthorizationsVersion2 p10ChainParameters
    it "keeps old pre-P11 persistent chain-parameter hashes unchanged" $ do
        assertOldLayoutHashCompatible @'ChainParametersV0 @'AuthorizationsVersion0 dummyChainParameters'
        assertOldLayoutHashCompatible @'ChainParametersV1 @'AuthorizationsVersion1 dummyChainParameters'
        assertOldLayoutHashCompatible @'ChainParametersV2 @'AuthorizationsVersion1 dummyChainParameters'
        assertOldLayoutHashCompatible @'ChainParametersV3 @'AuthorizationsVersion2 p10ChainParameters
    it "stores, loads, caches, hashes, and exposes P11 external maxLockDuration" $
        assertP11RoundtripExposesMaxLockDuration
    it "includes P11 external maxLockDuration in the persistent chain-parameter hash" $
        assertP11HashIncludesExternalChainParameters
    it "rejects P11 persistent construction without maxLockDuration" $
        assertP11ConstructionRequiresMaxLockDuration
