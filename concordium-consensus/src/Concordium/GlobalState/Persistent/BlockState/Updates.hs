{-# LANGUAGE BangPatterns, ScopedTypeVariables, OverloadedStrings, DataKinds #-}
{-# LANGUAGE UndecidableInstances, GADTs #-}
-- |Implementation of the chain update mechanism with persistent storage: https://concordium.gitlab.io/whitepapers/update-mechanism/main.pdf
module Concordium.GlobalState.Persistent.BlockState.Updates where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Serialize
import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Updates
import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.Types.UpdateQueues as UQ
import qualified Concordium.Types.IdentityProviders as IPS
import qualified Concordium.Types.AnonymityRevokers as ARS

-- |An update queue consists of pending future updates ordered by
-- the time at which they will take effect.
--
-- Queue items are stored as (hashed) references, but the queue itself
-- is stored as a sequence.  This choice is based on the idea that the
-- queue will ordinarily be very short (0 or 1 items), and we need to
-- update at either end of the queue (which creates problems for a
-- linked list representation).
data UpdateQueue e = UpdateQueue {
        -- |The next available sequence number for an update.
        uqNextSequenceNumber :: !UpdateSequenceNumber,
        -- |Pending updates, in ascending order of effective time.
        uqQueue :: !(Seq.Seq (TransactionTime, HashedBufferedRef (StoreSerialized e)))
    }

instance (MonadBlobStore m, Serialize e, MHashableTo m H.Hash e)
        => BlobStorable m (UpdateQueue e) where
    storeUpdate uq@UpdateQueue{..} = do
        l <- forM uqQueue $ \(t, r) -> do
            (pr, r') <- storeUpdate r
            return (put t >> pr, (t, r'))
        let putUQ = do
                put uqNextSequenceNumber
                putLength $ Seq.length l
                sequence_ $ fst <$> l
        let uq' = uq{uqQueue = snd <$> l}
        return (putUQ, uq')
    store uq = fst <$> storeUpdate uq
    load = do
        uqNextSequenceNumber <- get
        l <- getLength
        muqQueue <- Seq.replicateM l $ do
            t <- get
            e <- load
            return $ (t,) <$> e
        return $! do
            uqQueue <- sequence muqQueue
            return UpdateQueue{..}

instance (BlobStorable m e, Serialize e, MHashableTo m H.Hash e) => MHashableTo m H.Hash (UpdateQueue e) where
    getHashM UpdateQueue{..} = do
        q :: (Seq.Seq (TransactionTime, H.Hash)) <- mapM (\(tt, href) -> (tt,) <$> getHashM href) uqQueue
        return $! H.hash $ runPut $ do
            put uqNextSequenceNumber
            putLength $ length q
            mapM_ (\(t, h) -> put t >> put h) q

instance (BlobStorable m e, Serialize e, MHashableTo m H.Hash e) => Cacheable m (UpdateQueue e) where
    cache uq = do
        q <- mapM (\(t, h) -> (t,) <$> cache h) (uqQueue uq)
        return uq{uqQueue = q}

-- |Serialize an update queue in V0 format.
putUpdateQueueV0 :: (MonadBlobStore m, MonadPut m, MHashableTo m H.Hash e, Serialize e) => UpdateQueue e -> m ()
putUpdateQueueV0 UpdateQueue{..} = do
        sPut uqNextSequenceNumber
        forM_ uqQueue $ \(tt, vref) -> do
            v <- refLoad vref
            liftPut $ do
                putWord8 1
                put tt
                put v
        liftPut $ putWord8 0

-- |Update queue with no pending updates, and with the minimal next
-- sequence number.
emptyUpdateQueue :: UpdateQueue e
emptyUpdateQueue = UpdateQueue {
        uqNextSequenceNumber = minUpdateSequenceNumber,
        uqQueue = Seq.empty
    }

-- |Make a persistent update queue from a memory-only queue.
makePersistentUpdateQueue :: (MonadIO m) => UQ.UpdateQueue e -> m (UpdateQueue e)
makePersistentUpdateQueue UQ.UpdateQueue{..} = do
    let uqNextSequenceNumber = _uqNextSequenceNumber
    uqQueue <- Seq.fromList <$> forM _uqQueue (\(t, e) -> (t,) <$> makeHashedBufferedRef (StoreSerialized e))
    return UpdateQueue{..}

-- |Convert a persistent update queue to an in-memory one.
makeBasicUpdateQueue :: (MonadBlobStore m, MHashableTo m H.Hash (StoreSerialized e), Serialize e) => UpdateQueue e -> m (UQ.UpdateQueue e)
makeBasicUpdateQueue UpdateQueue{..} = do
    let _uqNextSequenceNumber = uqNextSequenceNumber
    _uqQueue <- toList <$> forM uqQueue (\(t, e) -> (t,) . unStoreSerialized <$> refLoad e)
    return UQ.UpdateQueue{..}

-- |Convert a persistent update queue to an in-memory one.
makeBasicUpdateQueueHashed :: (MonadBlobStore m, MHashableTo m H.Hash (StoreSerialized e), Serialize e) => UpdateQueue e -> m (UQ.UpdateQueue (Hashed e))
makeBasicUpdateQueueHashed UpdateQueue{..} = do
    let _uqNextSequenceNumber = uqNextSequenceNumber
    _uqQueue <- toList <$> forM uqQueue (\(t, e) -> do
            v <- unStoreSerialized <$> refLoad e
            h <- getHashM e
            return (t, Hashed v h))
    return UQ.UpdateQueue{..}

-- |Add an update event to an update queue, incrementing the sequence number.
-- Any updates in the queue with later or equal effective times are removed
-- from the queue.
enqueue :: (MonadIO m, Reference m ref (UpdateQueue e))
    => TransactionTime -> e -> ref (UpdateQueue e) -> m (ref (UpdateQueue e))
enqueue !t !e q = do
        UpdateQueue{..} <- refLoad q
        eref <- makeHashedBufferedRef (StoreSerialized e)
        -- Existing queued updates are only kept if their effective
        -- timestamp is lower than the new update.
        let !surviving = Seq.takeWhileL ((< t) . fst) uqQueue
        refMake $ UpdateQueue {
                uqNextSequenceNumber = uqNextSequenceNumber + 1,
                uqQueue = surviving Seq.|> (t, eref)
            }

-- |Clear all pending updates from a queue.
clearQueue :: (MonadIO m, Reference m ref (UpdateQueue e))
    => ref (UpdateQueue e) -> m (ref (UpdateQueue e))
clearQueue q = do
        UpdateQueue{..} <- refLoad q
        refMake $ UpdateQueue {
                uqNextSequenceNumber = uqNextSequenceNumber,
                uqQueue = Seq.empty
            }

-- |Load the all pending updates from a queue.
loadQueue :: (Reference m ref (UpdateQueue t), MonadBlobStore m, Serialize t, MHashableTo m H.Hash t)
    => ref (UpdateQueue t)
    -> m [(TransactionTime, t)]
loadQueue q = do
        UpdateQueue{..} <- refLoad q
        mapM (\(a,b) -> (a,) . unStoreSerialized <$> refLoad b) (toList uqQueue)

-- |Update queues for all on-chain update types.
data PendingUpdates (cpv :: ChainParametersVersion) = PendingUpdates {
        -- |Updates to the root keys.
        pRootKeysUpdateQueue :: !(HashedBufferedRef (UpdateQueue (HigherLevelKeys RootKeysKind))),
        -- |Updates to the level 1 keys.
        pLevel1KeysUpdateQueue :: !(HashedBufferedRef (UpdateQueue (HigherLevelKeys Level1KeysKind))),
        -- |Updates to the level 2 keys.
        pLevel2KeysUpdateQueue :: !(HashedBufferedRef (UpdateQueue (Authorizations cpv))),
        -- |Protocol updates.
        pProtocolQueue :: !(HashedBufferedRef (UpdateQueue ProtocolUpdate)),
        -- |Updates to the election difficulty parameter.
        pElectionDifficultyQueue :: !(HashedBufferedRef (UpdateQueue ElectionDifficulty)),
        -- |Updates to the euro:energy exchange rate.
        pEuroPerEnergyQueue :: !(HashedBufferedRef (UpdateQueue ExchangeRate)),
        -- |Updates to the GTU:euro exchange rate.
        pMicroGTUPerEuroQueue :: !(HashedBufferedRef (UpdateQueue ExchangeRate)),
        -- |Updates to the foundation account.
        pFoundationAccountQueue :: !(HashedBufferedRef (UpdateQueue AccountIndex)),
        -- |Updates to the mint distribution.
        pMintDistributionQueue :: !(HashedBufferedRef (UpdateQueue (MintDistribution cpv))),
        -- |Updates to the transaction fee distribution.
        pTransactionFeeDistributionQueue :: !(HashedBufferedRef (UpdateQueue TransactionFeeDistribution)),
        -- |Updates to the GAS rewards.
        pGASRewardsQueue :: !(HashedBufferedRef (UpdateQueue GASRewards)),
        -- |Updates to the baker minimum threshold
        pPoolParametersQueue :: !(HashedBufferedRef (UpdateQueue (PoolParameters cpv))),
        -- |Additions to the set of anonymity revokers
        pAddAnonymityRevokerQueue :: !(HashedBufferedRef (UpdateQueue ARS.ArInfo)),
        -- |Additions to the set of identity providers
        pAddIdentityProviderQueue :: !(HashedBufferedRef (UpdateQueue IPS.IpInfo)),
        -- |Updates cooldown parameters
        pCooldownParametersQueue :: !(HashedBufferedRefForCPV1 cpv (UpdateQueue (CooldownParameters 'ChainParametersV1))),
        -- |Updates time parameters.
        pTimeParametersQueue :: !(HashedBufferedRefForCPV1 cpv (UpdateQueue (TimeParameters 'ChainParametersV1)))
    }

instance
    (MonadBlobStore m, IsChainParametersVersion cpv) =>
    MHashableTo m H.Hash (PendingUpdates cpv)
    where
    getHashM PendingUpdates{..} = do
        hRootKeysUpdateQueue <- H.hashToByteString <$> getHashM pRootKeysUpdateQueue
        hLevel1KeysUpdateQueue <- H.hashToByteString <$> getHashM pLevel1KeysUpdateQueue
        hLevel2KeysUpdateQueue <- H.hashToByteString <$> getHashM pLevel2KeysUpdateQueue
        hProtocolQueue <- H.hashToByteString <$> getHashM pProtocolQueue
        hElectionDifficultyQueue <- H.hashToByteString <$> getHashM pElectionDifficultyQueue
        hEuroPerEnergyQueue <- H.hashToByteString <$> getHashM pEuroPerEnergyQueue
        hMicroGTUPerEuroQueue <- H.hashToByteString <$> getHashM pMicroGTUPerEuroQueue
        hFoundationAccountQueue <- H.hashToByteString <$> getHashM pFoundationAccountQueue
        hMintDistributionQueue <- H.hashToByteString <$> getHashM pMintDistributionQueue
        hTransactionFeeDistributionQueue <- H.hashToByteString <$> getHashM pTransactionFeeDistributionQueue
        hGASRewardsQueue <- H.hashToByteString <$> getHashM pGASRewardsQueue
        hPoolParametersQueue <- H.hashToByteString <$> getHashM pPoolParametersQueue
        hAddAnonymityRevokerQueue <- H.hashToByteString <$> getHashM pAddAnonymityRevokerQueue
        hAddIdentityProviderQueue <- H.hashToByteString <$> getHashM pAddIdentityProviderQueue
        hCooldownParametersQueue <- maybeForCPV1 (return mempty) (fmap H.hashToByteString . getHashM) pCooldownParametersQueue
        hTimeParametersQueue <- maybeForCPV1 (return mempty) (fmap H.hashToByteString . getHashM) pTimeParametersQueue
        return $! H.hash $
            hRootKeysUpdateQueue
            <> hLevel1KeysUpdateQueue
            <> hLevel2KeysUpdateQueue
            <> hProtocolQueue
            <> hElectionDifficultyQueue
            <> hEuroPerEnergyQueue
            <> hMicroGTUPerEuroQueue
            <> hFoundationAccountQueue
            <> hMintDistributionQueue
            <> hTransactionFeeDistributionQueue
            <> hGASRewardsQueue
            <> hPoolParametersQueue
            <> hAddAnonymityRevokerQueue
            <> hAddIdentityProviderQueue
            <> hCooldownParametersQueue
            <> hTimeParametersQueue

instance
    (MonadBlobStore m, IsChainParametersVersion cpv) =>
    BlobStorable m (PendingUpdates cpv)
    where
    storeUpdate PendingUpdates{..} = do
            (pRKQ, rkQ) <- storeUpdate pRootKeysUpdateQueue
            (pL1KQ, l1kQ) <- storeUpdate pLevel1KeysUpdateQueue
            (pL2KQ, l2kQ) <- storeUpdate pLevel2KeysUpdateQueue
            (pPrQ, prQ) <- storeUpdate pProtocolQueue
            (pEDQ, edQ) <- storeUpdate pElectionDifficultyQueue
            (pEPEQ, epeQ) <- storeUpdate pEuroPerEnergyQueue
            (pMGTUPEQ, mgtupeQ) <- storeUpdate pMicroGTUPerEuroQueue
            (putFoundationAccountQueue, newFoundationAccountQueue) <- storeUpdate pFoundationAccountQueue
            (putMintDistributionQueue, newMintDistributionQueue) <- storeUpdate pMintDistributionQueue
            (putTransactionFeeDistributionQueue, newTransactionFeeDistributionQueue) <- storeUpdate pTransactionFeeDistributionQueue
            (putGASRewardsQueue, newGASRewardsQueue) <- storeUpdate pGASRewardsQueue
            (putPoolParametersQueue, newPoolParametersQueue) <- storeUpdate pPoolParametersQueue
            (putAddAnonymityRevokerQueue, newAddAnonymityRevokerQueue) <- storeUpdate pAddAnonymityRevokerQueue
            (putAddIdentityProviderQueue, newAddIdentityProviderQueue) <- storeUpdate pAddIdentityProviderQueue
            (putCooldownParametersQueue, newCooldownParametersQueue) <- storeUpdate pCooldownParametersQueue
            (putTimeParametersQueue, newTimeParametersQueue) <- storeUpdate pTimeParametersQueue
            let newPU = PendingUpdates {
                    pRootKeysUpdateQueue = rkQ,
                    pLevel1KeysUpdateQueue = l1kQ,
                    pLevel2KeysUpdateQueue = l2kQ,
                    pProtocolQueue = prQ,
                    pElectionDifficultyQueue = edQ,
                    pEuroPerEnergyQueue = epeQ,
                    pMicroGTUPerEuroQueue = mgtupeQ,
                    pFoundationAccountQueue = newFoundationAccountQueue,
                    pMintDistributionQueue = newMintDistributionQueue,
                    pTransactionFeeDistributionQueue = newTransactionFeeDistributionQueue,
                    pGASRewardsQueue = newGASRewardsQueue,
                    pPoolParametersQueue = newPoolParametersQueue,
                    pAddAnonymityRevokerQueue = newAddAnonymityRevokerQueue,
                    pAddIdentityProviderQueue = newAddIdentityProviderQueue,
                    pCooldownParametersQueue = newCooldownParametersQueue,
                    pTimeParametersQueue = newTimeParametersQueue
                }
            let putPU = pRKQ >> pL1KQ >> pL2KQ >> pPrQ >> pEDQ >> pEPEQ >> pMGTUPEQ
                    >> putFoundationAccountQueue
                    >> putMintDistributionQueue
                    >> putTransactionFeeDistributionQueue
                    >> putGASRewardsQueue
                    >> putPoolParametersQueue
                    >> putAddAnonymityRevokerQueue
                    >> putAddIdentityProviderQueue
                    >> putCooldownParametersQueue
                    >> putTimeParametersQueue
            return (putPU, newPU)
    store pu = fst <$> storeUpdate pu
    load = do
        mRKQ <- label "Root keys update queue" load
        mL1KQ <- label "Level 1 keys update queue" load
        mL2KQ <- label "Level 2 keys update queue" load
        mPrQ <- label "Protocol update queue" load
        mEDQ <- label "Election difficulty update queue" load
        mEPEQ <- label "Euro per energy update queue" load
        mMGTUPEQ <- label "Micro GTU per Euro update queue" load
        mFoundationAccountQueue <- label "Foundation account update queue" load
        mMintDistributionQueue <- label "Mint distribution update queue" load
        mTransactionFeeDistributionQueue <- label "Transaction fee distribution update queue" load
        mGASRewardsQueue <- label "GAS rewards update queue" load
        mPoolParametersQueue <- label "Baker minimum threshold update queue" load
        mAddAnonymityRevokerQueue <- label "Add anonymity revoker update queue" load
        mAddIdentityProviderQueue <- label "Add identity provider update queue" load
        mCooldownParametersQueue <- label "Cooldown parameters update queue" load
        mTimeParametersQueue <- label "Time parameters update queue" load
        return $! do
            pRootKeysUpdateQueue <- mRKQ
            pLevel1KeysUpdateQueue <- mL1KQ
            pLevel2KeysUpdateQueue <- mL2KQ
            pProtocolQueue <- mPrQ
            pElectionDifficultyQueue <- mEDQ
            pEuroPerEnergyQueue <- mEPEQ
            pMicroGTUPerEuroQueue <- mMGTUPEQ
            pFoundationAccountQueue <- mFoundationAccountQueue
            pMintDistributionQueue <- mMintDistributionQueue
            pTransactionFeeDistributionQueue <- mTransactionFeeDistributionQueue
            pGASRewardsQueue <- mGASRewardsQueue
            pPoolParametersQueue <- mPoolParametersQueue
            pAddAnonymityRevokerQueue <- mAddAnonymityRevokerQueue
            pAddIdentityProviderQueue <- mAddIdentityProviderQueue
            pCooldownParametersQueue <- mCooldownParametersQueue
            pTimeParametersQueue <- mTimeParametersQueue
            return PendingUpdates{..}

instance
    (MonadBlobStore m, IsChainParametersVersion cpv) =>
    Cacheable m (PendingUpdates cpv)
    where
    cache PendingUpdates{..} =
        PendingUpdates
            <$> cache pRootKeysUpdateQueue
            <*> cache pLevel1KeysUpdateQueue
            <*> cache pLevel2KeysUpdateQueue
            <*> cache pProtocolQueue
            <*> cache pElectionDifficultyQueue
            <*> cache pEuroPerEnergyQueue
            <*> cache pMicroGTUPerEuroQueue
            <*> cache pFoundationAccountQueue
            <*> cache pMintDistributionQueue
            <*> cache pTransactionFeeDistributionQueue
            <*> cache pGASRewardsQueue
            <*> cache pPoolParametersQueue
            <*> cache pAddAnonymityRevokerQueue
            <*> cache pAddIdentityProviderQueue
            <*> cache pCooldownParametersQueue
            <*> cache pTimeParametersQueue

-- |Serialize the pending updates.
putPendingUpdatesV0
    :: forall m cpv
     . (MonadBlobStore m, MonadPut m, IsChainParametersVersion cpv)
    => PendingUpdates cpv -> m ()
putPendingUpdatesV0 PendingUpdates{..} = do
        putUpdateQueueV0 =<< refLoad pRootKeysUpdateQueue
        putUpdateQueueV0 =<< refLoad pLevel1KeysUpdateQueue
        putUpdateQueueV0 =<< refLoad pLevel2KeysUpdateQueue
        putUpdateQueueV0 =<< refLoad pProtocolQueue
        putUpdateQueueV0 =<< refLoad pElectionDifficultyQueue
        putUpdateQueueV0 =<< refLoad pEuroPerEnergyQueue
        putUpdateQueueV0 =<< refLoad pMicroGTUPerEuroQueue
        putUpdateQueueV0 =<< refLoad pFoundationAccountQueue
        putUpdateQueueV0 =<< refLoad pMintDistributionQueue
        putUpdateQueueV0 =<< refLoad pTransactionFeeDistributionQueue
        putUpdateQueueV0 =<< refLoad pGASRewardsQueue
        putUpdateQueueV0 =<< refLoad pPoolParametersQueue
        putUpdateQueueV0 =<< refLoad pAddAnonymityRevokerQueue
        putUpdateQueueV0 =<< refLoad pAddIdentityProviderQueue
        mapM_ (putUpdateQueueV0 <=< refLoad) pCooldownParametersQueue
        mapM_ (putUpdateQueueV0 <=< refLoad) pTimeParametersQueue

-- |Initial pending updates with empty queues.
emptyPendingUpdates
    :: forall m cpv
     . (MonadBlobStore m, IsChainParametersVersion cpv)
    => m (PendingUpdates cpv)
emptyPendingUpdates = PendingUpdates <$> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> justForCPV1A e <*> justForCPV1A e
    where
        e :: m (HashedBufferedRef (UpdateQueue a))
        e = makeHashedBufferedRef emptyUpdateQueue

-- |Construct a persistent 'PendingUpdates' from an in-memory one.
makePersistentPendingUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => UQ.PendingUpdates cpv -> m (PendingUpdates cpv)
makePersistentPendingUpdates UQ.PendingUpdates{..} = do
        pRootKeysUpdateQueue <- refMake =<< makePersistentUpdateQueue _pRootKeysUpdateQueue
        pLevel1KeysUpdateQueue <- refMake =<< makePersistentUpdateQueue _pLevel1KeysUpdateQueue
        pLevel2KeysUpdateQueue <- refMake =<< makePersistentUpdateQueue _pLevel2KeysUpdateQueue
        pProtocolQueue <- refMake =<< makePersistentUpdateQueue _pProtocolQueue
        pElectionDifficultyQueue <- refMake =<< makePersistentUpdateQueue _pElectionDifficultyQueue
        pEuroPerEnergyQueue <- refMake =<< makePersistentUpdateQueue _pEuroPerEnergyQueue
        pMicroGTUPerEuroQueue <- refMake =<< makePersistentUpdateQueue _pMicroGTUPerEuroQueue
        pFoundationAccountQueue <- refMake =<< makePersistentUpdateQueue _pFoundationAccountQueue
        pMintDistributionQueue <- refMake =<< makePersistentUpdateQueue _pMintDistributionQueue
        pTransactionFeeDistributionQueue <- refMake =<< makePersistentUpdateQueue _pTransactionFeeDistributionQueue
        pGASRewardsQueue <- refMake =<< makePersistentUpdateQueue _pGASRewardsQueue
        pPoolParametersQueue <- refMake =<< makePersistentUpdateQueue _pPoolParametersQueue
        pAddAnonymityRevokerQueue <- refMake =<< makePersistentUpdateQueue _pAddAnonymityRevokerQueue
        pAddIdentityProviderQueue <- refMake =<< makePersistentUpdateQueue _pAddIdentityProviderQueue
        pCooldownParametersQueue <- mapM ((refMake =<<) . makePersistentUpdateQueue) _pCooldownParametersQueue
        pTimeParametersQueue <- mapM ((refMake =<<) . makePersistentUpdateQueue) _pTimeParametersQueue
        return PendingUpdates{..}

-- |Convert a persistent 'PendingUpdates' to an in-memory 'UQ.PendingUpdates'.
makeBasicPendingUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => PendingUpdates cpv -> m (UQ.PendingUpdates cpv)
makeBasicPendingUpdates PendingUpdates{..} = do
        _pRootKeysUpdateQueue <- makeBasicUpdateQueue =<< refLoad pRootKeysUpdateQueue
        _pLevel1KeysUpdateQueue <- makeBasicUpdateQueue =<< refLoad pLevel1KeysUpdateQueue
        _pLevel2KeysUpdateQueue <- makeBasicUpdateQueue =<< refLoad pLevel2KeysUpdateQueue
        _pProtocolQueue <- makeBasicUpdateQueue =<< refLoad pProtocolQueue
        _pElectionDifficultyQueue <- makeBasicUpdateQueue =<< refLoad pElectionDifficultyQueue
        _pEuroPerEnergyQueue <- makeBasicUpdateQueue =<< refLoad pEuroPerEnergyQueue
        _pMicroGTUPerEuroQueue <- makeBasicUpdateQueue =<< refLoad pMicroGTUPerEuroQueue
        _pFoundationAccountQueue <- makeBasicUpdateQueue =<< refLoad pFoundationAccountQueue
        _pMintDistributionQueue <- makeBasicUpdateQueue =<< refLoad pMintDistributionQueue
        _pTransactionFeeDistributionQueue <- makeBasicUpdateQueue =<< refLoad pTransactionFeeDistributionQueue
        _pGASRewardsQueue <- makeBasicUpdateQueue =<< refLoad pGASRewardsQueue
        _pPoolParametersQueue <- makeBasicUpdateQueue =<< refLoad pPoolParametersQueue
        _pAddAnonymityRevokerQueue <- makeBasicUpdateQueue =<< refLoad pAddAnonymityRevokerQueue
        _pAddIdentityProviderQueue <- makeBasicUpdateQueue =<< refLoad pAddIdentityProviderQueue
        _pCooldownParametersQueue <- mapM ((makeBasicUpdateQueue =<<) . refLoad) pCooldownParametersQueue
        _pTimeParametersQueue <- mapM ((makeBasicUpdateQueue =<<) . refLoad) pTimeParametersQueue
        return UQ.PendingUpdates{..}

-- |Current state of updatable parameters and update queues.
data Updates' (cpv :: ChainParametersVersion) = Updates {
        -- |Current update authorizations.
        currentKeyCollection :: !(HashedBufferedRef (StoreSerialized (UpdateKeysCollection cpv))),
        -- |Current protocol update.
        currentProtocolUpdate :: !(Nullable (HashedBufferedRef (StoreSerialized ProtocolUpdate))),
        -- |Current chain parameters.
        currentParameters :: !(HashedBufferedRef (StoreSerialized (ChainParameters' cpv))),
        -- |Pending updates.
        pendingUpdates :: !(PendingUpdates cpv)
    }

type Updates (pv :: ProtocolVersion) = Updates' (ChainParametersVersionFor pv)

instance (MonadBlobStore m, IsChainParametersVersion cpv) => MHashableTo m H.Hash (Updates' cpv) where
    getHashM Updates{..} = do
        hCA <- getHashM currentKeyCollection
        mHCPU <- mapM getHashM currentProtocolUpdate
        hCP <- getHashM currentParameters
        hPU <- getHashM pendingUpdates
        return $! H.hash $
            H.hashToByteString hCA
            <> case mHCPU of
                Null -> "\x00"
                Some hcpu -> "\x01" <> H.hashToByteString hcpu
            <> H.hashToByteString hCP
            <> H.hashToByteString hPU

instance (MonadBlobStore m, IsChainParametersVersion cpv)
        => BlobStorable m (Updates' cpv) where
    storeUpdate Updates{..} = do
        (pKC, kC) <- storeUpdate currentKeyCollection
        (pCPU, cPU) <- storeUpdate currentProtocolUpdate
        (pCP, cP) <- storeUpdate currentParameters
        (pPU, pU) <- storeUpdate pendingUpdates
        let newUpdates = Updates{
                currentKeyCollection = kC,
                currentProtocolUpdate = cPU,
                currentParameters = cP,
                pendingUpdates = pU
            }
        return (pKC >> pCPU >> pCP >> pPU, newUpdates)
    store u = fst <$> storeUpdate u
    load = do
        mKC <- label "Current key collection" load
        mCPU <- label "Current protocol update" load
        mCP <- label "Current parameters" load
        mPU <- label "Pending updates" load
        return $! do
            currentKeyCollection <- mKC
            currentProtocolUpdate <- mCPU
            currentParameters <- mCP
            pendingUpdates <- mPU
            return Updates{..}

instance (MonadBlobStore m, IsChainParametersVersion cpv) => Cacheable m (Updates' cpv) where
    cache Updates{..} = Updates
        <$> cache currentKeyCollection
        <*> cache currentProtocolUpdate
        <*> cache currentParameters
        <*> cache pendingUpdates

-- |An initial 'Updates' with the given initial 'Authorizations'
-- and 'ChainParameters'.
initialUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => UpdateKeysCollection cpv
    -> ChainParameters' cpv
    -> m (Updates' cpv)
initialUpdates initialKeyCollection chainParams = do
        currentKeyCollection <- makeHashedBufferedRef (StoreSerialized initialKeyCollection)
        let currentProtocolUpdate = Null
        currentParameters <- makeHashedBufferedRef (StoreSerialized chainParams)
        pendingUpdates <- emptyPendingUpdates
        return Updates{..}

-- |Make a persistent 'Updates' from an in-memory one.
makePersistentUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => UQ.Updates' cpv
    -> m (Updates' cpv)
makePersistentUpdates UQ.Updates{..} = do
        currentKeyCollection <- refMake (StoreSerialized (_unhashed _currentKeyCollection))
        currentProtocolUpdate <- case _currentProtocolUpdate of
            Nothing -> return Null
            Just pu -> Some <$> refMake (StoreSerialized pu)
        currentParameters <- refMake (StoreSerialized _currentParameters)
        pendingUpdates <- makePersistentPendingUpdates _pendingUpdates
        return Updates{..}

-- |Convert a persistent 'Updates' to an in-memory 'UQ.Updates'.
makeBasicUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => (Updates' cpv)
    -> m (UQ.Updates' cpv)
makeBasicUpdates Updates{..} = do
        hKC <- getHashM currentKeyCollection
        kc <- unStoreSerialized <$> refLoad currentKeyCollection
        let _currentKeyCollection = Hashed kc hKC
        _currentProtocolUpdate <- case currentProtocolUpdate of
            Null -> return Nothing
            Some pu -> Just . unStoreSerialized <$> refLoad pu
        _currentParameters <- unStoreSerialized <$> refLoad currentParameters
        _pendingUpdates <- makeBasicPendingUpdates pendingUpdates
        return UQ.Updates{..}

-- |Process the update queue to determine the new value of a parameter (or the authorizations).
-- This splits the queue at the given timestamp. The last value up to and including the timestamp
-- is the new value, if any -- otherwise the current value is retained. The queue is updated to
-- be the updates with later timestamps.
processValueUpdates :: (MonadBlobStore m, Serialize v, MHashableTo m H.Hash v)
    => Timestamp
    -> UpdateQueue v
    -> m res
    -- ^No update continuation
    -> (HashedBufferedRef (StoreSerialized v) -> UpdateQueue v -> Map.Map TransactionTime v -> m res)
    -- ^Update continuation
    -> m res
processValueUpdates t uq noUpdate doUpdate = case ql of
                Seq.Empty -> noUpdate
                _ Seq.:|> (_, b) -> do
                    changes <- foldM accumChanges Map.empty ql
                    doUpdate b uq{uqQueue = qr} changes
    where
        (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue uq)
        accumChanges m (tt, r) = do
            v <- unStoreSerialized <$> refLoad r
            return $! Map.insert tt v m

-- |Process root keys updates.
processRootKeysUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processRootKeysUpdates t bu = do
        u@Updates{..} <- refLoad bu
        rootKeysQueue <- refLoad (pRootKeysUpdateQueue pendingUpdates)
        previousKeyCollection <- unStoreSerialized <$> refLoad currentKeyCollection
        processValueUpdates t rootKeysQueue (return (Map.empty, bu)) $ \newRootKeys newRootKeysQueue changes -> (UVRootKeys <$> changes,) <$> do
          newRootKeysValue <- unStoreSerialized <$> refLoad newRootKeys
          newKeyCollection <- refMake . StoreSerialized $ previousKeyCollection { rootKeys = newRootKeysValue }
          newRootKeysQueueStored <- refMake newRootKeysQueue
          refMake u {
            currentKeyCollection = newKeyCollection,
            pendingUpdates = pendingUpdates { pRootKeysUpdateQueue = newRootKeysQueueStored }
            }

-- |Process level 1 keys updates.
processLevel1KeysUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processLevel1KeysUpdates t bu = do
        u@Updates{..} <- refLoad bu
        level1KeysQueue <- refLoad (pLevel1KeysUpdateQueue pendingUpdates)
        previousKeyCollection <- unStoreSerialized <$> refLoad currentKeyCollection
        processValueUpdates t level1KeysQueue (return (Map.empty, bu)) $ \newLevel1Keys newLevel1KeysQueue changes -> (UVLevel1Keys <$> changes,) <$> do
          newLevel1KeysValue <- unStoreSerialized <$> refLoad newLevel1Keys
          newKeyCollection <- refMake . StoreSerialized $ previousKeyCollection { level1Keys = newLevel1KeysValue }
          newLevel1KeysQueueStored <- refMake newLevel1KeysQueue
          refMake u {
            currentKeyCollection = newKeyCollection,
            pendingUpdates = pendingUpdates { pLevel1KeysUpdateQueue = newLevel1KeysQueueStored }
            }

-- |Process level 2 keys updates.
processLevel2KeysUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processLevel2KeysUpdates t bu = do
        u@Updates{..} <- refLoad bu
        level2KeysQueue <- refLoad (pLevel2KeysUpdateQueue pendingUpdates)
        previousKeyCollection <- unStoreSerialized <$> refLoad currentKeyCollection
        processValueUpdates t level2KeysQueue (return (Map.empty, bu)) $ \newLevel2Keys newLevel2KeysQueue changes -> (UVLevel2Keys <$> changes,) <$> do
          newLevel2KeysValue <- unStoreSerialized <$> refLoad newLevel2Keys
          newKeyCollection <- refMake . StoreSerialized $ previousKeyCollection { level2Keys = newLevel2KeysValue }
          newLevel2KeysQueueStored <- refMake newLevel2KeysQueue
          refMake u {
            currentKeyCollection = newKeyCollection,
            pendingUpdates = pendingUpdates { pLevel2KeysUpdateQueue = newLevel2KeysQueueStored }
            }

-- |Process election difficulty updates.
processElectionDifficultyUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processElectionDifficultyUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pElectionDifficultyQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newEDp newQ m -> (UVElectionDifficulty <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newED <- refLoad newEDp
            newParameters <- refMake $ StoreSerialized $ oldCP & cpElectionDifficulty .~ newED
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pElectionDifficultyQueue = newpQ}
                }

-- |Process Euro:energy rate updates.
processEuroPerEnergyUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processEuroPerEnergyUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pEuroPerEnergyQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newEPEp newQ m -> (UVEuroPerEnergy <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newEPE <- refLoad newEPEp
            newParameters <- refMake $ StoreSerialized $ oldCP & euroPerEnergy .~ newEPE
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pEuroPerEnergyQueue = newpQ}
                }

-- |Process microGTU:Euro rate updates.
processMicroGTUPerEuroUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processMicroGTUPerEuroUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pMicroGTUPerEuroQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newMGTUPEp newQ m -> (UVMicroGTUPerEuro <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newMGTUPE <- refLoad newMGTUPEp
            newParameters <- refMake $ StoreSerialized $ oldCP & microGTUPerEuro .~ newMGTUPE
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pMicroGTUPerEuroQueue = newpQ}
                }

processFoundationAccountUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processFoundationAccountUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pFoundationAccountQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVFoundationAccount <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newParam <- refLoad newParamPtr
            newParameters <- refMake $ StoreSerialized $ oldCP & cpFoundationAccount .~ newParam
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pFoundationAccountQueue = newpQ}
                }

processMintDistributionUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processMintDistributionUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pMintDistributionQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVMintDistribution <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newParam <- refLoad newParamPtr
            newParameters <- refMake $ StoreSerialized $ oldCP & rpMintDistribution .~ newParam
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pMintDistributionQueue = newpQ}
                }

processTransactionFeeDistributionUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processTransactionFeeDistributionUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pTransactionFeeDistributionQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVTransactionFeeDistribution <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newParam <- refLoad newParamPtr
            newParameters <- refMake $ StoreSerialized $ oldCP & rpTransactionFeeDistribution .~ newParam
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pTransactionFeeDistributionQueue = newpQ}
                }

processGASRewardsUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processGASRewardsUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pGASRewardsQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVGASRewards <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newParam <- refLoad newParamPtr
            newParameters <- refMake $ StoreSerialized $ oldCP & rpGASRewards .~ newParam
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pGASRewardsQueue = newpQ}
                }

processPoolParamatersUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processPoolParamatersUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pPoolParametersQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVPoolParameters <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newParam <- refLoad newParamPtr
            newParameters <- refMake $ StoreSerialized $ oldCP & cpPoolParameters .~ newParam
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pPoolParametersQueue = newpQ}
                }

-- |Process cooldown parameters updates.
processCooldownParametersUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processCooldownParametersUpdates t bu = do
        u@Updates{..} <- refLoad bu
        case pCooldownParametersQueue pendingUpdates of
            NothingForCPV1 -> return (Map.empty, bu)
            JustForCPV1 qref -> do
                oldQ <- refLoad qref
                processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVCooldownParameters <$> m,) <$> do
                    newpQ <- refMake newQ
                    StoreSerialized oldCP <- refLoad currentParameters
                    StoreSerialized newParam <- refLoad newParamPtr
                    newParameters <- refMake $ StoreSerialized $ oldCP & cpCooldownParameters .~ newParam
                    refMake u{
                            currentParameters = newParameters,
                            pendingUpdates = pendingUpdates{pCooldownParametersQueue = JustForCPV1 newpQ}
                        }

-- |Process time parameters updates.
processTimeParametersUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processTimeParametersUpdates t bu = do
        u@Updates{..} <- refLoad bu
        case pTimeParametersQueue pendingUpdates of
            NothingForCPV1 -> return (Map.empty, bu)
            JustForCPV1 qref -> do
                oldQ <- refLoad qref
                processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVTimeParameters <$> m,) <$> do
                    newpQ <- refMake newQ
                    StoreSerialized oldCP <- refLoad currentParameters
                    StoreSerialized newParam <- refLoad newParamPtr
                    newParameters <- refMake $ StoreSerialized $ oldCP & cpTimeParameters .~ newParam
                    refMake u{
                            currentParameters = newParameters,
                            pendingUpdates = pendingUpdates{pTimeParametersQueue = JustForCPV1 newpQ}
                        }

-- |Process the add anonymity revoker update queue.
--  Ignores updates with duplicate ARs.
processAddAnonymityRevokerUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> HashedBufferedRef ARS.AnonymityRevokers
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv), HashedBufferedRef ARS.AnonymityRevokers)
processAddAnonymityRevokerUpdates t bu hbar = do
    u@Updates{..} <- refLoad bu
    oldQ <- refLoad (pAddAnonymityRevokerQueue pendingUpdates)
    let (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue oldQ)
    if null ql
        then return (Map.empty, bu, hbar)
        else do
            oldARs <- ARS.arRevokers <$> refLoad hbar
            (changes, updatedARs) <- addAndAccumNonduplicateUpdates oldARs ARS.arIdentity UVAddAnonymityRevoker ql

            newQ <- refMake oldQ { uqQueue = qr }
            newU <- refMake u { pendingUpdates = pendingUpdates {pAddAnonymityRevokerQueue = newQ} }

            -- Since updates might be invalid, we check whether any actual changes occurred.
            hbar' <- if null changes
                        then return hbar -- Return existing reference if no changes occurred.
                        else refMake . ARS.AnonymityRevokers $ updatedARs
            return (changes, newU, hbar')

-- |Process the add identity provider update queue.
--  Ignores updates with duplicate IPs.
processAddIdentityProviderUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> HashedBufferedRef IPS.IdentityProviders
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv), HashedBufferedRef IPS.IdentityProviders)
processAddIdentityProviderUpdates t bu hbip = do
    u@Updates{..} <- refLoad bu
    oldQ <- refLoad (pAddIdentityProviderQueue pendingUpdates)
    let (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue oldQ)
    if null ql
        then return (Map.empty, bu, hbip)
        else do
            oldIPs <- IPS.idProviders <$> refLoad hbip
            (changes, updatedIPs) <- addAndAccumNonduplicateUpdates oldIPs IPS.ipIdentity UVAddIdentityProvider ql

            newQ <- refMake oldQ { uqQueue = qr }
            newU <- refMake u { pendingUpdates = pendingUpdates {pAddIdentityProviderQueue = newQ} }

            -- Since updates might be invalid, we check whether any actual changes occured.
            hbip' <- if null changes
                        then return hbip -- Return existing reference if no changes occured.
                        else refMake . IPS.IdentityProviders $ updatedIPs
            return (changes, newU, hbip')

-- |Used for adding new IPs and ARs.
-- Ensuring that new IPs/ARs have unique ids is difficult when enqueueing.
-- Instead, it is handled here by ignoring updates with duplicate IPs/ARs.
-- It also accumulates the actual changes that occured.
addAndAccumNonduplicateUpdates :: (MonadBlobStore m, Foldable f, Reference m ref (StoreSerialized v), Ord k)
                      => Map.Map k v -- ^ The existing IPs / ARs.
                      -> (v -> k) -- ^ Getter for the key field.
                      -> (v -> UpdateValue cpv) -- ^ Data constructor for UpdateValue.
                      -> f (TransactionTime, ref (StoreSerialized v)) -- ^ The updates.
                      -> m (Map.Map TransactionTime (UpdateValue cpv), Map.Map k v)
addAndAccumNonduplicateUpdates oldMap getKey toUV = foldM go (Map.empty, oldMap)
  where go (changesMap, valMap) (tt, r) = do
          v <- unStoreSerialized <$> refLoad r
          let key = getKey v
          if Map.member key valMap
            then return (changesMap, valMap) -- Ignore invalid update
            else do
                let changesMap' = Map.insert tt (toUV v) changesMap
                    valMap' = Map.insert key v valMap
                return (changesMap', valMap')

-- |Process the protocol update queue.  Unlike other queues, once a protocol update occurs, it is not
-- overridden by later ones.
-- FIXME: We may just want to keep unused protocol updates in the queue, even if their timestamps have
-- elapsed.
processProtocolUpdates
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> BufferedRef (Updates' cpv)
    -> m (Map.Map TransactionTime (UpdateValue cpv), BufferedRef (Updates' cpv))
processProtocolUpdates t bu = do
        u@Updates{..} <- refLoad bu
        protQueue <- refLoad (pProtocolQueue pendingUpdates)
        let (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue protQueue)
        case ql of
            Seq.Empty -> return (Map.empty, bu)
            (_, pu) Seq.:<| _ -> do
                changes <- foldM accumChanges Map.empty ql
                let newProtocolUpdate = case currentProtocolUpdate of
                        Null -> Some pu
                        s -> s
                newpProtQueue <- refMake protQueue {
                        uqQueue = qr
                    }
                (changes,) <$> refMake u {
                        currentProtocolUpdate = newProtocolUpdate,
                        pendingUpdates = pendingUpdates {pProtocolQueue = newpProtQueue}
                    }
    where
        accumChanges m (tt, r) = do
            v <- UVProtocol . unStoreSerialized <$> refLoad r
            return $! Map.insert tt v m

type UpdatesWithARsAndIPs (cpv :: ChainParametersVersion) =
    (BufferedRef (Updates' cpv), HashedBufferedRef ARS.AnonymityRevokers, HashedBufferedRef IPS.IdentityProviders)

-- |Process all update queues.
processUpdateQueues
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => Timestamp
    -> UpdatesWithARsAndIPs cpv
    -> m (Map.Map TransactionTime (UpdateValue cpv), UpdatesWithARsAndIPs cpv)
processUpdateQueues t (u0, ars, ips) = do
  (m1, u1) <- (processRootKeysUpdates t
        `pThen` processLevel1KeysUpdates t
        `pThen` processLevel2KeysUpdates t
        `pThen` processProtocolUpdates t
        `pThen` processElectionDifficultyUpdates t
        `pThen` processEuroPerEnergyUpdates t
        `pThen` processMicroGTUPerEuroUpdates t
        `pThen` processFoundationAccountUpdates t
        `pThen` processMintDistributionUpdates t
        `pThen` processTransactionFeeDistributionUpdates t
        `pThen` processGASRewardsUpdates t
        `pThen` processPoolParamatersUpdates t
        `pThen` processCooldownParametersUpdates t
        `pThen` processTimeParametersUpdates t) u0

  -- AR and IP updates are handled separately to avoid adding the large objects to the 'Updates' types.
  (m2, u2, ars') <- processAddAnonymityRevokerUpdates t u1 ars
  (m3, u3, ips') <- processAddIdentityProviderUpdates t u2 ips

  return (m1 <> m2 <> m3, (u3, ars', ips'))
    where
        pThen a b = \i -> do
            (m1, r1) <- a i
            (m2, r2) <- b r1
            return (m1 <> m2, r2)

-- |Determine the future election difficulty (at a given time) based
-- on a current 'Updates'.
futureElectionDifficulty
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> Timestamp
    -> m ElectionDifficulty
futureElectionDifficulty uref ts = do
        Updates{..} <- refLoad uref
        oldQ <- refLoad (pElectionDifficultyQueue pendingUpdates)
        let getCurED = do
                StoreSerialized cp <- refLoad currentParameters
                return $ cp ^. cpElectionDifficulty
        processValueUpdates ts oldQ getCurED (\newEDp _ _ -> unStoreSerialized <$> refLoad newEDp)

-- |Get the protocol update status: either an effective protocol update or
-- a list of pending future protocol updates.
protocolUpdateStatus
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> m UQ.ProtocolUpdateStatus
protocolUpdateStatus uref = do
        Updates{..} <- refLoad uref
        case currentProtocolUpdate of
            Null -> do
                pq <- refLoad (pProtocolQueue pendingUpdates)
                UQ.PendingProtocolUpdates . toList <$> forM (uqQueue pq) (\(t, e) -> (t,) . unStoreSerialized <$> refLoad e)
            Some puRef -> UQ.ProtocolUpdated . unStoreSerialized <$> refLoad puRef

-- |Determine the next sequence number for a given update type.
lookupNextUpdateSequenceNumber
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> UpdateType
    -> m UpdateSequenceNumber
lookupNextUpdateSequenceNumber uref uty = do
        Updates{..} <- refLoad uref
        case uty of
            UpdateProtocol -> uqNextSequenceNumber <$> refLoad (pProtocolQueue pendingUpdates)
            UpdateElectionDifficulty -> uqNextSequenceNumber <$> refLoad (pElectionDifficultyQueue pendingUpdates)
            UpdateEuroPerEnergy -> uqNextSequenceNumber <$> refLoad (pEuroPerEnergyQueue pendingUpdates)
            UpdateMicroGTUPerEuro -> uqNextSequenceNumber <$> refLoad (pMicroGTUPerEuroQueue pendingUpdates)
            UpdateFoundationAccount -> uqNextSequenceNumber <$> refLoad (pFoundationAccountQueue pendingUpdates)
            UpdateMintDistribution -> uqNextSequenceNumber <$> refLoad (pMintDistributionQueue pendingUpdates)
            UpdateTransactionFeeDistribution -> uqNextSequenceNumber <$> refLoad (pTransactionFeeDistributionQueue pendingUpdates)
            UpdateGASRewards -> uqNextSequenceNumber <$> refLoad (pGASRewardsQueue pendingUpdates)
            UpdatePoolParameters -> uqNextSequenceNumber <$> refLoad (pPoolParametersQueue pendingUpdates)
            UpdateAddAnonymityRevoker -> uqNextSequenceNumber <$> refLoad (pAddAnonymityRevokerQueue pendingUpdates)
            UpdateAddIdentityProvider -> uqNextSequenceNumber <$> refLoad (pAddIdentityProviderQueue pendingUpdates)
            UpdateRootKeys -> uqNextSequenceNumber <$> refLoad (pRootKeysUpdateQueue pendingUpdates)
            UpdateLevel1Keys -> uqNextSequenceNumber <$> refLoad (pLevel1KeysUpdateQueue pendingUpdates)
            UpdateLevel2Keys -> uqNextSequenceNumber <$> refLoad (pLevel2KeysUpdateQueue pendingUpdates)
            UpdateCooldownParameters ->
                maybeForCPV1
                    (pure minUpdateSequenceNumber)
                    (fmap uqNextSequenceNumber . refLoad)
                    (pCooldownParametersQueue pendingUpdates)
            UpdateTimeParameters ->
                maybeForCPV1
                    (pure minUpdateSequenceNumber)
                    (fmap uqNextSequenceNumber . refLoad)
                    (pTimeParametersQueue pendingUpdates)

-- |Enqueue an update in the appropriate queue.
enqueueUpdate
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => TransactionTime
    -> UpdateValue cpv
    -> BufferedRef (Updates' cpv)
    -> m (BufferedRef (Updates' cpv))
enqueueUpdate effectiveTime payload uref = do
        u@Updates{pendingUpdates = p@PendingUpdates{..}} <- refLoad uref
        newPendingUpdates <- case payload of
            UVProtocol auths -> enqueue effectiveTime auths pProtocolQueue <&> \newQ -> p{pProtocolQueue=newQ}
            UVElectionDifficulty auths -> enqueue effectiveTime auths pElectionDifficultyQueue <&> \newQ -> p{pElectionDifficultyQueue=newQ}
            UVEuroPerEnergy auths -> enqueue effectiveTime auths pEuroPerEnergyQueue <&> \newQ -> p{pEuroPerEnergyQueue=newQ}
            UVMicroGTUPerEuro auths -> enqueue effectiveTime auths pMicroGTUPerEuroQueue <&> \newQ -> p{pMicroGTUPerEuroQueue=newQ}
            UVFoundationAccount v -> enqueue effectiveTime v pFoundationAccountQueue <&> \newQ -> p {pFoundationAccountQueue=newQ}
            UVMintDistribution v -> enqueue effectiveTime v pMintDistributionQueue <&> \newQ -> p {pMintDistributionQueue=newQ}
            UVTransactionFeeDistribution v -> enqueue effectiveTime v pTransactionFeeDistributionQueue <&> \newQ -> p {pTransactionFeeDistributionQueue=newQ}
            UVGASRewards v -> enqueue effectiveTime v pGASRewardsQueue <&> \newQ -> p {pGASRewardsQueue=newQ}
            UVPoolParameters v -> enqueue effectiveTime v pPoolParametersQueue <&> \newQ -> p {pPoolParametersQueue=newQ}
            UVAddAnonymityRevoker v -> enqueue effectiveTime v pAddAnonymityRevokerQueue <&> \newQ -> p {pAddAnonymityRevokerQueue=newQ}
            UVAddIdentityProvider v -> enqueue effectiveTime v pAddIdentityProviderQueue <&> \newQ -> p {pAddIdentityProviderQueue=newQ}
            UVRootKeys v -> enqueue effectiveTime v pRootKeysUpdateQueue <&> \newQ -> p {pRootKeysUpdateQueue=newQ}
            UVLevel1Keys v -> enqueue effectiveTime v pLevel1KeysUpdateQueue <&> \newQ -> p {pLevel1KeysUpdateQueue=newQ}
            UVLevel2Keys v -> enqueue effectiveTime v pLevel2KeysUpdateQueue <&> \newQ -> p {pLevel2KeysUpdateQueue=newQ}
            UVCooldownParameters v ->
                enqueue effectiveTime v (unJustForCPV1 pCooldownParametersQueue)
                    <&> \newQ -> p {pCooldownParametersQueue = JustForCPV1 newQ}
            UVTimeParameters v ->
                enqueue effectiveTime v (unJustForCPV1 pTimeParametersQueue)
                    <&> \newQ -> p {pTimeParametersQueue = JustForCPV1 newQ}
        refMake u{pendingUpdates = newPendingUpdates}

-- |Overwrite the election difficulty with the specified value and remove
-- any pending updates to the election difficulty from the queue.
overwriteElectionDifficulty
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => ElectionDifficulty
    -> BufferedRef (Updates' cpv)
    -> m (BufferedRef (Updates' cpv))
overwriteElectionDifficulty newDifficulty uref = do
    u@Updates{pendingUpdates = p@PendingUpdates{..}, ..} <- refLoad uref
    StoreSerialized cp <- refLoad currentParameters
    newCurrentParameters <- refMake $ StoreSerialized (cp & cpElectionDifficulty .~ newDifficulty)
    newPendingUpdates <- clearQueue pElectionDifficultyQueue <&> \newQ -> p{pElectionDifficultyQueue=newQ}
    refMake u{currentParameters = newCurrentParameters, pendingUpdates = newPendingUpdates}

-- |Clear the protocol update and remove any pending protocol updates from
-- the queue.
clearProtocolUpdate
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> m (BufferedRef (Updates' cpv))
clearProtocolUpdate uref = do
    u@Updates{pendingUpdates = p@PendingUpdates{..}} <- refLoad uref
    newPendingUpdates <- clearQueue pProtocolQueue <&> \newQ -> p{pProtocolQueue=newQ}
    refMake u{currentProtocolUpdate = Null, pendingUpdates = newPendingUpdates}

-- |Get the current EnergyRate.
lookupEnergyRate
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> m EnergyRate
lookupEnergyRate uref = do
        Updates{..} <- refLoad uref
        StoreSerialized ChainParameters{..} <- refLoad currentParameters
        return (_cpExchangeRates ^. energyRate)

-- |Look up the current chain parameters.
lookupCurrentParameters
    :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> m (ChainParameters' cpv)
lookupCurrentParameters uref = do
        Updates{..} <- refLoad uref
        unStoreSerialized <$> refLoad currentParameters

-- |Serialize updates in V0 format.
putUpdatesV0
    :: (MonadBlobStore m, MonadPut m, IsChainParametersVersion cpv)
    => Updates' cpv
    -> m ()
putUpdatesV0 Updates{..} = do
        sPut . unStoreSerialized =<< refLoad currentKeyCollection
        case currentProtocolUpdate of
            Null -> liftPut $ putWord8 0
            Some pu -> do
                liftPut $ putWord8 1
                sPut . unStoreSerialized =<< refLoad pu
        sPut . unStoreSerialized =<< refLoad currentParameters
        putPendingUpdatesV0 pendingUpdates

-- |Look up the pending changes to the time parameters.
lookupPendingTimeParameters :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> m [(TransactionTime, TimeParameters cpv)]
lookupPendingTimeParameters uref = do
        Updates{..} <- refLoad uref
        case pTimeParametersQueue pendingUpdates of
            NothingForCPV1  -> return []
            JustForCPV1 tpq -> loadQueue tpq

-- |Look up the pending changes to the pool parameters.
lookupPendingPoolParameters :: (MonadBlobStore m, IsChainParametersVersion cpv)
    => BufferedRef (Updates' cpv)
    -> m [(TransactionTime, PoolParameters cpv)]
lookupPendingPoolParameters uref = do
        Updates{..} <- refLoad uref
        loadQueue (pPoolParametersQueue pendingUpdates)
