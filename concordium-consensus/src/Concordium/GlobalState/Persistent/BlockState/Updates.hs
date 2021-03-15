{-# LANGUAGE MonoLocalBinds, BangPatterns, ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Implementation of the chain update mechanism with persistent storage: https://concordium.gitlab.io/whitepapers/update-mechanism/main.pdf
module Concordium.GlobalState.Persistent.BlockState.Updates where

import Data.Foldable (toList)
import qualified Data.Map as Map
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
import qualified Concordium.GlobalState.Basic.BlockState.Updates as Basic

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
makePersistentUpdateQueue :: (MonadIO m, MHashableTo m H.Hash (StoreSerialized e)) => Basic.UpdateQueue e -> m (UpdateQueue e)
makePersistentUpdateQueue Basic.UpdateQueue{..} = do
    let uqNextSequenceNumber = _uqNextSequenceNumber
    uqQueue <- Seq.fromList <$> forM _uqQueue (\(t, e) -> (t,) <$> makeHashedBufferedRef (StoreSerialized e))
    return UpdateQueue{..}

-- |Convert a persistent update queue to an in-memory one.
makeBasicUpdateQueue :: (MonadBlobStore m, MHashableTo m H.Hash (StoreSerialized e), Serialize e) => UpdateQueue e -> m (Basic.UpdateQueue e)
makeBasicUpdateQueue UpdateQueue{..} = do
    let _uqNextSequenceNumber = uqNextSequenceNumber
    _uqQueue <- toList <$> forM uqQueue (\(t, e) -> (t,) . unStoreSerialized <$> refLoad e)
    return Basic.UpdateQueue{..}

-- |Convert a persistent update queue to an in-memory one.
makeBasicUpdateQueueHashed :: (MonadBlobStore m, MHashableTo m H.Hash (StoreSerialized e), Serialize e) => UpdateQueue e -> m (Basic.UpdateQueue (Hashed e))
makeBasicUpdateQueueHashed UpdateQueue{..} = do
    let _uqNextSequenceNumber = uqNextSequenceNumber
    _uqQueue <- toList <$> forM uqQueue (\(t, e) -> do
            v <- unStoreSerialized <$> refLoad e
            h <- getHashM e
            return (t, Hashed v h))
    return Basic.UpdateQueue{..}

-- |Add an update event to an update queue, incrementing the sequence number.
-- Any updates in the queue with later or equal effective times are removed
-- from the queue.
enqueue :: (MonadIO m, Reference m ref (UpdateQueue e), MHashableTo m H.Hash e)
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

-- |Update queues for all on-chain update types.
data PendingUpdates = PendingUpdates {
        -- |Updates to authorized update keys.
        pAuthorizationQueue :: !(HashedBufferedRef (UpdateQueue Authorizations)),
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
        pMintDistributionQueue :: !(HashedBufferedRef (UpdateQueue MintDistribution)),
        -- |Updates to the transaction fee distribution.
        pTransactionFeeDistributionQueue :: !(HashedBufferedRef (UpdateQueue TransactionFeeDistribution)),
        -- |Updates to the GAS rewards.
        pGASRewardsQueue :: !(HashedBufferedRef (UpdateQueue GASRewards)),
        -- |Updates to the baker minimum threshold
        pBakerStakeThresholdQueue :: !(HashedBufferedRef (UpdateQueue Amount))
    }

instance (MonadBlobStore m) => MHashableTo m H.Hash PendingUpdates where
    getHashM PendingUpdates{..} = do
        hAuthorizationQueue <- H.hashToByteString <$> getHashM pAuthorizationQueue
        hProtocolQueue <- H.hashToByteString <$> getHashM pProtocolQueue
        hElectionDifficultyQueue <- H.hashToByteString <$> getHashM pElectionDifficultyQueue
        hEuroPerEnergyQueue <- H.hashToByteString <$> getHashM pEuroPerEnergyQueue
        hMicroGTUPerEuroQueue <- H.hashToByteString <$> getHashM pMicroGTUPerEuroQueue
        hFoundationAccountQueue <- H.hashToByteString <$> getHashM pFoundationAccountQueue
        hMintDistributionQueue <- H.hashToByteString <$> getHashM pMintDistributionQueue
        hTransactionFeeDistributionQueue <- H.hashToByteString <$> getHashM pTransactionFeeDistributionQueue
        hGASRewardsQueue <- H.hashToByteString <$> getHashM pGASRewardsQueue
        hBakerStakeThresholdQueue <- H.hashToByteString <$> getHashM pBakerStakeThresholdQueue
        return $! H.hash $
            hAuthorizationQueue
            <> hProtocolQueue
            <> hElectionDifficultyQueue
            <> hEuroPerEnergyQueue
            <> hMicroGTUPerEuroQueue
            <> hFoundationAccountQueue
            <> hMintDistributionQueue
            <> hTransactionFeeDistributionQueue
            <> hGASRewardsQueue
            <> hBakerStakeThresholdQueue

instance (MonadBlobStore m)
        => BlobStorable m PendingUpdates where
    storeUpdate PendingUpdates{..} = do
            (pAQ, aQ) <- storeUpdate pAuthorizationQueue
            (pPrQ, prQ) <- storeUpdate pProtocolQueue
            (pEDQ, edQ) <- storeUpdate pElectionDifficultyQueue
            (pEPEQ, epeQ) <- storeUpdate pEuroPerEnergyQueue
            (pMGTUPEQ, mgtupeQ) <- storeUpdate pMicroGTUPerEuroQueue
            (putFoundationAccountQueue, newFoundationAccountQueue) <- storeUpdate pFoundationAccountQueue
            (putMintDistributionQueue, newMintDistributionQueue) <- storeUpdate pMintDistributionQueue
            (putTransactionFeeDistributionQueue, newTransactionFeeDistributionQueue) <- storeUpdate pTransactionFeeDistributionQueue
            (putGASRewardsQueue, newGASRewardsQueue) <- storeUpdate pGASRewardsQueue
            (putBakerStakeThresholdQueue, newBakerStakeThresholdQueue) <- storeUpdate pBakerStakeThresholdQueue
            let newPU = PendingUpdates {
                    pAuthorizationQueue = aQ,
                    pProtocolQueue = prQ,
                    pElectionDifficultyQueue = edQ,
                    pEuroPerEnergyQueue = epeQ,
                    pMicroGTUPerEuroQueue = mgtupeQ,
                    pFoundationAccountQueue = newFoundationAccountQueue,
                    pMintDistributionQueue = newMintDistributionQueue,
                    pTransactionFeeDistributionQueue = newTransactionFeeDistributionQueue,
                    pGASRewardsQueue = newGASRewardsQueue,
                    pBakerStakeThresholdQueue = newBakerStakeThresholdQueue
                }
            let putPU = pAQ >> pPrQ >> pEDQ >> pEPEQ >> pMGTUPEQ
                    >> putFoundationAccountQueue
                    >> putMintDistributionQueue
                    >> putTransactionFeeDistributionQueue
                    >> putGASRewardsQueue
                    >> putBakerStakeThresholdQueue
            return (putPU, newPU)
    store pu = fst <$> storeUpdate pu
    load = do
        mAQ <- label "Authorization update queue" load
        mPrQ <- label "Protocol update queue" load
        mEDQ <- label "Election difficulty update queue" load
        mEPEQ <- label "Euro per energy update queue" load
        mMGTUPEQ <- label "Micro GTU per Euro update queue" load
        mFoundationAccountQueue <- label "Foundation account update queue" load
        mMintDistributionQueue <- label "Mint distribution update queue" load
        mTransactionFeeDistributionQueue <- label "Transaction fee distribution update queue" load
        mGASRewardsQueue <- label "GAS rewards update queue" load
        mBakerStakeThresholdQueue <- label "Baker minimum threshold update queue" load
        return $! do
            pAuthorizationQueue <- mAQ
            pProtocolQueue <- mPrQ
            pElectionDifficultyQueue <- mEDQ
            pEuroPerEnergyQueue <- mEPEQ
            pMicroGTUPerEuroQueue <- mMGTUPEQ
            pFoundationAccountQueue <- mFoundationAccountQueue
            pMintDistributionQueue <- mMintDistributionQueue
            pTransactionFeeDistributionQueue <- mTransactionFeeDistributionQueue
            pGASRewardsQueue <- mGASRewardsQueue
            pBakerStakeThresholdQueue <- mBakerStakeThresholdQueue
            return PendingUpdates{..}

instance (MonadBlobStore m) => Cacheable m PendingUpdates where
    cache PendingUpdates{..} =
        PendingUpdates
            <$> cache pAuthorizationQueue
            <*> cache pProtocolQueue
            <*> cache pElectionDifficultyQueue
            <*> cache pEuroPerEnergyQueue
            <*> cache pMicroGTUPerEuroQueue
            <*> cache pFoundationAccountQueue
            <*> cache pMintDistributionQueue
            <*> cache pTransactionFeeDistributionQueue
            <*> cache pGASRewardsQueue
            <*> cache pBakerStakeThresholdQueue

-- |Serialize the pending updates.
putPendingUpdatesV0 :: (MonadBlobStore m, MonadPut m) => PendingUpdates -> m ()
putPendingUpdatesV0 PendingUpdates{..} = do
        putUpdateQueueV0 =<< refLoad pAuthorizationQueue
        putUpdateQueueV0 =<< refLoad pProtocolQueue
        putUpdateQueueV0 =<< refLoad pElectionDifficultyQueue
        putUpdateQueueV0 =<< refLoad pEuroPerEnergyQueue
        putUpdateQueueV0 =<< refLoad pMicroGTUPerEuroQueue
        putUpdateQueueV0 =<< refLoad pFoundationAccountQueue
        putUpdateQueueV0 =<< refLoad pMintDistributionQueue
        putUpdateQueueV0 =<< refLoad pTransactionFeeDistributionQueue
        putUpdateQueueV0 =<< refLoad pGASRewardsQueue
        putUpdateQueueV0 =<< refLoad pBakerStakeThresholdQueue

-- |Initial pending updates with empty queues.
emptyPendingUpdates :: forall m. (MonadBlobStore m) => m PendingUpdates
emptyPendingUpdates = PendingUpdates <$> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e
    where
        e :: MHashableTo m H.Hash (UpdateQueue a) => m (HashedBufferedRef (UpdateQueue a))
        e = makeHashedBufferedRef emptyUpdateQueue

-- |Construct a persistent 'PendingUpdates' from an in-memory one.
makePersistentPendingUpdates :: (MonadBlobStore m) => Basic.PendingUpdates -> m PendingUpdates
makePersistentPendingUpdates Basic.PendingUpdates{..} = do
        pAuthorizationQueue <- refMake =<< makePersistentUpdateQueue (_unhashed <$> _pAuthorizationQueue)
        pProtocolQueue <- refMake =<< makePersistentUpdateQueue _pProtocolQueue
        pElectionDifficultyQueue <- refMake =<< makePersistentUpdateQueue _pElectionDifficultyQueue
        pEuroPerEnergyQueue <- refMake =<< makePersistentUpdateQueue _pEuroPerEnergyQueue
        pMicroGTUPerEuroQueue <- refMake =<< makePersistentUpdateQueue _pMicroGTUPerEuroQueue
        pFoundationAccountQueue <- refMake =<< makePersistentUpdateQueue _pFoundationAccountQueue
        pMintDistributionQueue <- refMake =<< makePersistentUpdateQueue _pMintDistributionQueue
        pTransactionFeeDistributionQueue <- refMake =<< makePersistentUpdateQueue _pTransactionFeeDistributionQueue
        pGASRewardsQueue <- refMake =<< makePersistentUpdateQueue _pGASRewardsQueue
        pBakerStakeThresholdQueue <- refMake =<< makePersistentUpdateQueue _pBakerStakeThresholdQueue
        return PendingUpdates{..}

-- |Convert a persistent 'PendingUpdates' to an in-memory 'Basic.PendingUpdates'.
makeBasicPendingUpdates :: (MonadBlobStore m) => PendingUpdates -> m Basic.PendingUpdates
makeBasicPendingUpdates PendingUpdates{..} = do
        _pAuthorizationQueue <- makeBasicUpdateQueueHashed =<< refLoad pAuthorizationQueue
        _pProtocolQueue <- makeBasicUpdateQueue =<< refLoad pProtocolQueue
        _pElectionDifficultyQueue <- makeBasicUpdateQueue =<< refLoad pElectionDifficultyQueue
        _pEuroPerEnergyQueue <- makeBasicUpdateQueue =<< refLoad pEuroPerEnergyQueue
        _pMicroGTUPerEuroQueue <- makeBasicUpdateQueue =<< refLoad pMicroGTUPerEuroQueue
        _pFoundationAccountQueue <- makeBasicUpdateQueue =<< refLoad pFoundationAccountQueue
        _pMintDistributionQueue <- makeBasicUpdateQueue =<< refLoad pMintDistributionQueue
        _pTransactionFeeDistributionQueue <- makeBasicUpdateQueue =<< refLoad pTransactionFeeDistributionQueue
        _pGASRewardsQueue <- makeBasicUpdateQueue =<< refLoad pGASRewardsQueue
        _pBakerStakeThresholdQueue <- makeBasicUpdateQueue =<< refLoad pBakerStakeThresholdQueue
        return Basic.PendingUpdates{..}

-- |Current state of updatable parameters and update queues.
data Updates = Updates {
        -- |Current update authorizations.
        currentAuthorizations :: !(HashedBufferedRef (StoreSerialized Authorizations)),
        -- |Current protocol update.
        currentProtocolUpdate :: !(Nullable (HashedBufferedRef (StoreSerialized ProtocolUpdate))),
        -- |Current chain parameters.
        currentParameters :: !(HashedBufferedRef (StoreSerialized ChainParameters)),
        -- |Pending updates.
        pendingUpdates :: !PendingUpdates
    }

instance (MonadBlobStore m) => MHashableTo m H.Hash Updates where
    getHashM Updates{..} = do
        hCA <- getHashM currentAuthorizations
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

instance (MonadBlobStore m)
        => BlobStorable m Updates where
    storeUpdate Updates{..} = do
        (pCA, cA) <- storeUpdate currentAuthorizations
        (pCPU, cPU) <- storeUpdate currentProtocolUpdate
        (pCP, cP) <- storeUpdate currentParameters
        (pPU, pU) <- storeUpdate pendingUpdates
        let newUpdates = Updates{
                currentAuthorizations = cA,
                currentProtocolUpdate = cPU,
                currentParameters = cP,
                pendingUpdates = pU
            }
        return (pCA >> pCPU >> pCP >> pPU, newUpdates)
    store u = fst <$> storeUpdate u
    load = do
        mCA <- label "Current authorizations" load
        mCPU <- label "Current protocol update" load
        mCP <- label "Current parameters" load
        mPU <- label "Pending updates" load
        return $! do
            currentAuthorizations <- mCA
            currentProtocolUpdate <- mCPU
            currentParameters <- mCP
            pendingUpdates <- mPU
            return Updates{..}

instance (MonadBlobStore m) => Cacheable m Updates where
    cache Updates{..} = Updates
        <$> cache currentAuthorizations
        <*> cache currentProtocolUpdate
        <*> cache currentParameters
        <*> cache pendingUpdates

-- |An initial 'Updates' with the given initial 'Authorizations'
-- and 'ChainParameters'.
initialUpdates :: (MonadBlobStore m) => Authorizations -> ChainParameters -> m Updates
initialUpdates auths chainParams = do
        currentAuthorizations <- makeHashedBufferedRef (StoreSerialized auths)
        let currentProtocolUpdate = Null
        currentParameters <- makeHashedBufferedRef (StoreSerialized chainParams)
        pendingUpdates <- emptyPendingUpdates
        return Updates{..}

-- |Make a persistent 'Updates' from an in-memory one.
makePersistentUpdates :: (MonadBlobStore m) => Basic.Updates -> m Updates
makePersistentUpdates Basic.Updates{..} = do
        currentAuthorizations <- refMake (StoreSerialized (_unhashed _currentAuthorizations))
        currentProtocolUpdate <- case _currentProtocolUpdate of
            Nothing -> return Null
            Just pu -> Some <$> refMake (StoreSerialized pu)
        currentParameters <- refMake (StoreSerialized _currentParameters)
        pendingUpdates <- makePersistentPendingUpdates _pendingUpdates
        return Updates{..}

-- |Convert a persistent 'Updates' to an in-memory 'Basic.Updates'.
makeBasicUpdates :: (MonadBlobStore m) => Updates -> m Basic.Updates
makeBasicUpdates Updates{..} = do
        hCA <- getHashM currentAuthorizations
        ca <- unStoreSerialized <$> refLoad currentAuthorizations
        let _currentAuthorizations = Hashed ca hCA
        _currentProtocolUpdate <- case currentProtocolUpdate of
            Null -> return Nothing
            Some pu -> Just . unStoreSerialized <$> refLoad pu
        _currentParameters <- unStoreSerialized <$> refLoad currentParameters
        _pendingUpdates <- makeBasicPendingUpdates pendingUpdates
        return Basic.Updates{..}

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

-- |Process authorization updates.
processAuthorizationUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
processAuthorizationUpdates t bu = do
        u@Updates{..} <- refLoad bu
        authQueue <- refLoad (pAuthorizationQueue pendingUpdates)
        processValueUpdates t authQueue (return (Map.empty, bu)) $ \newAuths newQ m -> (UVAuthorization <$> m,) <$> do
            newpAuthQueue <- refMake newQ
            refMake u{
                    currentAuthorizations = newAuths,
                    pendingUpdates = pendingUpdates{pAuthorizationQueue = newpAuthQueue}
                }

-- |Process election difficulty updates.
processElectionDifficultyUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
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
processEuroPerEnergyUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
processEuroPerEnergyUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pEuroPerEnergyQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newEPEp newQ m -> (UVEuroPerEnergy <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newEPE <- refLoad newEPEp
            newParameters <- refMake $ StoreSerialized $ oldCP & cpEuroPerEnergy .~ newEPE
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pEuroPerEnergyQueue = newpQ}
                }

-- |Process microGTU:Euro rate updates.
processMicroGTUPerEuroUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
processMicroGTUPerEuroUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pMicroGTUPerEuroQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newMGTUPEp newQ m -> (UVMicroGTUPerEuro <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newMGTUPE <- refLoad newMGTUPEp
            newParameters <- refMake $ StoreSerialized $ oldCP & cpMicroGTUPerEuro .~ newMGTUPE
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pMicroGTUPerEuroQueue = newpQ}
                }

processFoundationAccountUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
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

processMintDistributionUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
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

processTransactionFeeDistributionUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
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

processGASRewardsUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
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

processBakerStakeThresholdUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
processBakerStakeThresholdUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pBakerStakeThresholdQueue pendingUpdates)
        processValueUpdates t oldQ (return (Map.empty, bu)) $ \newParamPtr newQ m -> (UVBakerStakeThreshold <$> m,) <$> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newParam <- refLoad newParamPtr
            newParameters <- refMake $ StoreSerialized $ oldCP & cpBakerStakeThreshold .~ newParam
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pBakerStakeThresholdQueue = newpQ}
                }

-- |Process the protocol update queue.  Unlike other queues, once a protocol update occurs, it is not
-- overridden by later ones.
-- FIXME: We may just want to keep unused protocol updates in the queue, even if their timestamps have
-- elapsed.
processProtocolUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
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

-- |Process all update queues.
processUpdateQueues :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (Map.Map TransactionTime UpdateValue, BufferedRef Updates)
processUpdateQueues t = processAuthorizationUpdates t
        `pThen` processProtocolUpdates t
        `pThen` processElectionDifficultyUpdates t
        `pThen` processEuroPerEnergyUpdates t
        `pThen` processMicroGTUPerEuroUpdates t
        `pThen` processFoundationAccountUpdates t
        `pThen` processMintDistributionUpdates t
        `pThen` processTransactionFeeDistributionUpdates t
        `pThen` processGASRewardsUpdates t
        `pThen` processBakerStakeThresholdUpdates t
    where
        pThen a b = \i -> do
            (m1, r1) <- a i
            (m2, r2) <- b r1
            return (m1 <> m2, r2)

-- |Determine the future election difficulty (at a given time) based
-- on a current 'Updates'.
futureElectionDifficulty :: (MonadBlobStore m) => BufferedRef Updates -> Timestamp -> m ElectionDifficulty
futureElectionDifficulty uref ts = do
        Updates{..} <- refLoad uref
        oldQ <- refLoad (pElectionDifficultyQueue pendingUpdates)
        let getCurED = do
                StoreSerialized cp <- refLoad currentParameters
                return $ cp ^. cpElectionDifficulty
        processValueUpdates ts oldQ getCurED (\newEDp _ _ -> unStoreSerialized <$> refLoad newEDp)

-- |Get the protocol update status: either an effective protocol update or
-- a list of pending future protocol updates.
protocolUpdateStatus :: (MonadBlobStore m) => BufferedRef Updates -> m (Either ProtocolUpdate [(TransactionTime, ProtocolUpdate)])
protocolUpdateStatus uref = do
        Updates{..} <- refLoad uref
        case currentProtocolUpdate of
            Null -> do
                pq <- refLoad (pProtocolQueue pendingUpdates)
                Right . toList <$> forM (uqQueue pq) (\(t, e) -> (t,) . unStoreSerialized <$> refLoad e)
            Some puRef -> Left . unStoreSerialized <$> refLoad puRef

-- |Determine the next sequence number for a given update type.
lookupNextUpdateSequenceNumber :: (MonadBlobStore m) => BufferedRef Updates -> UpdateType -> m UpdateSequenceNumber
lookupNextUpdateSequenceNumber uref uty = do
        Updates{..} <- refLoad uref
        case uty of
            UpdateAuthorization -> uqNextSequenceNumber <$> refLoad (pAuthorizationQueue pendingUpdates)
            UpdateProtocol -> uqNextSequenceNumber <$> refLoad (pProtocolQueue pendingUpdates)
            UpdateElectionDifficulty -> uqNextSequenceNumber <$> refLoad (pElectionDifficultyQueue pendingUpdates)
            UpdateEuroPerEnergy -> uqNextSequenceNumber <$> refLoad (pEuroPerEnergyQueue pendingUpdates)
            UpdateMicroGTUPerEuro -> uqNextSequenceNumber <$> refLoad (pMicroGTUPerEuroQueue pendingUpdates)
            UpdateFoundationAccount -> uqNextSequenceNumber <$> refLoad (pFoundationAccountQueue pendingUpdates)
            UpdateMintDistribution -> uqNextSequenceNumber <$> refLoad (pMintDistributionQueue pendingUpdates)
            UpdateTransactionFeeDistribution -> uqNextSequenceNumber <$> refLoad (pTransactionFeeDistributionQueue pendingUpdates)
            UpdateGASRewards -> uqNextSequenceNumber <$> refLoad (pGASRewardsQueue pendingUpdates)
            UpdateBakerStakeThreshold -> uqNextSequenceNumber <$> refLoad (pBakerStakeThresholdQueue pendingUpdates)

-- |Enqueue an update in the appropriate queue.
enqueueUpdate :: (MonadBlobStore m) => TransactionTime -> UpdateValue -> BufferedRef Updates -> m (BufferedRef Updates)
enqueueUpdate effectiveTime payload uref = do
        u@Updates{pendingUpdates = p@PendingUpdates{..}} <- refLoad uref
        newPendingUpdates <- case payload of
            UVAuthorization auths -> enqueue effectiveTime auths pAuthorizationQueue <&> \newQ -> p{pAuthorizationQueue=newQ}
            UVProtocol auths -> enqueue effectiveTime auths pProtocolQueue <&> \newQ -> p{pProtocolQueue=newQ}
            UVElectionDifficulty auths -> enqueue effectiveTime auths pElectionDifficultyQueue <&> \newQ -> p{pElectionDifficultyQueue=newQ}
            UVEuroPerEnergy auths -> enqueue effectiveTime auths pEuroPerEnergyQueue <&> \newQ -> p{pEuroPerEnergyQueue=newQ}
            UVMicroGTUPerEuro auths -> enqueue effectiveTime auths pMicroGTUPerEuroQueue <&> \newQ -> p{pMicroGTUPerEuroQueue=newQ}
            UVFoundationAccount v -> enqueue effectiveTime v pFoundationAccountQueue <&> \newQ -> p {pFoundationAccountQueue=newQ}
            UVMintDistribution v -> enqueue effectiveTime v pMintDistributionQueue <&> \newQ -> p {pMintDistributionQueue=newQ}
            UVTransactionFeeDistribution v -> enqueue effectiveTime v pTransactionFeeDistributionQueue <&> \newQ -> p {pTransactionFeeDistributionQueue=newQ}
            UVGASRewards v -> enqueue effectiveTime v pGASRewardsQueue <&> \newQ -> p {pGASRewardsQueue=newQ}
            UVBakerStakeThreshold v -> enqueue effectiveTime v pBakerStakeThresholdQueue <&> \newQ -> p {pBakerStakeThresholdQueue=newQ}
        refMake u{pendingUpdates = newPendingUpdates}

-- |Get the current EnergyRate.
lookupEnergyRate :: (MonadBlobStore m) => BufferedRef Updates -> m EnergyRate
lookupEnergyRate uref = do
        Updates{..} <- refLoad uref
        StoreSerialized ChainParameters{..} <- refLoad currentParameters
        return _cpEnergyRate

-- |Look up the current chain parameters.
lookupCurrentParameters :: (MonadBlobStore m) => BufferedRef Updates -> m ChainParameters
lookupCurrentParameters uref = do
        Updates{..} <- refLoad uref
        unStoreSerialized <$> refLoad currentParameters

-- |Serialize updates in V0 format.
putUpdatesV0 :: (MonadBlobStore m, MonadPut m) => Updates -> m ()
putUpdatesV0 Updates{..} = do
        sPut . unStoreSerialized =<< refLoad currentAuthorizations
        case currentProtocolUpdate of
            Null -> liftPut $ putWord8 0
            Some pu -> do
                liftPut $ putWord8 1
                sPut . unStoreSerialized =<< refLoad pu
        sPut . unStoreSerialized =<< refLoad currentParameters
        putPendingUpdatesV0 pendingUpdates
