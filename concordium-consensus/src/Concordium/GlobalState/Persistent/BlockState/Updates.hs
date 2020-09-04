{-# LANGUAGE MonoLocalBinds, BangPatterns, ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-} -- TODO: get rid of this when possible
module Concordium.GlobalState.Persistent.BlockState.Updates where

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

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.Updates as Basic

data UpdateQueue e = UpdateQueue {
        uqNextSequenceNumber :: !UpdateSequenceNumber,
        uqQueue :: !(Seq.Seq (TransactionTime, HashedBufferedRef (StoreSerialized e)))
    }

instance (MonadBlobStore r m, Serialize e, MHashableTo m H.Hash e)
        => BlobStorable r m (UpdateQueue e) where
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

instance (BlobStorable r m e, Serialize e, MHashableTo m H.Hash e) => MHashableTo m H.Hash (UpdateQueue e) where
    getHashM UpdateQueue{..} = do
        q :: (Seq.Seq (TransactionTime, H.Hash)) <- mapM (\(tt, href) -> (tt,) <$> getHashM href) uqQueue
        return $! H.hash $ runPut $ do
            put uqNextSequenceNumber
            putLength $ length q
            mapM_ (\(t, h) -> put t >> put h) q

emptyUpdateQueue :: UpdateQueue e
emptyUpdateQueue = UpdateQueue {
        uqNextSequenceNumber = minUpdateSequenceNumber,
        uqQueue = Seq.empty
    }

makePersistentUpdateQueue :: (MonadIO m, MHashableTo m H.Hash (StoreSerialized e)) => Basic.UpdateQueue e -> m (UpdateQueue e)
makePersistentUpdateQueue Basic.UpdateQueue{..} = do
    let uqNextSequenceNumber = _uqNextSequenceNumber
    uqQueue <- Seq.fromList <$> forM _uqQueue (\(t, e) -> (t,) <$> makeHashedBufferedRef (StoreSerialized e))
    return UpdateQueue{..}

-- |Add an update event to an update queue, incrementing the sequence number.
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

data PendingUpdates = PendingUpdates {
        pAuthorizationQueue :: !(HashedBufferedRef (UpdateQueue Authorizations)),
        pProtocolQueue :: !(HashedBufferedRef (UpdateQueue ProtocolUpdate)),
        pElectionDifficultyQueue :: !(HashedBufferedRef (UpdateQueue ElectionDifficulty)),
        pEuroPerEnergyQueue :: !(HashedBufferedRef (UpdateQueue ExchangeRate)),
        pMicroGTUPerEuroQueue :: !(HashedBufferedRef (UpdateQueue ExchangeRate))
    }

instance (MonadBlobStore r m) => MHashableTo m H.Hash PendingUpdates where
    getHashM PendingUpdates{..} = do
        hAuthorizationQueue <- H.hashToByteString <$> getHashM pAuthorizationQueue
        hProtocolQueue <- H.hashToByteString <$> getHashM pProtocolQueue
        hElectionDifficultyQueue <- H.hashToByteString <$> getHashM pElectionDifficultyQueue
        hEuroPerEnergyQueue <- H.hashToByteString <$> getHashM pEuroPerEnergyQueue
        hMicroGTUPerEuroQueue <- H.hashToByteString <$> getHashM pMicroGTUPerEuroQueue
        return $! H.hash $
            hAuthorizationQueue
            <> hProtocolQueue
            <> hElectionDifficultyQueue
            <> hEuroPerEnergyQueue
            <> hMicroGTUPerEuroQueue

instance (MonadBlobStore r m)
        => BlobStorable r m PendingUpdates where
    storeUpdate PendingUpdates{..} = do
            (pAQ, aQ) <- storeUpdate pAuthorizationQueue
            (pPrQ, prQ) <- storeUpdate pProtocolQueue
            (pEDQ, edQ) <- storeUpdate pElectionDifficultyQueue
            (pEPEQ, epeQ) <- storeUpdate pEuroPerEnergyQueue
            (pMGTUPEQ, mgtupeQ) <- storeUpdate pMicroGTUPerEuroQueue
            let newPU = PendingUpdates {
                    pAuthorizationQueue = aQ,
                    pProtocolQueue = prQ,
                    pElectionDifficultyQueue = edQ,
                    pEuroPerEnergyQueue = epeQ,
                    pMicroGTUPerEuroQueue = mgtupeQ
                }
            return (pAQ >> pPrQ >> pEDQ >> pEPEQ >> pMGTUPEQ, newPU)
    store pu = fst <$> storeUpdate pu
    load = do
        mAQ <- label "Authorization update queue" load
        mPrQ <- label "Protocol update queue" load
        mEDQ <- label "Election difficulty update queue" load
        mEPEQ <- label "Euro per energy update queue" load
        mMGTUPEQ <- label "Micro GTU per Euro update queue" load
        return $! do
            pAuthorizationQueue <- mAQ
            pProtocolQueue <- mPrQ
            pElectionDifficultyQueue <- mEDQ
            pEuroPerEnergyQueue <- mEPEQ
            pMicroGTUPerEuroQueue <- mMGTUPEQ
            return PendingUpdates{..}

emptyPendingUpdates :: forall r m. (MonadBlobStore r m) => m PendingUpdates
emptyPendingUpdates = PendingUpdates <$> e <*> e <*> e <*> e <*> e
    where
        e :: MHashableTo m H.Hash (UpdateQueue a) => m (HashedBufferedRef (UpdateQueue a))
        e = makeHashedBufferedRef emptyUpdateQueue

makePersistentPendingUpdates :: (MonadBlobStore r m) => Basic.PendingUpdates -> m PendingUpdates
makePersistentPendingUpdates Basic.PendingUpdates{..} = do
        pAuthorizationQueue <- refMake =<< makePersistentUpdateQueue (_unhashed <$> _pAuthorizationQueue)
        pProtocolQueue <- refMake =<< makePersistentUpdateQueue _pProtocolQueue
        pElectionDifficultyQueue <- refMake =<< makePersistentUpdateQueue _pElectionDifficultyQueue
        pEuroPerEnergyQueue <- refMake =<< makePersistentUpdateQueue _pEuroPerEnergyQueue
        pMicroGTUPerEuroQueue <- refMake =<< makePersistentUpdateQueue _pMicroGTUPerEuroQueue
        return PendingUpdates{..}

data Updates = Updates {
        currentAuthorizations :: !(HashedBufferedRef (StoreSerialized Authorizations)),
        currentProtocolUpdate :: !(Nullable (HashedBufferedRef (StoreSerialized ProtocolUpdate))),
        currentParameters :: !(HashedBufferedRef (StoreSerialized ChainParameters)),
        pendingUpdates :: !PendingUpdates
    }

instance (MonadBlobStore r m) => MHashableTo m H.Hash Updates where
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

instance (MonadBlobStore r m)
        => BlobStorable r m Updates where
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


initialUpdates :: (MonadBlobStore r m) => Authorizations -> ChainParameters -> m Updates
initialUpdates auths chainParams = do
        currentAuthorizations <- makeHashedBufferedRef (StoreSerialized auths)
        let currentProtocolUpdate = Null
        currentParameters <- makeHashedBufferedRef (StoreSerialized chainParams)
        pendingUpdates <- emptyPendingUpdates
        return Updates{..}

makePersistentUpdates :: (MonadBlobStore r m) => Basic.Updates -> m Updates
makePersistentUpdates Basic.Updates{..} = do
        currentAuthorizations <- refMake (StoreSerialized (_unhashed _currentAuthorizations))
        currentProtocolUpdate <- case _currentProtocolUpdate of
            Nothing -> return Null
            Just pu -> Some <$> refMake (StoreSerialized pu)
        currentParameters <- refMake (StoreSerialized _currentParameters)
        pendingUpdates <- makePersistentPendingUpdates _pendingUpdates
        return Updates{..}

processValueUpdates ::
    Timestamp
    -> UpdateQueue v
    -> res
    -- ^No update continuation
    -> (HashedBufferedRef (StoreSerialized v) -> UpdateQueue v -> res)
    -- ^Update continuation
    -> res
processValueUpdates t uq noUpdate doUpdate = case ql of
                Seq.Empty -> noUpdate
                _ Seq.:|> (_, b) -> doUpdate b uq{uqQueue = qr}
    where
        (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue uq)

processAuthorizationUpdates :: (MonadBlobStore r m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processAuthorizationUpdates t bu = do
        u@Updates{..} <- refLoad bu
        authQueue <- refLoad (pAuthorizationQueue pendingUpdates)
        processValueUpdates t authQueue (return bu) $ \newAuths newQ -> do
            newpAuthQueue <- refMake newQ
            refMake u{
                    currentAuthorizations = newAuths,
                    pendingUpdates = pendingUpdates{pAuthorizationQueue = newpAuthQueue}
                }

processElectionDifficultyUpdates :: (MonadBlobStore r m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processElectionDifficultyUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pElectionDifficultyQueue pendingUpdates)
        processValueUpdates t oldQ (return bu) $ \newEDp newQ -> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newED <- refLoad newEDp
            newParameters <- refMake $ StoreSerialized $ oldCP & cpElectionDifficulty .~ newED
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pElectionDifficultyQueue = newpQ}
                }

processEuroPerEnergyUpdates :: (MonadBlobStore r m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processEuroPerEnergyUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pEuroPerEnergyQueue pendingUpdates)
        processValueUpdates t oldQ (return bu) $ \newEPEp newQ -> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newEPE <- refLoad newEPEp
            newParameters <- refMake $ StoreSerialized $ oldCP & cpEuroPerEnergy .~ newEPE
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pEuroPerEnergyQueue = newpQ}
                }

processMicroGTUPerEuroUpdates :: (MonadBlobStore r m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processMicroGTUPerEuroUpdates t bu = do
        u@Updates{..} <- refLoad bu
        oldQ <- refLoad (pMicroGTUPerEuroQueue pendingUpdates)
        processValueUpdates t oldQ (return bu) $ \newMGTUPEp newQ -> do
            newpQ <- refMake newQ
            StoreSerialized oldCP <- refLoad currentParameters
            StoreSerialized newMGTUPE <- refLoad newMGTUPEp
            newParameters <- refMake $ StoreSerialized $ oldCP & cpMicroGTUPerEuro .~ newMGTUPE
            refMake u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pMicroGTUPerEuroQueue = newpQ}
                }


processProtocolUpdates :: (MonadBlobStore r m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processProtocolUpdates t bu = do
        u@Updates{..} <- refLoad bu
        protQueue <- refLoad (pProtocolQueue pendingUpdates)
        let (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue protQueue)
        case ql of
            Seq.Empty -> return bu
            (_, pu) Seq.:<| _ -> do
                let newProtocolUpdate = case currentProtocolUpdate of
                        Null -> Some pu
                        s -> s
                newpProtQueue <- refMake protQueue {
                        uqQueue = qr
                    }
                refMake u {
                        currentProtocolUpdate = newProtocolUpdate,
                        pendingUpdates = pendingUpdates {pProtocolQueue = newpProtQueue}
                    }

processUpdateQueues :: (MonadBlobStore r m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processUpdateQueues t = processAuthorizationUpdates t
        >=> processProtocolUpdates t
        >=> processElectionDifficultyUpdates t
        >=> processEuroPerEnergyUpdates t
        >=> processMicroGTUPerEuroUpdates t

futureElectionDifficulty :: (MonadBlobStore r m) => BufferedRef Updates -> Timestamp -> m ElectionDifficulty
futureElectionDifficulty uref ts = do
        Updates{..} <- refLoad uref
        oldQ <- refLoad (pElectionDifficultyQueue pendingUpdates)
        let getCurED = do
                StoreSerialized cp <- refLoad currentParameters
                return $ cp ^. cpElectionDifficulty
        processValueUpdates ts oldQ getCurED (\newEDp _ -> unStoreSerialized <$> refLoad newEDp)

lookupNextUpdateSequenceNumber :: (MonadBlobStore r m) => BufferedRef Updates -> UpdateType -> m UpdateSequenceNumber
lookupNextUpdateSequenceNumber uref uty = do
        Updates{..} <- refLoad uref
        case uty of
            UpdateAuthorization -> uqNextSequenceNumber <$> refLoad (pAuthorizationQueue pendingUpdates)
            UpdateProtocol -> uqNextSequenceNumber <$> refLoad (pProtocolQueue pendingUpdates)
            UpdateElectionDifficulty -> uqNextSequenceNumber <$> refLoad (pElectionDifficultyQueue pendingUpdates)
            UpdateEuroPerEnergy -> uqNextSequenceNumber <$> refLoad (pEuroPerEnergyQueue pendingUpdates)
            UpdateMicroGTUPerEuro -> uqNextSequenceNumber <$> refLoad (pMicroGTUPerEuroQueue pendingUpdates)
        
enqueueUpdate :: (MonadBlobStore r m) => TransactionTime -> UpdatePayload -> BufferedRef Updates -> m (BufferedRef Updates)
enqueueUpdate effectiveTime payload uref = do
        u@Updates{pendingUpdates = p@PendingUpdates{..}} <- refLoad uref
        newPendingUpdates <- case payload of
            AuthorizationUpdatePayload auths -> enqueue effectiveTime auths pAuthorizationQueue <&> \newQ -> p{pAuthorizationQueue=newQ}
            ProtocolUpdatePayload auths -> enqueue effectiveTime auths pProtocolQueue <&> \newQ -> p{pProtocolQueue=newQ}
            ElectionDifficultyUpdatePayload auths -> enqueue effectiveTime auths pElectionDifficultyQueue <&> \newQ -> p{pElectionDifficultyQueue=newQ}
            EuroPerEnergyUpdatePayload auths -> enqueue effectiveTime auths pEuroPerEnergyQueue <&> \newQ -> p{pEuroPerEnergyQueue=newQ}
            MicroGTUPerEuroUpdatePayload auths -> enqueue effectiveTime auths pMicroGTUPerEuroQueue <&> \newQ -> p{pMicroGTUPerEuroQueue=newQ}
        refMake u{pendingUpdates = newPendingUpdates}