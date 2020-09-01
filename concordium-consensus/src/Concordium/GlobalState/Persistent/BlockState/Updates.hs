{-# LANGUAGE MonoLocalBinds, BangPatterns, ScopedTypeVariables #-}
module Concordium.GlobalState.Persistent.BlockState.Updates where

import qualified Data.Sequence as Seq
import Data.Serialize
import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Updates
import Concordium.Utils.Serialization

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.Updates as Basic

data UpdateQueue e = UpdateQueue {
        uqNextSequenceNumber :: !UpdateSequenceNumber,
        uqQueue :: !(Seq.Seq (TransactionTime, BufferedRef (StoreSerialized e)))
    }

instance (MonadBlobStore m, Serialize e)
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

emptyUpdateQueue :: UpdateQueue e
emptyUpdateQueue = UpdateQueue {
        uqNextSequenceNumber = minUpdateSequenceNumber,
        uqQueue = Seq.empty
    }

makePersistentUpdateQueue :: (MonadIO m) => Basic.UpdateQueue e -> m (UpdateQueue e)
makePersistentUpdateQueue Basic.UpdateQueue{..} = do
    let uqNextSequenceNumber = _uqNextSequenceNumber
    uqQueue <- Seq.fromList <$> forM _uqQueue (\(t, e) -> (t,) <$> makeBufferedRef (StoreSerialized e))
    return UpdateQueue{..}

-- |Add an update event to an update queue, incrementing the sequence number.
enqueue :: (MonadBlobStore m, Serialize e)
    => TransactionTime -> e -> BufferedRef (UpdateQueue e) -> m (BufferedRef (UpdateQueue e))
enqueue !t !e q = do
        UpdateQueue{..} <- loadBufferedRef q
        eref <- makeBufferedRef (StoreSerialized e)
        -- Existing queued updates are only kept if their effective
        -- timestamp is lower than the new update.
        let !surviving = Seq.takeWhileL ((< t) . fst) uqQueue
        makeBufferedRef $ UpdateQueue {
                uqNextSequenceNumber = uqNextSequenceNumber + 1,
                uqQueue = surviving Seq.|> (t, eref)
            }

data PendingUpdates = PendingUpdates {
        pAuthorizationQueue :: !(BufferedRef (UpdateQueue Authorizations)),
        pProtocolQueue :: !(BufferedRef (UpdateQueue ProtocolUpdate)),
        pElectionDifficultyQueue :: !(BufferedRef (UpdateQueue ElectionDifficulty)),
        pEuroPerEnergyQueue :: !(BufferedRef (UpdateQueue ExchangeRate)),
        pMicroGTUPerEuroQueue :: !(BufferedRef (UpdateQueue ExchangeRate))
    }

instance (MonadBlobStore m)
        => BlobStorable m PendingUpdates where
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

emptyPendingUpdates :: (MonadIO m) => m PendingUpdates
emptyPendingUpdates = PendingUpdates <$> e <*> e <*> e <*> e <*> e
    where
        e = makeBufferedRef emptyUpdateQueue

makePersistentPendingUpdates :: (MonadIO m) => Basic.PendingUpdates -> m PendingUpdates
makePersistentPendingUpdates Basic.PendingUpdates{..} = do
        pAuthorizationQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pAuthorizationQueue
        pProtocolQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pProtocolQueue
        pElectionDifficultyQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pElectionDifficultyQueue
        pEuroPerEnergyQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pEuroPerEnergyQueue
        pMicroGTUPerEuroQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pMicroGTUPerEuroQueue
        return PendingUpdates{..}

data Updates = Updates {
        currentAuthorizations :: !(BufferedRef (StoreSerialized Authorizations)),
        currentProtocolUpdate :: !(Nullable (BufferedRef (StoreSerialized ProtocolUpdate))),
        currentParameters :: !(BufferedRef (StoreSerialized ChainParameters)),
        pendingUpdates :: !PendingUpdates
    }

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


initialUpdates :: (MonadIO m) => Authorizations -> ChainParameters -> m Updates
initialUpdates auths chainParams = do
        currentAuthorizations <- makeBufferedRef (StoreSerialized auths)
        let currentProtocolUpdate = Null
        currentParameters <- makeBufferedRef (StoreSerialized chainParams)
        pendingUpdates <- emptyPendingUpdates
        return Updates{..}

makePersistentUpdates :: (MonadIO m) => Basic.Updates -> m Updates
makePersistentUpdates Basic.Updates{..} = do
        currentAuthorizations <- makeBufferedRef (StoreSerialized _currentAuthorizations)
        currentProtocolUpdate <- case _currentProtocolUpdate of
            Nothing -> return Null
            Just pu -> Some <$> makeBufferedRef (StoreSerialized pu)
        currentParameters <- makeBufferedRef (StoreSerialized _currentParameters)
        pendingUpdates <- makePersistentPendingUpdates _pendingUpdates
        return Updates{..}

processValueUpdates ::
    Timestamp
    -> UpdateQueue v
    -> res
    -- ^No update continuation
    -> (BufferedRef (StoreSerialized v) -> UpdateQueue v -> res)
    -- ^Update continuation
    -> res
processValueUpdates t uq noUpdate doUpdate = case ql of
                Seq.Empty -> noUpdate
                _ Seq.:|> (_, b) -> doUpdate b uq{uqQueue = qr}
    where
        (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue uq)

processAuthorizationUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processAuthorizationUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        authQueue <- loadBufferedRef (pAuthorizationQueue pendingUpdates)
        processValueUpdates t authQueue (return bu) $ \newAuths newQ -> do
            newpAuthQueue <- makeBufferedRef newQ
            makeBufferedRef u{
                    currentAuthorizations = newAuths,
                    pendingUpdates = pendingUpdates{pAuthorizationQueue = newpAuthQueue}
                }

processElectionDifficultyUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processElectionDifficultyUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        oldQ <- loadBufferedRef (pElectionDifficultyQueue pendingUpdates)
        processValueUpdates t oldQ (return bu) $ \newEDp newQ -> do
            newpQ <- makeBufferedRef newQ
            StoreSerialized oldCP <- loadBufferedRef currentParameters
            StoreSerialized newED <- loadBufferedRef newEDp
            newParameters <- makeBufferedRef $ StoreSerialized $ oldCP & cpElectionDifficulty .~ newED
            makeBufferedRef u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pElectionDifficultyQueue = newpQ}
                }

processEuroPerEnergyUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processEuroPerEnergyUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        oldQ <- loadBufferedRef (pEuroPerEnergyQueue pendingUpdates)
        processValueUpdates t oldQ (return bu) $ \newEPEp newQ -> do
            newpQ <- makeBufferedRef newQ
            StoreSerialized oldCP <- loadBufferedRef currentParameters
            StoreSerialized newEPE <- loadBufferedRef newEPEp
            newParameters <- makeBufferedRef $ StoreSerialized $ oldCP & cpEuroPerEnergy .~ newEPE
            makeBufferedRef u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pEuroPerEnergyQueue = newpQ}
                }

processMicroGTUPerEuroUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processMicroGTUPerEuroUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        oldQ <- loadBufferedRef (pMicroGTUPerEuroQueue pendingUpdates)
        processValueUpdates t oldQ (return bu) $ \newMGTUPEp newQ -> do
            newpQ <- makeBufferedRef newQ
            StoreSerialized oldCP <- loadBufferedRef currentParameters
            StoreSerialized newMGTUPE <- loadBufferedRef newMGTUPEp
            newParameters <- makeBufferedRef $ StoreSerialized $ oldCP & cpMicroGTUPerEuro .~ newMGTUPE
            makeBufferedRef u{
                    currentParameters = newParameters,
                    pendingUpdates = pendingUpdates{pMicroGTUPerEuroQueue = newpQ}
                }


processProtocolUpdates :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processProtocolUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        protQueue <- loadBufferedRef (pProtocolQueue pendingUpdates)
        let (ql, qr) = Seq.spanl ((<= t) . transactionTimeToTimestamp . fst) (uqQueue protQueue)
        case ql of
            Seq.Empty -> return bu
            (_, pu) Seq.:<| _ -> do
                let newProtocolUpdate = case currentProtocolUpdate of
                        Null -> Some pu
                        s -> s
                newpProtQueue <- makeBufferedRef protQueue {
                        uqQueue = qr
                    }
                makeBufferedRef u {
                        currentProtocolUpdate = newProtocolUpdate,
                        pendingUpdates = pendingUpdates {pProtocolQueue = newpProtQueue}
                    }

processUpdateQueues :: (MonadBlobStore m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processUpdateQueues t = processAuthorizationUpdates t
        >=> processProtocolUpdates t
        >=> processElectionDifficultyUpdates t
        >=> processEuroPerEnergyUpdates t
        >=> processMicroGTUPerEuroUpdates t

futureElectionDifficulty :: (MonadBlobStore m) => BufferedRef Updates -> Timestamp -> m ElectionDifficulty
futureElectionDifficulty uref ts = do
        Updates{..} <- loadBufferedRef uref
        oldQ <- loadBufferedRef (pElectionDifficultyQueue pendingUpdates)
        let getCurED = do
                StoreSerialized cp <- loadBufferedRef currentParameters
                return $ cp ^. cpElectionDifficulty
        processValueUpdates ts oldQ getCurED (\newEDp _ -> unStoreSerialized <$> loadBufferedRef newEDp)

lookupNextUpdateSequenceNumber :: (MonadBlobStore m) => BufferedRef Updates -> UpdateType -> m UpdateSequenceNumber
lookupNextUpdateSequenceNumber uref uty = do
        Updates{..} <- loadBufferedRef uref
        case uty of
            UpdateAuthorization -> uqNextSequenceNumber <$> loadBufferedRef (pAuthorizationQueue pendingUpdates)
            UpdateProtocol -> uqNextSequenceNumber <$> loadBufferedRef (pProtocolQueue pendingUpdates)
            UpdateElectionDifficulty -> uqNextSequenceNumber <$> loadBufferedRef (pElectionDifficultyQueue pendingUpdates)
            UpdateEuroPerEnergy -> uqNextSequenceNumber <$> loadBufferedRef (pEuroPerEnergyQueue pendingUpdates)
            UpdateMicroGTUPerEuro -> uqNextSequenceNumber <$> loadBufferedRef (pMicroGTUPerEuroQueue pendingUpdates)
        
enqueueUpdate :: (MonadBlobStore m) => TransactionTime -> UpdatePayload -> BufferedRef Updates -> m (BufferedRef Updates)
enqueueUpdate effectiveTime payload uref = do
        u@Updates{pendingUpdates = p@PendingUpdates{..}} <- loadBufferedRef uref
        newPendingUpdates <- case payload of
            AuthorizationUpdatePayload auths -> enqueue effectiveTime auths pAuthorizationQueue <&> \newQ -> p{pAuthorizationQueue=newQ}
            ProtocolUpdatePayload auths -> enqueue effectiveTime auths pProtocolQueue <&> \newQ -> p{pProtocolQueue=newQ}
            ElectionDifficultyUpdatePayload auths -> enqueue effectiveTime auths pElectionDifficultyQueue <&> \newQ -> p{pElectionDifficultyQueue=newQ}
            EuroPerEnergyUpdatePayload auths -> enqueue effectiveTime auths pEuroPerEnergyQueue <&> \newQ -> p{pEuroPerEnergyQueue=newQ}
            MicroGTUPerEuroUpdatePayload auths -> enqueue effectiveTime auths pMicroGTUPerEuroQueue <&> \newQ -> p{pMicroGTUPerEuroQueue=newQ}
        makeBufferedRef u{pendingUpdates = newPendingUpdates}