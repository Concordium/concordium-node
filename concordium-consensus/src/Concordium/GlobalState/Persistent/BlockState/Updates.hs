{-# LANGUAGE MonoLocalBinds, BangPatterns, ScopedTypeVariables #-}
module Concordium.GlobalState.Persistent.BlockState.Updates where

import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Serialize
import Control.Monad
import Control.Monad.IO.Class

import Concordium.Types
import Concordium.Types.Updates
import Concordium.Utils.Serialization

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.Updates as Basic

data UpdateQueue e = UpdateQueue {
        uqNextSequenceNumber :: !UpdateSequenceNumber,
        uqQueue :: !(Seq.Seq (Timestamp, BufferedRef (StoreSerialized e)))
    }

instance (MonadBlobStore m BlobRef, MonadIO m, Serialize e)
        => BlobStorable m BlobRef (UpdateQueue e) where
    storeUpdate p uq@UpdateQueue{..} = do
        l <- forM uqQueue $ \(t, r) -> do
            (pr, r') <- storeUpdate p r
            return (put t >> pr, (t, r'))
        let putUQ = do
                put uqNextSequenceNumber
                putLength $ Seq.length l
                sequence_ $ fst <$> l
        let uq' = uq{uqQueue = snd <$> l}
        return (putUQ, uq')
    store p uq = fst <$> storeUpdate p uq
    load p = do
        uqNextSequenceNumber <- get
        l <- getLength
        muqQueue <- Seq.replicateM l $ do
            t <- get
            e <- load p
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

-- |Try to add an update event to an update queue. Fails if the sequence number
-- is not correct.
checkEnqueue :: (MonadBlobStore m BlobRef, MonadIO m) 
    => UpdateSequenceNumber -> Timestamp -> e -> UpdateQueue e -> m (Maybe (UpdateQueue e))
checkEnqueue sn !t !e UpdateQueue{..}
    | sn == uqNextSequenceNumber = do
        eref <- makeBufferedRef (StoreSerialized e)
        -- Existing queued updates are only kept if their effective
        -- timestamp is lower than the new update.
        let !surviving = Seq.takeWhileL ((< t) . fst) uqQueue
        return $ Just $ UpdateQueue {
                uqNextSequenceNumber = sn + 1,
                uqQueue = surviving Seq.|> (t, eref)
            }
    | otherwise = return Nothing

data PendingUpdates = PendingUpdates {
        pAuthorizationQueue :: !(BufferedRef (UpdateQueue Authorizations)),
        pProtocolQueue :: !(BufferedRef (UpdateQueue ProtocolUpdate)),
        pElectionDifficultyQueue :: !(BufferedRef (UpdateQueue ElectionDifficulty)),
        pEuroPerEnergyQueue :: !(BufferedRef (UpdateQueue ExchangeRate)),
        pMicroGTUPerEuroQueue :: !(BufferedRef (UpdateQueue ExchangeRate))
    }

instance (MonadBlobStore m BlobRef, MonadIO m)
        => BlobStorable m BlobRef PendingUpdates where
    storeUpdate p PendingUpdates{..} = do
            (pAQ, aQ) <- storeUpdate p pAuthorizationQueue
            (pPrQ, prQ) <- storeUpdate p pProtocolQueue
            (pEDQ, edQ) <- storeUpdate p pElectionDifficultyQueue
            (pEPEQ, epeQ) <- storeUpdate p pEuroPerEnergyQueue
            (pMGTUPEQ, mgtupeQ) <- storeUpdate p pMicroGTUPerEuroQueue
            let newPU = PendingUpdates {
                    pAuthorizationQueue = aQ,
                    pProtocolQueue = prQ,
                    pElectionDifficultyQueue = edQ,
                    pEuroPerEnergyQueue = epeQ,
                    pMicroGTUPerEuroQueue = mgtupeQ
                }
            return (pAQ >> pPrQ >> pEDQ >> pEPEQ >> pMGTUPEQ, newPU)
    store p pu = fst <$> storeUpdate p pu
    load p = do
        mAQ <- label "Authorization update queue" $ load p
        mPrQ <- label "Protocol update queue" $ load p
        mEDQ <- label "Election difficulty update queue" $ load p
        mEPEQ <- label "Euro per energy update queue" $ load p
        mMGTUPEQ <- label "Micro GTU per Euro update queue" $ load p
        return $! do
            pAuthorizationQueue <- mAQ
            pProtocolQueue <- mPrQ
            pElectionDifficultyQueue <- mEDQ
            pEuroPerEnergyQueue <- mEPEQ
            pMicroGTUPerEuroQueue <- mMGTUPEQ
            return PendingUpdates{..}

emptyPendingUpdates :: (MonadIO m) => m PendingUpdates
emptyPendingUpdates = PendingUpdates <$> e <*> e <*> e
    where
        e = makeBufferedRef emptyUpdateQueue

makePersistentPendingUpdates :: (MonadIO m) => Basic.PendingUpdates -> m PendingUpdates
makePersistentPendingUpdates Basic.PendingUpdates{..} = do
        pAuthorizationQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pAuthorizationQueue
        pProtocolQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pProtocolQueue
        pParameterQueue <- makeBufferedRef =<< makePersistentUpdateQueue _pParameterQueue
        return PendingUpdates{..}


data Updates = Updates {
        currentAuthorizations :: !(BufferedRef (StoreSerialized Authorizations)),
        currentProtocolUpdate :: !(Nullable (BufferedRef (StoreSerialized ProtocolUpdate))),
        currentParameters :: !(BufferedRef (StoreSerialized ChainParameters)),
        pendingUpdates :: !PendingUpdates
    }

instance (MonadBlobStore m BlobRef, MonadIO m)
        => BlobStorable m BlobRef Updates where
    storeUpdate p Updates{..} = do
        (pCA, cA) <- storeUpdate p currentAuthorizations
        (pCPU, cPU) <- storeUpdate p currentProtocolUpdate
        (pCP, cP) <- storeUpdate p currentParameters
        (pPU, pU) <- storeUpdate p pendingUpdates
        let newUpdates = Updates{
                currentAuthorizations = cA,
                currentProtocolUpdate = cPU,
                currentParameters = cP,
                pendingUpdates = pU
            }
        return (pCA >> pCPU >> pCP >> pPU, newUpdates)
    store p u = fst <$> storeUpdate p u
    load p = do
        mCA <- label "Current authorizations" $ load p
        mCPU <- label "Current protocol update" $ load p
        mCP <- label "Current parameters" $ load p
        mPU <- label "Pending updates" $ load p
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

processAuthorizationUpdates :: (MonadBlobStore m BlobRef, MonadIO m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processAuthorizationUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        authQueue <- loadBufferedRef (pAuthorizationQueue pendingUpdates)
        let (ql, qr) = Seq.spanl ((<= t) . fst) (uqQueue authQueue)
        case ql of
            Seq.Empty -> return bu
            _ Seq.:|> (_, b) -> do
                newpAuthQueue <- makeBufferedRef authQueue {
                        uqQueue = qr
                    }
                makeBufferedRef u {
                        currentAuthorizations = b,
                        pendingUpdates = pendingUpdates {pAuthorizationQueue = newpAuthQueue}
                    }

processProtocolUpdates :: (MonadBlobStore m BlobRef, MonadIO m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processProtocolUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        protQueue <- loadBufferedRef (pProtocolQueue pendingUpdates)
        let (ql, qr) = Seq.spanl ((<= t) . fst) (uqQueue protQueue)
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

processParameterUpdates :: (MonadBlobStore m BlobRef, MonadIO m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processParameterUpdates t bu = do
        u@Updates{..} <- loadBufferedRef bu
        paramQueue <- loadBufferedRef (pParameterQueue pendingUpdates)
        let (ql, qr) = Seq.spanl ((<= t) . fst) (uqQueue paramQueue)
        case ql of
            Seq.Empty -> return bu
            _ Seq.:|> (_, pu) -> do
                StoreSerialized ParameterUpdate{..} <- loadBufferedRef pu
                StoreSerialized ChainParameters{..} <- loadBufferedRef currentParameters
                newParameters <- makeBufferedRef $ StoreSerialized $ makeChainParameters
                        (fromMaybe _cpElectionDifficulty puElectionDifficulty)
                        (fromMaybe _cpEuroPerEnergy puEuroPerEnergy)
                        (fromMaybe _cpMicroGTUPerEuro puMicroGTUPerEuro)
                newpParameterQueue <- makeBufferedRef paramQueue {
                        uqQueue = qr
                    }
                makeBufferedRef u {
                        currentParameters = newParameters,
                        pendingUpdates = pendingUpdates {pParameterQueue = newpParameterQueue}
                    }

processUpdateQueues :: (MonadBlobStore m BlobRef, MonadIO m) => Timestamp -> BufferedRef Updates -> m (BufferedRef Updates)
processUpdateQueues t = processAuthorizationUpdates t >=> processProtocolUpdates t >=> processParameterUpdates t

futureElectionDifficulty :: (MonadBlobStore m BlobRef, MonadIO m) => BufferedRef Updates -> Timestamp -> m ElectionDifficulty
futureElectionDifficulty uref ts = do
        Updates{..} <- loadBufferedRef uref
        paramQueue <- loadBufferedRef (pParameterQueue pendingUpdates)
        let elapsedUpdates = Seq.takeWhileL ((<= ts) . fst) (uqQueue paramQueue)
            latestUpdate Seq.Empty = do
                StoreSerialized ChainParameters{..} <- loadBufferedRef currentParameters
                return _cpElectionDifficulty
            latestUpdate (l Seq.:|> (_, ref)) = do
                StoreSerialized ParameterUpdate{..} <- loadBufferedRef ref
                case puElectionDifficulty of
                    Nothing -> latestUpdate l
                    Just ed -> return ed
        latestUpdate elapsedUpdates

lookupNextUpdateSequenceNumber :: forall m. (MonadBlobStore m BlobRef, MonadIO m) => BufferedRef Updates -> UpdateType -> m UpdateSequenceNumber
lookupNextUpdateSequenceNumber uref uty = do
        Updates{..} <- loadBufferedRef uref
        case uty of
            UpdateAuthorization -> uqNextSequenceNumber <$> loadBufferedRef (pAuthorizationQueue pendingUpdates)
            UpdateProtocol -> uqNextSequenceNumber <$> loadBufferedRef (pProtocolQueue pendingUpdates)
            UpdateParameters -> uqNextSequenceNumber <$> loadBufferedRef (pParameterQueue pendingUpdates)
        
