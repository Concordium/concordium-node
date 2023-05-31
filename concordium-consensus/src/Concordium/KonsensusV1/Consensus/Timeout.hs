{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Timeout where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.List (union)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Lens.Micro.Platform

import Concordium.Genesis.Data.BaseV1
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import Concordium.KonsensusV1.Consensus.Finality (checkFinality)
import Concordium.KonsensusV1.Consensus.Timeout.Internal
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimerMonad

-- |Reasons that a 'TimeoutMessage' can be rejected.
data ReceiveTimeoutMessageRejectReason
    = -- |The 'Round' presented in the 'TimeoutMessage' is obsolete.
      ObsoleteRound
    | -- | The 'QuorumCertificate' associated with the 'TimeoutMessage' is for
      -- either an obsolete 'Round' or 'Epoch'.
      ObsoleteQC
    | -- |The signer of the 'TimeoutMessage' is not a finalizer for the
      -- current 'Epoch'.
      NotAFinalizer
    | -- |The signature on the 'TimeoutMessage' is invalid.
      InvalidSignature
    | -- |The finalizer already signed a 'TimeoutMessage' for the
      -- current round.
      DoubleSigning
    | -- |The 'QuorumCertificate' is pointing to a block prior
      -- to the last finalized block.
      ObsoleteQCPointer
    | -- |The 'QuorumCertificate' is pointing to a dead block.
      DeadQCPointer
    | -- |The epoch of the 'QuorumCertificate' does not agree with the epoch of an existing
      -- certificate for that round.
      BadQCEpoch
    | -- |The 'TimeoutMessage' is a duplicate.
      Duplicate
    deriving (Eq, Show)

-- |Possibly return codes for when receiving
-- a 'TimeoutMessage'.
data ReceiveTimeoutMessageResult pv
    = -- |The 'TimeoutMessage' was well received and should
      -- be relayed onto the network.
      Received !(PartiallyVerifiedTimeoutMessage pv)
    | -- |The 'TimeoutMessage' could not be verified and should not be
      -- relayed.
      Rejected !ReceiveTimeoutMessageRejectReason
    | -- |The consensus runner needs to catch up before processing the
      -- 'TimeoutMessage'.
      CatchupRequired
    deriving (Eq, Show)

-- |A partially verified 'TimeoutMessage' with its associated finalization committees.
-- The timeout message is partially verified itself but the aggregate signature and
-- associated quorum certificate are not.
data PartiallyVerifiedTimeoutMessage pv = PartiallyVerifiedTimeoutMessage
    { -- |The 'TimeoutMessage' that has been partially verified
      pvtmTimeoutMessage :: !TimeoutMessage,
      -- |The finalization committee with respect to the 'QuorumCertificate' contained
      -- in the 'TimeoutMessage'.
      pvtmQuorumFinalizers :: !FinalizationCommittee,
      -- |Whether the aggregate signature is valid.
      -- This is intentionally lazy, as forcing this will perform the signature check.
      pvtmAggregateSignatureValid :: Bool,
      -- |Block pointer for the block referenced by the 'QuorumCertificate' of the 'TimeoutMessage'.
      -- This is @Absent@ when the block that the 'QuorumCertificate' refers to is either 'BlockPending' or 'BlockUnknown'.
      pvtmBlock :: !(Option (BlockPointer pv))
    }
    deriving (Eq, Show)

makeLenses ''PartiallyVerifiedTimeoutMessage

-- |Receive and verify the basics of a 'TimeoutMessage' with respect to
-- the supplied tree state.
-- If this function returns @Received PartiallyVerifiedTimeoutMessage@ then 'executeTimeoutMessage' MUST
-- be invoked immediately after relaying the message. Hence there must be no changes to the tree state in the mean time.
receiveTimeoutMessage ::
    LowLevel.MonadTreeStateStore m =>
    -- |The 'TimeoutMessage' to receive.
    TimeoutMessage ->
    -- |The tree state to verify the 'TimeoutMessage' within.
    SkovData (MPV m) ->
    -- |Result of receiving the 'TimeoutMessage'.
    m (ReceiveTimeoutMessageResult (MPV m))
receiveTimeoutMessage tm@TimeoutMessage{tmBody = TimeoutMessageBody{..}} skovData
    --  The round of the 'TimeoutMessage' is obsolete.
    | tmRound < currentRound =
        return $ Rejected ObsoleteRound
    -- If the round or epoch of the qc associated with the timeout message
    -- is behind the last finalized block then reject the timeout message.
    -- This happens in case of dishonest behavior as it is checked above that the round of
    -- the timeout message is greater or equal to the current round, hence an honest
    -- sender of the timeout message must have progressed their QC to at least the
    -- last finalized block.
    | let lastFin = skovData ^. lastFinalized,
      qcRound tmQuorumCertificate < blockRound lastFin
        || qcEpoch tmQuorumCertificate < blockEpoch lastFin =
        return $ Rejected ObsoleteQC
    -- We check if the epoch that the sender is on is ahead of our current epoch.
    -- It could be that the QC in the timeout message allows us to advance to the epoch, but
    -- instead we just trigger catch-up. While this is less efficient in that case, we opt for
    -- this simpler implementation as it is sufficient and the case is not likely to be common.
    | tmEpoch > theCurrentEpoch =
        return CatchupRequired
    -- Obtain the finalizer information for the signer of the timeout message.
    | otherwise = withFinalizers $ \finalizationCommittee ->
        withFinalizer finalizationCommittee $ \case
            finInfo@FinalizerInfo{..}
                -- Check whether the signature is ok or not.
                | not (checkTimeoutMessageSignature finalizerSignKey genesisBlockHash tm) ->
                    return $ Rejected InvalidSignature
                -- Consensus runner is not caught up to the round that the timeout message
                -- refers to. So catch up is required.
                | tmRound > currentRound,
                  qcRound tmQuorumCertificate < tmRound - 1 ->
                    return CatchupRequired
                | otherwise ->
                    checkForDuplicate $ do
                        -- Check whether we have already checked a QC for the round of the quorum certificate
                        -- associated with the timeout message and that the qc round is higher (with respect to round number) than
                        -- our highest certified block.
                        let existingQC = skovData ^. roundExistingQuorumCertificate (qcRound tmQuorumCertificate)
                            oldOrCurrentHighestCertifiedQCRound = qcRound tmQuorumCertificate <= skovData ^. roundStatus . rsHighestCertifiedBlock . to cbRound
                        case (existingQC, oldOrCurrentHighestCertifiedQCRound) of
                            (Just (QuorumCertificateCheckedWitness certEpoch), True) -> do
                                -- The epochs must match otherwise we reject.
                                if qcEpoch tmQuorumCertificate == certEpoch
                                    then received finInfo finalizationCommittee Absent
                                    else return $ Rejected BadQCEpoch
                            -- If we have not already checked a QC for the round of the quorum certificate assoicated
                            -- with the timeout message.
                            _ -> do
                                getRecentBlockStatus (qcBlock tmQuorumCertificate) skovData >>= \case
                                    -- In this case, the quorum certificate cannot be valid, because otherwise
                                    -- it would already have been rejected for an obsolete QC.
                                    OldFinalized ->
                                        return $ Rejected ObsoleteQCPointer
                                    -- The QC pointer points to a block that has been marked dead.
                                    -- (Dead blocks are either invalid or branch from a pruned block.)
                                    -- Here also the QC cannot be valid.
                                    RecentBlock BlockDead ->
                                        return $ Rejected DeadQCPointer
                                    -- The timeout message is now verified and ready for being retransmitted.
                                    RecentBlock (BlockAliveOrFinalized qcBlock) ->
                                        received finInfo finalizationCommittee (Present qcBlock)
                                    -- If the block is pending or unknown, then due to the checks above
                                    -- we know that we have to initiate catch up since the round of the quorum certificate
                                    -- is greater than our highest certified block.
                                    RecentBlock BlockPendingOrUnknown -> return CatchupRequired
  where
    -- Get the bakers and finalizers for the epoch of the timeout message's QC.
    -- If they are not available, trigger catch-up.
    withFinalizers cont = case getBakersForEpoch (qcEpoch tmQuorumCertificate) skovData of
        -- Since we have checked that @tmEpoch <= theCurrentEpoch@ and by definition
        -- we have @tmEpoch >= qcEpoch@ then if the finalizers cannot be retrieved
        -- it must mean that the timeout message is from the past.
        Nothing -> return $ Rejected ObsoleteQC
        Just bakers -> cont (bakers ^. bfFinalizers)
    -- Look up the finalizer in the finalization committee.
    -- Reject with 'NotAFinalizer' if the finalizer index is not valid in the committee.
    withFinalizer finalizers cont = case finalizerByIndex finalizers tmFinalizerIndex of
        Nothing -> return $ Rejected NotAFinalizer
        Just finalizer -> cont finalizer
    -- Check whether this 'TimeoutMessage' is a duplicate or an instance
    -- of double signing.
    -- If neither of these are the cases we continue verifying the
    -- timeout message.
    checkForDuplicate cont =
        case getExistingMessage of
            -- The message is a duplicate. Check for double signing.
            Just existingMessage
                | existingMessage == tm -> return $ Rejected Duplicate
                | otherwise -> do
                    -- The finalizer has already sent a timeout message for this round, this is not
                    -- allowed so the behaviour is flagged and timeout message is rejected.
                    flag $! TimeoutDoubleSigning tm existingMessage
                    return $ Rejected DoubleSigning
            -- Call the continuation
            Nothing -> cont
    -- The timeout message is received and ready for
    -- retransmission.
    received finalizerInfo qcFinalizationCommittee mBlock =
        return $
            Received $
                PartiallyVerifiedTimeoutMessage
                    { pvtmTimeoutMessage = tm,
                      pvtmQuorumFinalizers = qcFinalizationCommittee,
                      pvtmAggregateSignatureValid =
                        checkTimeoutSignatureSingle
                            (tmSignatureMessage genesisBlockHash (tmBody tm))
                            (finalizerBlsKey finalizerInfo)
                            tmAggregateSignature,
                      pvtmBlock = mBlock
                    }
    -- Get an existing message if present otherwise return nothing.
    getExistingMessage = case skovData ^. currentTimeoutMessages of
        Present messages
            | tmFirstEpoch messages == qcEpoch tmQuorumCertificate ->
                tmFirstEpochTimeouts messages ^? ix tmFinalizerIndex
            | tmFirstEpoch messages + 1 == qcEpoch tmQuorumCertificate ->
                tmSecondEpochTimeouts messages ^? ix tmFinalizerIndex
        _ -> Nothing
    -- The genesis block hash.
    genesisBlockHash = skovData ^. genesisMetadata . to gmCurrentGenesisHash
    -- The current round with respect to the tree state supplied.
    currentRound = skovData ^. roundStatus . rsCurrentRound
    -- The current epoch with respect to the tree state supplied.
    theCurrentEpoch = skovData ^. roundStatus . rsCurrentEpoch

-- |The result of executing a 'TimeoutMessage'.
data ExecuteTimeoutMessageResult
    = -- |The 'TimeoutMessage' was successfully executed.
      ExecutionSuccess
    | -- |The 'AggregateSiganture' is incorrect.
      InvalidAggregateSignature
    | -- |The 'QuorumCertificate' for the 'TimeoutMessage'
      -- is invalid.
      InvalidQC !QuorumCertificate
    deriving (Eq, Show)

-- |Execute a 'PartiallyVerifiedTimeoutMessage' that has been _received_ ('receiveTimeoutMessage') prior to
-- this call.
--
-- This function verifies the 'QuorumCertificate' and possibly advances the round.
-- This function also makes sure the check whether a block can be finalized due to the
-- 'QuorumCertificate' of the 'TimeoutMessage'.
executeTimeoutMessage ::
    ( IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      MonadProtocolVersion m,
      BlockStateStorage m,
      TimeMonad m,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m,
      MonadConsensusEvent m,
      MonadLogger m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      TimerMonad m,
      MonadBroadcast m,
      MonadReader r m,
      HasBakerContext r
    ) =>
    -- |The partially verified 'TimeoutMessage' to execute.
    PartiallyVerifiedTimeoutMessage (MPV m) ->
    -- |Returns @Left TimeoutMessage@ if the 'QuorumCertificate' could not be verified,
    -- and otherwise @Right ()@.
    m ExecuteTimeoutMessageResult
executeTimeoutMessage (PartiallyVerifiedTimeoutMessage{..})
    -- Check the aggregate signature of the timeout message.
    | not pvtmAggregateSignatureValid = do
        flag $ InvalidTimeoutSignature pvtmTimeoutMessage
        return InvalidAggregateSignature
    -- Note that we now know that the epoch of the qc is valid as
    -- otherwise it would've been rejected in 'receiveTimeoutMessage'.

    -- Note: deserialization of the 'TimeoutMessageBody' checks the following
    -- - that the timeout round and QC round are coherent, so we do not need to check that here.
    -- - that @tmEpoch >= qcEpoch tmQuorumCertificate@, so we do not need to check that here.
    | Absent <- pvtmBlock = do
        -- In this case, we have already checked a valid QC for the round and epoch of the timeout
        -- message, but the message is on either a 'BlockPending' or 'BlockUnknown' block.
        -- We just need to process the timeout.
        processTimeout pvtmTimeoutMessage
        return ExecutionSuccess
    | Present block <- pvtmBlock = do
        -- In this case we have not checked a QC for the 'Round' and 'Epoch' so we need to
        -- check it now.
        checkQC $ do
            -- Record that we've checked the QC.
            recordCheckedQuorumCertificate tmQuorumCertificate
            -- The quorum certificate is valid so check whether it finalises any blocks.
            checkFinality tmQuorumCertificate
            -- Advance the round if we can advance by the quorum certificate.
            -- Note that we have either @currentRound == tmRound@ or
            -- @currentRound < tmRound && tmRound - 1 == qcRound tmQuorumCertificate@
            -- In the latter case we advance by the quorum certificate associated
            -- with the timeout message and the @currentRound@ becomes
            -- @1 + qcRound tmQuorumCertificate@, hence @currentRound@ becomes @tmRound@.
            currentRound <- use $ roundStatus . rsCurrentRound
            let newCertifiedBlock =
                    CertifiedBlock
                        { cbQuorumCertificate = tmQuorumCertificate,
                          cbQuorumBlock = block
                        }
            if currentRound <= qcRound tmQuorumCertificate
                then -- Advance the round with the new certified block.
                    advanceRoundWithQuorum newCertifiedBlock
                else -- Otherwise we just update the highest certified block.
                    checkedUpdateHighestCertifiedBlock newCertifiedBlock
            -- Process the timeout message
            processTimeout pvtmTimeoutMessage
            return ExecutionSuccess
  where
    TimeoutMessageBody{..} = tmBody pvtmTimeoutMessage
    -- Check the quorum certificate of the timeout message.
    checkQC cont = do
        genesisBlockHash <- use $ genesisMetadata . to gmCurrentGenesisHash
        signatureThreshold <- use $ genesisMetadata . to gmParameters . to genesisSignatureThreshold
        let quorumCertOK =
                checkQuorumCertificate
                    genesisBlockHash
                    (toRational signatureThreshold)
                    pvtmQuorumFinalizers
                    tmQuorumCertificate
        if quorumCertOK
            then cont
            else do
                flag $! TimeoutMessageInvalidQC pvtmTimeoutMessage
                return $! InvalidQC tmQuorumCertificate

-- |This is 'uponTimeoutEvent' from the bluepaper. If a timeout occurs, a finalizers should call
-- this function to generate a timeout message, send it out, and process it.
-- NB: If the caller is not a finalizer, this function does nothing.
-- Precondition: This function MUST have exclusive write permission to the tree state.
uponTimeoutEvent ::
    ( MonadTimeout m,
      MonadBroadcast m,
      MonadReader r m,
      HasBakerContext r,
      MonadProtocolVersion m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m,
      MonadLogger m,
      BlockStateStorage m,
      TimeMonad m,
      TimerMonad m,
      MonadThrow m,
      MonadIO m,
      MonadConsensusEvent m
    ) =>
    m ()
uponTimeoutEvent = do
    currentRoundStatus <- use roundStatus
    -- We need to timeout the round in the epoch of the highest QC, which may differ from our
    -- current epoch.
    let highestQC = currentRoundStatus ^. rsHighestCertifiedBlock . to cbQuorumCertificate
    withFinalizerForEpoch (qcEpoch highestQC) $ \BakerIdentity{..} finInfo -> do
        -- We do not directly update the next signable round, as that is implicitly updated by
        -- setting 'rsLastSignedTimeoutMessage'.
        lastFinBlockPtr <- use lastFinalized
        growTimeout lastFinBlockPtr

        genesisHash <- use currentGenesisHash
        let curRound = _rsCurrentRound currentRoundStatus

        let timeoutSigMessage =
                TimeoutSignatureMessage
                    { tsmGenesis = genesisHash,
                      tsmRound = curRound,
                      tsmQCRound = qcRound highestQC,
                      tsmQCEpoch = qcEpoch highestQC
                    }
        let timeoutSig = signTimeoutSignatureMessage timeoutSigMessage bakerAggregationKey
        curEpoch <- use $ roundStatus . rsCurrentEpoch
        let timeoutMessageBody =
                TimeoutMessageBody
                    { tmFinalizerIndex = finalizerIndex finInfo,
                      tmRound = curRound,
                      tmEpoch = curEpoch,
                      tmQuorumCertificate = highestQC,
                      tmAggregateSignature = timeoutSig
                    }
        let timeoutMessage = signTimeoutMessage timeoutMessageBody genesisHash bakerSignKey
        updatePersistentRoundStatus $
            prsLastSignedTimeoutMessage .~ Present timeoutMessage
        sendTimeoutMessage timeoutMessage
        processTimeout timeoutMessage

-- |Add a 'TimeoutMessage' to an existing set of timeout messages. Returns 'Nothing' if there is
-- no change (i.e. the new message was from an epoch that is too early).
-- The supplied timeout messages can be 'Absent' (i.e. there are no messages), but the return value
-- is only 'Nothing' when there is no change: if the timeout messages are updated, they will
-- always be non-empty (in particular, including the new message).
updateTimeoutMessages ::
    Option TimeoutMessages ->
    TimeoutMessage ->
    Maybe TimeoutMessages
updateTimeoutMessages tms tm =
    case tms of
        Absent ->
            Just $
                TimeoutMessages
                    { tmFirstEpoch = epoch,
                      tmFirstEpochTimeouts = singletonTimeout,
                      tmSecondEpochTimeouts = Map.empty
                    }
        Present TimeoutMessages{..}
            | epoch == tmFirstEpoch ->
                Just $
                    TimeoutMessages
                        { tmFirstEpochTimeouts = insertTimeout tmFirstEpochTimeouts,
                          ..
                        }
            | epoch == tmFirstEpoch + 1 ->
                Just $
                    TimeoutMessages
                        { tmSecondEpochTimeouts = insertTimeout tmSecondEpochTimeouts,
                          ..
                        }
            | epoch + 1 == tmFirstEpoch && null tmSecondEpochTimeouts ->
                Just $
                    TimeoutMessages
                        { tmFirstEpoch = epoch,
                          tmFirstEpochTimeouts = singletonTimeout,
                          tmSecondEpochTimeouts = tmFirstEpochTimeouts
                        }
            | epoch == tmFirstEpoch + 2 && not (null tmSecondEpochTimeouts) ->
                Just $
                    TimeoutMessages
                        { tmFirstEpoch = tmFirstEpoch + 1,
                          tmFirstEpochTimeouts = tmSecondEpochTimeouts,
                          tmSecondEpochTimeouts = singletonTimeout
                        }
            | epoch >= tmFirstEpoch + 2 ->
                Just $
                    TimeoutMessages
                        { tmFirstEpoch = epoch,
                          tmFirstEpochTimeouts = singletonTimeout,
                          tmSecondEpochTimeouts = Map.empty
                        }
            | otherwise -> Nothing
  where
    epoch = qcEpoch $ tmQuorumCertificate $ tmBody tm
    finIndex = tmFinalizerIndex $ tmBody tm
    singletonTimeout = Map.singleton finIndex tm
    insertTimeout = Map.insert finIndex tm

-- |Process a timeout message. This stores the timeout, and makes sure the stored timeout messages
-- do not span more than 2 epochs. If enough timeout messages are stored, we form a timeout certificate and
-- advance round, and bake a block in the new round if possible.
--
-- Precondition:
-- * The given 'TimeoutMessage' is valid and has already been checked.
-- * The finalizer must not already have sent out a 'TimeoutMessage' for the current round.
processTimeout ::
    ( MonadTimeout m,
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadReader r m,
      HasBakerContext r,
      MonadProtocolVersion m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      TimeMonad m,
      TimerMonad m,
      MonadBroadcast m,
      MonadThrow m,
      MonadIO m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    TimeoutMessage ->
    m ()
processTimeout tm = do
    timeoutMessages <- use currentTimeoutMessages
    currentRoundStatus <- use roundStatus
    let highestCB = _rsHighestCertifiedBlock currentRoundStatus
    -- Add the new timeout message to the current messages.
    -- If the result is 'Nothing', then there was no change as a result, so nothing left to do.
    let maybeNewTimeoutMessages = updateTimeoutMessages timeoutMessages tm
    forM_ maybeNewTimeoutMessages $ \newTimeoutMessages@TimeoutMessages{..} -> do
        currentTimeoutMessages .=! Present newTimeoutMessages
        skovData <- get
        let getFinalizersForEpoch epoch = (^. bfFinalizers) <$> getBakersForEpoch epoch skovData
        -- We should not fail to get the finalizers for the epoch of the highest QC, because it
        -- should either be the current epoch or the previous one.
        let maybeFinComm = getFinalizersForEpoch (cbEpoch highestCB)
        forM_ maybeFinComm $ \finCommQC -> do
            -- The baker IDs of the finalizers who have signed in the first epoch.
            let firstBakerIds
                    | Just firstFinComm <- getFinalizersForEpoch tmFirstEpoch =
                        bakerIdsFor firstFinComm tmFirstEpochTimeouts
                    | otherwise = []
            -- The baker IDs of the finalizers who have signed in the second epoch.
            let secondBakerIds
                    | not (null tmSecondEpochTimeouts),
                      Just secondFinComm <- getFinalizersForEpoch (tmFirstEpoch + 1) =
                        bakerIdsFor secondFinComm tmSecondEpochTimeouts
                    | otherwise = []
            -- Compute the accumulated voting power by folding over the finalization committee.
            -- We are here making use of the fact that the finalization committee is ordered
            -- by ascending baker ids and that the list of bakerids are also ordered by ascending baker id.
            -- Moreover there MUST be no duplicates in either @firstBakerIds@ or @secondBakerIds@.
            let voterPowerSum =
                    fst $
                        foldl'
                            ( \(!accum, bids) finalizer ->
                                -- We are done accumulating.
                                if null bids
                                    then (accum, [])
                                    else
                                        if head bids == finalizerBakerId finalizer
                                            then -- If we have a match we add the weight to the
                                            -- accumulator and proceed to the next baker id
                                            -- and finalizer.
                                                (accum + finalizerWeight finalizer, tail bids)
                                            else -- If we did not have a match we continue
                                            -- checking with a new finalizer.
                                                (accum, bids)
                            )
                            (0, firstBakerIds `union` secondBakerIds)
                            (committeeFinalizers finCommQC)
            let totalWeightRational = toRational $ committeeTotalWeight finCommQC
            genesisSigThreshold <- toRational . genesisSignatureThreshold . gmParameters <$> use genesisMetadata
            let voterPowerSumRational = toRational voterPowerSum
            when (voterPowerSumRational / totalWeightRational >= genesisSigThreshold) $ do
                let currentRound = _rsCurrentRound currentRoundStatus
                let tc = makeTimeoutCertificate currentRound newTimeoutMessages
                advanceRoundWithTimeout
                    RoundTimeout
                        { rtCertifiedBlock = highestCB,
                          rtTimeoutCertificate = tc
                        }
                makeBlock
  where
    -- baker ids for the finalizers who have signed off the message.
    -- Note that the finalization committee is sorted by ascending baker ids.
    bakerIdsFor finComm timeouts =
        mapMaybe
            (fmap finalizerBakerId . finalizerByIndex finComm)
            -- Note that @Map.keys@ returns the keys in ascending order.
            (Map.keys timeouts)

-- |Make a 'TimeoutCertificate' from a 'TimeoutMessages'.
--
-- NB: It is not checked whether enough timeout messages are present.
-- This should be checked before calling 'makeTimeoutCertificate'.
makeTimeoutCertificate :: Round -> TimeoutMessages -> TimeoutCertificate
makeTimeoutCertificate currentRound TimeoutMessages{..} =
    TimeoutCertificate
        { tcRound = currentRound,
          tcMinEpoch = tmFirstEpoch,
          tcFinalizerQCRoundsFirstEpoch = toFinalizerRounds tmFirstEpochTimeouts,
          tcFinalizerQCRoundsSecondEpoch = toFinalizerRounds tmSecondEpochTimeouts,
          tcAggregateSignature =
            fold $ toAggSigs tmFirstEpochTimeouts ++ toAggSigs tmSecondEpochTimeouts
        }
  where
    -- Add an entry to a 'FinalizerRounds' map for a timeout message.
    foldHelper :: Map.Map Round FinalizerSet -> FinalizerIndex -> TimeoutMessage -> Map.Map Round FinalizerSet
    foldHelper finRounds finIndex tm =
        finRounds
            & at' roundOfQC . non emptyFinalizerSet %~ flip addFinalizer finIndex
      where
        roundOfQC = qcRound $ tmQuorumCertificate $ tmBody tm
    -- Convert timeout messages to 'FinalizerRounds'.
    toFinalizerRounds :: Map.Map FinalizerIndex TimeoutMessage -> FinalizerRounds
    toFinalizerRounds = FinalizerRounds . Map.foldlWithKey' foldHelper Map.empty
    -- Convert timeout messages to lists of signatures.
    toAggSigs :: Map.Map FinalizerIndex TimeoutMessage -> [TimeoutSignature]
    toAggSigs = fmap (tmAggregateSignature . tmBody) . Map.elems
