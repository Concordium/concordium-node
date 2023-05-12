{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Timeout where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
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
import Concordium.KonsensusV1.Consensus.Finality (checkFinality)
import Concordium.KonsensusV1.Consensus.Timeout.Internal
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

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
    | -- |The 'QuorumCertificate' is ponting to a dead block.
      DeadQCPointer
    | -- |The epoch of the 'QuorumCertificate' does not agree with the epoch of an existing
      -- certificate for that round.
      BadQCEpoch
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
    | -- |The 'TimeoutMessage' is a duplicate.
      Duplicate
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
      -- |Block pointer for the block referenced by the 'QuorumCertificate'.
      -- If this is absent, then the quorum certificate is for a round less than that of the highest
      -- certified block, and we have already checked a 'QuorumCertificate' for the same round
      -- and epoch.
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
    -- is behind the last finalized block then reject the timeout message,
    -- then it means that the sender of the timeout message was lacking behind for some reason.
    -- This can for instance happen if the sender of the timeout message
    -- did not receive the quorum message before sending out the time out message
    -- due to e.g. network issues.
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
                    getRecentBlockStatus (qcBlock tmQuorumCertificate) skovData >>= \case
                        -- In this case, the quorum certificate cannot be valid, because otherwise
                        -- it would already have been rejected for an obsolete QC.
                        OldFinalized ->
                            return $ Rejected ObsoleteQCPointer
                        -- If the block is unknown, we can still proceed provided that we won't
                        -- need to check the QC. This is the case if we have already checked a QC
                        -- for the round, and the highest certified block is for a later round.
                        RecentBlock BlockUnknown
                            | qcRound tmQuorumCertificate
                                < skovData ^. roundStatus . rsHighestCertifiedBlock . to cbRound,
                              Just (QuorumCertificateWitness certEpoch) <-
                                skovData ^. roundExistingQuorumCertificate (qcRound tmQuorumCertificate) ->
                                if qcEpoch tmQuorumCertificate == certEpoch
                                    then checkForDuplicate finInfo finalizationCommittee Absent
                                    else return $ Rejected BadQCEpoch
                            | otherwise ->
                                return CatchupRequired
                        -- The QC pointer points to a block that has been marked dead.
                        -- (Dead blocks are either invalid or branch from a pruned block.)
                        -- Here also the QC cannot be valid.
                        RecentBlock BlockDead ->
                            return $ Rejected DeadQCPointer
                        -- The QC pointer in the timeout message is pending so catch up
                        -- is required
                        RecentBlock (BlockPending _) ->
                            return CatchupRequired
                        RecentBlock (BlockAliveOrFinalized qcBlock) ->
                            checkForDuplicate finInfo finalizationCommittee (Present qcBlock)
  where
    -- Get the bakers and finalizers for the epoch of the timeout message's QC.
    -- If they are not available, trigger catch-up.
    withFinalizers cont = case getBakersForEpoch (qcEpoch tmQuorumCertificate) skovData of
        Nothing -> return CatchupRequired
        Just bakers -> cont (bakers ^. bfFinalizers)
    -- Look up the finalizer in the finalization committee.
    -- Reject with 'NotAFinalizer' if the finalizer index is not valid in the committee.
    withFinalizer finalizers cont = case finalizerByIndex finalizers tmFinalizerIndex of
        Nothing -> return $ Rejected NotAFinalizer
        Just finalizer -> cont finalizer
    -- Check the timeout message when the QC pointer is alive or finalized, or is unknown but a QC
    -- for the given round has already been verified.
    checkForDuplicate finalizerInfo qcFinalizationCommittee mBlock =
        case getExistingMessage of
            -- The message is a duplicate. Check for double signing.
            Just existingMessage
                | existingMessage == tm -> return Duplicate
                | otherwise -> do
                    -- The finalizer has already sent a timeout message for this round, this is not
                    -- allowed so the behaviour is flagged and timeout message is rejected.
                    flag $! TimeoutDoubleSigning tm existingMessage
                    return $ Rejected DoubleSigning
            -- Return a 'PartiallyVerifiedTimeoutMessage'
            Nothing ->
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
        Absent -> Nothing
        Present messages -> messages ^? to tmFirstEpochTimeouts . ix tmFinalizerIndex
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
    | -- |The 'QuorumCertificate' for the 'TimeoutMessage'
      -- is for a wrong 'Epoch'.
      InvalidQCEpoch !Epoch !QuorumCertificate
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
      BlockStateStorage m,
      TimeMonad m,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m,
      MonadConsensusEvent m,
      MonadLogger m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m
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
    -- Note: deserialization of the 'TimeoutMessageBody' checks that the timeout round and
    -- QC round are coherent, so we do not need to check that here.
    | Absent <- pvtmBlock = do
        -- In this case, we have already checked a valid QC for the round and epoch of the timeout
        -- message, so we just need to process the timeout.
        processTimeout pvtmTimeoutMessage
        return ExecutionSuccess
    | Present block <- pvtmBlock = do
        highestQCRound <- use $ roundStatus . rsHighestCertifiedBlock . to cbRound
        -- Check the quorum certificate if it's from a round we have not checked before.
        if qcRound tmQuorumCertificate > highestQCRound
            then checkQC $ do
                -- The quorum certificate is valid and we check whether we can advance by it.
                -- Check if the quorum certificate of the timeout message finalizes any blocks.
                checkFinality tmQuorumCertificate
                -- Advance the round if we can advance by the quorum certificate.
                currentRound <- use $ roundStatus . rsCurrentRound
                when (currentRound <= qcRound tmQuorumCertificate) $
                    advanceRoundWithQuorum
                        CertifiedBlock
                            { cbQuorumCertificate = tmQuorumCertificate,
                              cbQuorumBlock = block
                            }
                -- Record the witness of the quorum certificate in the existing QCs on the treestate.
                recordCheckedQuorumCertificate tmQuorumCertificate
                -- Process the timeout
                processTimeout pvtmTimeoutMessage
                return ExecutionSuccess
            else do
                -- Check whether we have already checked a QC for the round
                -- As the timeout message has been successfully received before this we know that
                -- the QC is for a round greater than the last finalized block.
                use (roundExistingQuorumCertificate (qcRound tmQuorumCertificate)) >>= \case
                    -- We only check the QC if we haven't done so for the round already.
                    Nothing ->
                        checkQC $ do
                            -- The quorum certificate is valid so check whether it finalises any blocks.
                            checkFinality tmQuorumCertificate
                            processTimeout pvtmTimeoutMessage
                            return ExecutionSuccess
                    -- A QC for the QC round is already checked, we just check that the
                    -- epochs are consistent now.
                    Just (QuorumCertificateWitness qcEpoch')
                        | qcEpoch' /= qcEpoch tmQuorumCertificate -> do
                            -- the qc is invalid since it was for another epoch.
                            flag $ TimeoutMessageInvalidQC pvtmTimeoutMessage
                            return $ InvalidQCEpoch qcEpoch' tmQuorumCertificate
                        | otherwise -> do
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
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m,
      MonadLogger m
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
-- advance round.
--
-- Precondition:
-- * The given 'TimeoutMessage' is valid and has already been checked.
processTimeout ::
    ( MonadTimeout m,
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m
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
                    | otherwise = Set.empty
            -- The baker IDs of the finalizers who have signed in the second epoch.
            let secondBakerIds
                    | not (null tmSecondEpochTimeouts),
                      Just secondFinComm <- getFinalizersForEpoch (tmFirstEpoch + 1) =
                        bakerIdsFor secondFinComm tmSecondEpochTimeouts
                    | otherwise = Set.empty
            -- Compute the voter power in the epoch of the highest QC for a baker by the baker ID.
            let getBakerVoterPower = fmap finalizerWeight . finalizerByBakerId finCommQC
            let voterPowerSum =
                    sum . mapMaybe getBakerVoterPower $
                        Set.toList (firstBakerIds `Set.union` secondBakerIds)
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
  where
    bakerIdsFor finComm timeouts =
        Set.fromList $
            mapMaybe
                (fmap finalizerBakerId . finalizerByIndex finComm)
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
