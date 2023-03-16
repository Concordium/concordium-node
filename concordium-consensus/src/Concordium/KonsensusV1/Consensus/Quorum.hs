{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module contains the logic for receiving and
-- verifying quorum signatures.
module Concordium.KonsensusV1.Consensus.Quorum where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Lens.Micro.Platform

import Concordium.Genesis.Data.BaseV1
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.Parameters

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Finality
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Utils

data ReceiveQuorumMessageResult
    = -- |The 'QuorumMessage' was received i.e. it passed verification.
      Received
    | -- |The 'QuorumMessage' was rejected.
      Rejected
    | -- |The 'QuorumMessage' cannot be processed at this time since the block it is pointing to is pending.
      Awaiting
    | -- |The 'QuorumMessage' points to a round which indicates a catch up is required.
      CatchupRequired

-- |Receive a 'QuorumMessage'.
-- If the provided 'QuorumMessage' can be verified then
-- the it is being rebroadcast over the network
-- and 'processQuorumSignatureMessage' will be invoked.
--
-- If the 'QuorumMessage' cannot be verified then it is not
-- being rebroadcast and this procedure terminates.
--
-- If the consensus runner is not able to verify the 'QuorumMessage' due to
-- not being caught up, then 'startCatchup' will be invoked.
--
-- Returns 'False' if the 'QuorumMessage' could not be verified otherwise 'True'.
receiveQuorumMessage ::
    ( IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      TimeMonad m,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadMulticast m
    ) =>
    -- |The 'QuorumMessage' we are receiving.
    QuorumMessage ->
    m ReceiveQuorumMessageResult
receiveQuorumMessage qm@QuorumMessage{..} = process =<< get
  where
    process skovData
        -- The round of the quorum signature message is obsolete.
        | qmRound < rsCurrentRound (skovData ^. roundStatus) = return Rejected
        -- The epoch of the quroum signature message is obsolete.
        | qmEpoch <= rsCurrentEpoch (skovData ^. roundStatus) = return Rejected
        | otherwise = do
            currentBakers <- use skovEpochBakers
            -- The consensus runner is not caugt up, so we initiate catchup.
            if qmEpoch > currentBakers ^. epochBakersEpoch
                then return CatchupRequired
                else do
                    let finalizersForCurrentEpoch = currentBakers ^. currentEpochBakers . bfFinalizers
                    case finalizerByIndex finalizersForCurrentEpoch qmFinalizerIndex of
                        -- Signee is not in the finalization committee so we flag it and stop.
                        Nothing -> flag (NotAFinalizer qmFinalizerIndex qm) >> return Rejected
                        Just FinalizerInfo{..} -> do
                            genesisHash <- gmFirstGenesisHash <$> use genesisMetadata
                            let qsm = getQuorumSignatureMessage genesisHash qm
                            -- Check whether the signature is ok or not.
                            if not (checkQuorumSignatureSingle qsm finalizerBlsKey qmSignature)
                                then return Rejected
                                else do
                                    getRecentBlockStatus qmBlock skovData >>= \case
                                        -- The signatory signed an already signed block. We flag and stop.
                                        OldFinalized -> flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm) >> return Rejected
                                        -- The signatory signed an already signed block. We flag and stop.
                                        RecentBlock (BlockFinalized _) -> flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm) >> return Rejected
                                        -- The signatory signed a dead block. We flag and stop.
                                        RecentBlock BlockDead -> flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm) >> return Rejected
                                        RecentBlock BlockUnknown -> return CatchupRequired
                                        RecentBlock (BlockPending _) -> return Awaiting
                                        RecentBlock (BlockAlive quorumMessagePointer) -> do
                                            if isDoubleSigning finalizerIndex skovData
                                                then flag (DoubleSigning qmFinalizerIndex qm) >> return Rejected
                                                else -- Inconsistent rounds of the quorum signature message and the block it points to.

                                                    if blockRound quorumMessagePointer /= qmRound
                                                        then flag (RoundInconsistency qmFinalizerIndex qmRound (blockRound quorumMessagePointer)) >> return Rejected
                                                        else
                                                            if blockEpoch quorumMessagePointer /= qmEpoch
                                                                then flag (EpochInconsistency qmFinalizerIndex qmEpoch (blockEpoch quorumMessagePointer)) >> return Rejected
                                                                else storeQuorumMessage qm finalizerWeight >> sendMessage qm >> processQuorumMessage qm >> return Received
    -- Checks whether a finalizer present is present in the 'QuourumMessages'.
    hasSignedOffQuorumMessage finalizerIndex skovData = isJust $! skovData ^? currentQuorumMessages . smFinIdxToMessage . ix finalizerIndex
    hasSignedOffTimeoutMessage = False -- TODO: check in the timeout messages for the current round also.
    isDoubleSigning finalizerIndex skovData = hasSignedOffQuorumMessage finalizerIndex skovData || hasSignedOffTimeoutMessage
    -- Update the collection of quorum messages for the running epoch and store it in the state.
    storeQuorumMessage quorumMessage weight = do
        currentMessages <- use currentQuorumMessages
        currentQuorumMessages .=! addQuorumMessage qmFinalizerIndex weight quorumMessage currentMessages
        return ()

addQuorumMessage ::
    -- |Identifier for the finalizer who has signed off.
    FinalizerIndex ->
    -- |Weight of the finalizer.
    VoterPower ->
    -- |The signature message.
    QuorumMessage ->
    -- |The messages to update.
    QuorumMessages ->
    -- |The resulting messages.
    QuorumMessages
addQuorumMessage finalizerIndex weight quorumMessage@QuorumMessage{..} (QuorumMessages currentMessages currentWeights) = updateQuorumSignatures
  where
    updateQuorumSignatures =
        QuorumMessages
            { _smFinIdxToMessage = newSignatureMessages,
              _smWeightsAndSignatures = updatedWeightAndSignature
            }
    newSignatureMessages = Map.insert finalizerIndex quorumMessage currentMessages
    justOrIncrement = maybe (Just (weight, qmSignature, [finalizerIndex])) (\(aggWeight, aggSig, aggFinalizers) -> Just (aggWeight + weight, aggSig <> qmSignature, finalizerIndex : aggFinalizers))
    updatedWeightAndSignature = Map.alter justOrIncrement qmBlock currentWeights

-- |A wittness that a 'QuorumCertificate' can be created.
data QuorumWitness = QuorumWitness
    { -- |The block that a quorum certificate will refer to.
      qwBlock :: !BlockHash,
      -- |The aggregated signature
      qwAggregateSignature :: !QuorumSignature,
      -- |The signatories.
      qwSignatories :: !FinalizerSet
    }

-- |Check whether there is enough weight for the provided
-- block with respect to the signature threshold recorded in the tree state.
checkForQuorumWittness ::
    (MonadState (SkovData (MPV m)) m) =>
    -- |The block we want to check whether we can
    -- create a 'QuorumCertificate' or not.
    BlockHash ->
    -- |Return @Just QuorumWitness@ if there are enough (weighted) quorum signatures
    -- for the provided block.
    -- Otherwise return @Nothing@.
    m (Maybe QuorumWitness)
checkForQuorumWittness qcBlock = do
    currentMessages <- use currentQuorumMessages
    case currentMessages ^? smWeightsAndSignatures . ix qcBlock of
        Nothing -> return Nothing
        Just (accummulatedWeight, aggregatedSignature, finalizers) -> do
            signatureThreshold <- genesisSignatureThreshold . gmParameters <$> use genesisMetadata
            totalWeight <- committeeTotalWeight <$> use (skovEpochBakers . currentEpochBakers . bfFinalizers)
            if toRational accummulatedWeight / toRational totalWeight >= toRational signatureThreshold
                then return $ Just QuorumWitness{qwBlock = qcBlock, qwAggregateSignature = aggregatedSignature, qwSignatories = finalizerSet finalizers}
                else return Nothing

makeQuorumCertificate :: (MonadState (SkovData (MPV m)) m) => QuorumWitness -> m QuorumCertificate
makeQuorumCertificate QuorumWitness{..} = do
    RoundStatus{..} <- use roundStatus
    let qcBlock = qwBlock
        qcRound = rsCurrentRound
        qcEpoch = rsCurrentEpoch
        qcAggregateSignature = qwAggregateSignature
        qcSignatories = qwSignatories
    return QuorumCertificate{..}

processQuorumMessage ::
    ( IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      TimeMonad m,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m
    ) =>
    QuorumMessage ->
    m ()
processQuorumMessage QuorumMessage{..} = do
    checkForQuorumWittness qmBlock >>= \case
        Nothing -> return ()
        Just witness -> do
            newQC <- makeQuorumCertificate witness
            checkFinality newQC
            advanceRound (qmRound + 1) (Right newQC)
            return ()
