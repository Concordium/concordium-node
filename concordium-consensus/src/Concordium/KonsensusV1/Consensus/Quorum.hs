{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module contains the logic for creating 'QuorumCertificate's, receiving and
-- verifying quorum signatures that is used for the consensus v1 protocol.
module Concordium.KonsensusV1.Consensus.Quorum where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
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
    deriving (Eq, Show)

-- |Receive a 'QuorumMessage'.
-- Verify the 'QuorumMessage' and if this turns out successful, then the 'QuorumMessage' will be
-- relayed to the network before processing (via 'processQuorumMessage'). Processing checks whether enough (weighted) signatures
-- are gathered, if this is the case then a 'QuorumCertificate' is formed and the consensus runner advances the round.
--
-- Possible return codes are
-- * 'Received' The 'QuorumMessage' was received, relayed and processed.
-- * 'Rejected' The 'QuorumMessage' failed validation and possible it has been flagged.
-- * 'Awaiting' The 'QuorumMessage' cannot be processed at this time as it has a dependency on a pending block.
-- * 'CatchupRequired' The 'QuorumMessage' cannot be processed before it is caught up.
receiveQuorumMessage ::
    ( MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    -- |The 'QuorumMessage' we are receiving.
    QuorumMessage ->
    -- |Result of receiving the 'QuorumMessage'.
    m ReceiveQuorumMessageResult
receiveQuorumMessage qm@QuorumMessage{..} = process =<< get
  where
    process skovData
        -- The round of the quorum signature message is obsolete.
        | qmRound < (skovData ^. roundStatus . rsCurrentRound) =
            return Rejected
        -- The epoch of the quorum signature message is obsolete.
        | qmEpoch <= skovData ^. skovEpochBakers . currentEpoch =
            return Rejected
        -- The consensus runner is not caught up.
        | qmEpoch > skovData ^. skovEpochBakers . currentEpoch =
            return CatchupRequired
        | otherwise = do
            case getFinalizerByIndex skovData of
                -- Signee is not in the finalization committee so we flag it and stop.
                Nothing -> do
                    flag (NotAFinalizer qmFinalizerIndex qm)
                    return Rejected
                Just FinalizerInfo{..}
                    -- Check whether the signature is ok or not.
                    | not (checkQuorumSignatureSingle (getQuorumSignatureMessage skovData) finalizerBlsKey qmSignature) ->
                        return Rejected
                    -- Finalizer already signed a message for this round.
                    | isDoubleSigning finalizerIndex skovData -> do
                        flag (DoubleSigning qmFinalizerIndex qm)
                        return Rejected
                    -- Continue verifying by looking up the block.
                    | otherwise -> do
                        getRecentBlockStatus qmBlock skovData >>= \case
                            -- The signatory signed an already signed block. We flag and stop.
                            OldFinalized -> do
                                flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm)
                                return Rejected
                            -- The signatory signed an already signed block. We flag and stop.
                            RecentBlock (BlockFinalized _) -> do
                                flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm)
                                return Rejected
                            -- The signatory signed a dead block. We flag and stop.
                            RecentBlock BlockDead -> do
                                flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm)
                                return Rejected
                            -- The block is unknown so catch up.
                            RecentBlock BlockUnknown ->
                                return CatchupRequired
                            -- The block is already pending so wait for it to be executed.
                            RecentBlock (BlockPending _) ->
                                return Awaiting
                            -- The block is executed but not finalized.
                            -- Perform the remaining checks before processing the 'QuorumMessage'.
                            RecentBlock (BlockAlive quorumMessagePointer)
                                -- Inconsistent rounds of the quorum signature message and the block it points to.
                                | blockRound quorumMessagePointer /= qmRound -> do
                                    flag $
                                        RoundInconsistency
                                            qmFinalizerIndex
                                            qmRound
                                            (blockRound quorumMessagePointer)
                                    return Rejected
                                -- Inconsistent epochs of the quorum signature message and the block it points to.
                                | blockEpoch quorumMessagePointer /= qmEpoch -> do
                                    flag $
                                        EpochInconsistency
                                            qmFinalizerIndex
                                            qmEpoch
                                            (blockEpoch quorumMessagePointer)
                                    return Rejected
                                -- Store, relay and process the quorum message.
                                | otherwise -> do
                                    storeQuorumMessage qm finalizerWeight
                                    return Received
    -- Extract the quorum signature message
    getQuorumSignatureMessage skovData =
        let genesisHash = skovData ^. genesisMetadata . to gmFirstGenesisHash
        in  quorumSignatureMessageFor qm genesisHash
    -- Get the finalizer if it is present in the current finalization committee otherwise return 'Nothing'.
    getFinalizerByIndex skovData =
        let finalizersForCurrentEpoch = skovData ^. skovEpochBakers . currentEpochBakers . bfFinalizers
        in  finalizerByIndex finalizersForCurrentEpoch qmFinalizerIndex
    -- Checks whether a finalizer has signed off a quorum message already.
    hasSignedOffQuorumMessage finalizerIndex skovData =
        isJust $! skovData ^? currentQuorumMessages . smFinalizerToQuorumMessage . ix finalizerIndex
    -- Checks whether a finalizer has signed off a timeout mesage already.
    hasSignedOffTimeoutMessage = False -- TODO: check in the timeout messages for the current round also.
    -- Checks if the finalizer has signed off either a quorum or a timeout message already.
    isDoubleSigning finalizerIndex skovData =
        hasSignedOffQuorumMessage finalizerIndex skovData || hasSignedOffTimeoutMessage
    -- Update the collection of quorum messages for the running epoch and store it in the state.
    storeQuorumMessage quorumMessage weight = do
        currentMessages <- use currentQuorumMessages
        currentQuorumMessages .=! addQuorumMessage weight quorumMessage currentMessages

-- |Adds a 'QuorumMessage' and the finalizer weight (deducted from the current epoch)
-- to the 'QuorumMessages' for the current round.
--
-- Precondition. The finalizer must not be present already.
addQuorumMessage ::
    -- |Weight of the finalizer.
    VoterPower ->
    -- |The signature message.
    QuorumMessage ->
    -- |The messages to update.
    QuorumMessages ->
    -- |The resulting messages.
    QuorumMessages
addQuorumMessage weight quorumMessage@QuorumMessage{..} (QuorumMessages currentMessages currentWeights) =
    QuorumMessages
        { _smFinalizerToQuorumMessage = newSignatureMessages,
          _smBlockToWeightsAndSignatures = updatedWeightAndSignature
        }
  where
    finalizerIndex = qmFinalizerIndex
    newSignatureMessages = Map.insert finalizerIndex quorumMessage currentMessages
    justOrIncrement = maybe (Just (weight, qmSignature, Set.singleton finalizerIndex)) (\(aggWeight, aggSig, aggFinalizers) -> Just (aggWeight + weight, aggSig <> qmSignature, Set.insert finalizerIndex aggFinalizers))
    updatedWeightAndSignature = Map.alter justOrIncrement qmBlock currentWeights

-- |If there are enough (weighted) signatures on the block provided
-- then this function creates the 'QuorumCertificate' for the block and returns @Just QuorumCertificate@
--
-- If a 'QuorumCertificate' could not be formed then this function returns @Nothing@.
makeQuorumCertificate ::
    (MonadState (SkovData pv) m) =>
    -- |The block we want to check whether we can
    -- create a 'QuorumCertificate' or not.
    BlockHash ->
    -- |Return @Just QuorumCertificate@ if there are enough (weighted) quorum signatures
    -- for the provided block.
    -- Otherwise return @Nothing@.
    m (Maybe QuorumCertificate)
makeQuorumCertificate blockPointer = do
    currentMessages <- use currentQuorumMessages
    case currentMessages ^? smBlockToWeightsAndSignatures . ix blockPointer of
        Nothing -> return Nothing
        Just (accummulatedWeight, aggregatedSignature, finalizers) -> do
            signatureThreshold <- genesisSignatureThreshold . gmParameters <$> use genesisMetadata
            totalWeight <- committeeTotalWeight <$> use (skovEpochBakers . currentEpochBakers . bfFinalizers)
            if toRational accummulatedWeight / toRational totalWeight >= toRational signatureThreshold
                then do
                    theRound <- use $ roundStatus . rsCurrentRound
                    theEpoch <- use $ skovEpochBakers . currentEpoch
                    return $
                        Just
                            QuorumCertificate
                                { qcBlock = blockPointer,
                                  qcRound = theRound,
                                  qcEpoch = theEpoch,
                                  qcAggregateSignature = aggregatedSignature,
                                  qcSignatories = finalizerSet $ Set.toList finalizers
                                }
                else return Nothing

-- |Process a 'QuorumMessage'
-- Check whether a 'QuorumCertificate' can be created.
-- If that is the case the this function checks for finality and
-- advance the round via the constructed 'QuorumCertificate'.
--
-- Precondition: The 'QuorumMessage' must've been pre verified via
-- 'receiveQuorumMessage' (i.e. 'Received' result code) before calling this function.
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
    -- |The 'QuorumMessage' to process.
    QuorumMessage ->
    m ()
processQuorumMessage QuorumMessage{..} = do
    makeQuorumCertificate qmBlock >>= \case
        Nothing -> return ()
        Just newQC -> do
            checkFinality newQC
            advanceRound (qmRound + 1) (Right newQC)
            return ()
