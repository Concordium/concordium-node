{-# LANGUAGE TypeFamilies #-}
-- |This module contains the logic for receiving and
-- verifying quorum signatures.
module Concordium.KonsensusV1.Consensus.QuorumSignatures where

import Control.Monad.State
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Parameters

import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.Types
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.Consensus

data ReceiveQuorumMessageResult
    -- |The 'QuorumMessage' was received i.e. it passed verification.
    = Received
    -- |The 'QuorumMessage' was rejected.
    | Rejected
    -- |The 'QuorumMessage' cannot be processed at this time since the block it is pointing to is pending.
    | Awaiting
    -- |The 'QuorumMessage' points to a round which indicates a catch up is required.
    | CatchupRequired
    
      

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
receiveQuorumMessage :: (IsConsensusV1 (MPV m), MonadMulticast m, MonadState (SkovData (MPV m)) m, LowLevel.MonadTreeStateStore m) => QuorumMessage -> m ReceiveQuorumMessageResult
receiveQuorumMessage qm@QuorumMessage{..} = process =<< get
  where
    process sd
        -- The round of the quorum signature message is obsolete.
        | qmRound < rsCurrentRound (sd ^. roundStatus) = return Rejected
        -- The epoch of the quroum signature message is obsolete.
        | qmEpoch <= rsCurrentEpoch (sd ^. roundStatus) = return Rejected
        | otherwise = do
            currentBakers <- use skovEpochBakers
            -- The consensus runner is not caugt up, so we initiate catchup.
            if qmEpoch > currentBakers ^. epochBakersEpoch then return CatchupRequired
            else do
                let finalizersForCurrentEpoch = currentBakers ^. currentEpochBakers . bfFinalizers
                case finalizerByIndex finalizersForCurrentEpoch qmFinalizerIndex of
                    -- Signee is not in the finalization committee so we flag it and stop.
                    Nothing -> flag (NotAFinalizer qmFinalizerIndex qm) >> return Rejected
                    Just FinalizerInfo{..} -> do
                        genesisHash <-  gmFirstGenesisHash <$> use genesisMetadata
                        let qsm = getQuorumSignatureMessage genesisHash qm
                        -- Check whether the signature is ok or not.
                        if not (checkQuorumSignatureSingle qsm finalizerBlsKey qmSignature) then return Rejected
                        else do
                            getRecentBlockStatus qmBlock sd >>= \case
                                Unknown -> return CatchupRequired
                                -- The signatory signed an already signed block. We flag and stop.
                                OldFinalized -> flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm) >> return Rejected
                                -- The signatory signed an already signed block. We flag and stop.
                                RecentBlock (BlockFinalized _) -> flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm) >> return Rejected
                                -- The signatory signed a dead block. We flag and stop.
                                RecentBlock BlockDead -> flag (SignedInvalidBlock qmFinalizerIndex qmBlock qm) >> return Rejected
                                RecentBlock BlockUnknown -> return CatchupRequired
                                RecentBlock (BlockPending _) -> return Awaiting
                                RecentBlock (BlockAlive quorumMessagePointer) -> do
                                    if isDoubleSigning finalizerIndex sd then flag (DoubleSigning qmFinalizerIndex qm) >> return Rejected
                                    -- Inconsistent rounds of the quorum signature message and the block it points to.
                                    else if blockRound quorumMessagePointer /= qmRound then flag (RoundInconsistency qmFinalizerIndex qmRound (blockRound quorumMessagePointer)) >> return Rejected
                                    else if blockEpoch quorumMessagePointer /= qmEpoch then flag (EpochInconsistency qmFinalizerIndex qmEpoch (blockEpoch quorumMessagePointer)) >> return Rejected
                                    else storeQuorumMessage >> sendQuorumMessage qm >> processQuorumMessage qm >> return Received
    -- Checks wether there is already either a timeout signature or a quorum signature by the supplied 'finalizerIndex' present for the current round.
    isDoubleSigning finalizerIndex skovData =
        isFinalizerPresent finalizerIndex (rsCurrentQuorumSignatureMessages (skovData ^. roundStatus))
        || isFinalizerPresent finalizerIndex (rsCurrentTimeoutSignatureMessages (skovData ^. roundStatus))
    -- todo: store the quorum message in the signatures on the round status (and set it i.e. persist it)
    storeQuorumMessage = return ()

makeQC :: BlockHash -> Round -> Epoch -> QuorumSignature -> FinalizerSet -> QuorumCertificate
makeQC = QuorumCertificate


processQuorumMessage :: (MonadState (SkovData (MPV m)) m, LowLevel.MonadTreeStateStore m) => QuorumMessage -> m ()
processQuorumMessage QuorumMessage{..} = do
    when enoughWeightedSignatures $ do
        -- todo.. aggregate here or optimistically? hmm
        let qcForRound = makeQC qmBlock qmRound qmEpoch qmSignature (FinalizerSet 0)
        return ()
        --todo: checkFinality
        -- todo: advanceRoundStatus
    return ()
  where
    -- todo genesisSignatureThreshold <$> use genesisMetadata
    enoughWeightedSignatures = True
