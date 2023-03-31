{-# LANGUAGE DataKinds #-}

module Concordium.KonsensusV1.Flag where

import Concordium.Types

import Concordium.KonsensusV1.Types

-- |Offense by a baker that can be flagged.
data FlaggableOffense (pv :: ProtocolVersion)
    = NotLeader !BlockSignatureWitness
    | DuplicateBlock !BlockSignatureWitness !BlockSignatureWitness
    | BlockQCRoundInconsistent !SignedBlock
    | BlockQCEpochInconsistent !SignedBlock
    | BlockRoundInconsistent !SignedBlock
    | BlockEpochInconsistent !SignedBlock
    | BlockTCMissing !SignedBlock
    | BlockTCRoundInconsistent !SignedBlock
    | BlockQCInconsistentWithTC !SignedBlock
    | BlockUnexpectedTC !SignedBlock
    | BlockInvalidTC !SignedBlock
    | BlockTooFast !SignedBlock !(Block pv)
    | BlockNonceIncorrect !SignedBlock
    | BlockEpochFinalizationMissing !SignedBlock
    | BlockUnexpectedEpochFinalization !SignedBlock
    | BlockInvalidQC !SignedBlock
    | BlockInvalidEpochFinalization !SignedBlock
    | BlockExecutionFailure !SignedBlock
    | BlockInvalidTransactionOutcomesHash !SignedBlock !(Block pv)
    | BlockInvalidStateHash !SignedBlock !(Block pv)
    | SignedInvalidBlock !QuorumMessage
    | -- |The finalizer signed two distinct quorum messages for the same round.
      QuorumDoubleSigning {qdsReceived :: !QuorumMessage, qdsExisting :: !QuorumMessage}
    | -- Note. This flag is currently triggered before the
      -- message is relayed and as such the message might not end up
      -- at a baker and so the bad behaviour won't become part a block.
      -- If this is to be punished in future, then we should relay the message
      -- before flagging.
      RoundInconsistency !QuorumMessage !(Block pv)
    | -- Note. This flag is currently triggered before the
      -- message is relayed and as such the message might not end up
      -- at a baker and so the bad behaviour won't become part a block.
      -- If this is to be punished in future, then we should relay the message
      -- before flagging.
      EpochInconsistency !QuorumMessage !(Block pv)
    | -- |The finalizer signed two distinct timeout messages for the same round.
      TimeoutDoubleSigning {tdsReceived :: !TimeoutMessage, tdsExisting :: !TimeoutMessage}
    | -- |The bls signature on the timeout message is not valid.
      InvalidTimeoutSignature !TimeoutMessage
    | -- |The timeout message yields an invalid 'QuorumCertificate'.
      TimeoutMessageInvalidQC !TimeoutMessage

-- |Flag an offense by a baker. Currently, this does nothing.
flag :: Monad m => FlaggableOffense pv -> m ()
flag _ = return ()
