{-# LANGUAGE DataKinds #-}

module Concordium.KonsensusV1.Flag where

import Concordium.Types

import Concordium.KonsensusV1.Types

-- |Offense by a baker that can be flagged.
data FlaggableOffense (pv :: ProtocolVersion)
    = -- |The baker was not a leader for the round.
      -- Note that the witness here should probably yield the round (and epoch) in the future,
      -- but it is currently not required as the flags are not used.
      NotLeader !BlockSignatureWitness
    | -- |A baker has already produced a block for the round.
      -- The first witness is the already recorded pending block while the
      -- latter witness is for the block just received.
      DuplicateBlock !BlockSignatureWitness !BlockSignatureWitness
    | -- |The 'Round' of the 'QuorumCertificate' is not consistent with the
      -- 'Round' of the parent block. Witnessed by the block received.
      BlockQCRoundInconsistent !SignedBlock
    | -- |The 'Epoch' of the 'QuorumCertificate' is not consistent with the
      -- 'Epoch' of the parent block. Witnessed by the block received.
      BlockQCEpochInconsistent !SignedBlock
    | -- |The round is not greater than the parent block. Witnessed by
      -- the block received.
      BlockRoundInconsistent !SignedBlock
    | -- |The epoch is not the current or current + 1 epoch. Witnessed
      -- by the block received.
      BlockEpochInconsistent !SignedBlock
    | -- |The block received contains an invalid 'QuorumCertificate'.
      -- Witnessed by the block received.
      BlockInvalidQC !SignedBlock
    | -- |The 'TimeoutCertificate' is missing and the 'Round' of the block is
      -- not sequentially the next 'Round'. Witnessed by the block received.
      BlockTCMissing !SignedBlock
    | -- |The 'Round' of the 'TimeoutCertificate' is inconsistent.
      -- Witnessed by the block received.
      BlockTCRoundInconsistent !SignedBlock
    | -- |The 'QuorumCertificate' is inconsistent
      -- with the 'TimeoutCertificate' of the block.
      -- Witnessed by the block received.
      BlockQCInconsistentWithTC !SignedBlock
    | -- |The previous round did not timeout, but there is a
      -- 'TimeoutCertificate' present in the block.
      -- Witnessed by the block received.
      BlockUnexpectedTC !SignedBlock
    | -- |The 'TimeoutCertificate' is invalid.
      -- Witnessed by the block received.
      BlockInvalidTC !SignedBlock
    | -- |The 'SignedBlock' is too close to its parent @Block pv@.
      BlockTooFast !SignedBlock !(Block pv)
    | -- |The block nonce is invalid. Witnessed by the block received.
      BlockNonceIncorrect !SignedBlock
    | -- |The block is in a new 'Epoch', but it is missing the finalization entry.
      -- Witnessed by the block received.
      BlockEpochFinalizationMissing !SignedBlock
    | -- |The block was not in a new 'Epoch', but a finalization entry is presnet.
      -- Witnessed by the block received.
      BlockUnexpectedEpochFinalization !SignedBlock
    | -- |The block is in a new 'Epoch' but the finalization entry is deemed invalid.
      -- Witnessed by the block received.
      BlockInvalidEpochFinalization !SignedBlock
    | -- |Execution of the block failed.
      -- Witnessed by the block received.
      BlockExecutionFailure !SignedBlock
    | -- |Execution of the block resulted in an unexpected outcome.
      -- Witnessed by the block received and the parent block.
      BlockInvalidTransactionOutcomesHash !SignedBlock !(Block pv)
    | -- |Execution of the block resulted in an unexpected state.
      -- Witnessed by the block received and the parent block.
      BlockInvalidStateHash !SignedBlock !(Block pv)
    | -- |An invalid block was signed by the 'QuorumMessage'.
      -- Witnessed by the 'QuorumMessage' received.
      SignedInvalidBlock !QuorumMessage
    | -- |The finalizer signed two distinct quorum messages for the same round.
      QuorumDoubleSigning {qdsReceived :: !QuorumMessage, qdsExisting :: !QuorumMessage}
    | -- |The 'Round' of the 'QuorumMessage' and the block it points to are
      -- inconsistent. Witnessed by the 'QuorumMessage' and the block it points to.
      RoundInconsistency !QuorumMessage !(Block pv)
    | -- |The 'Epoch' of the 'QuorumMessage' and the block it points to are
      -- inconsistent. Witnessed by the 'QuorumMessage' and the block it points to.
      EpochInconsistency !QuorumMessage !(Block pv)
    | -- |The finalizer signed two distinct timeout messages for the same round.
      TimeoutDoubleSigning {tdsReceived :: !TimeoutMessage, tdsExisting :: !TimeoutMessage}
    | -- |The BLS signature on the timeout message is not valid.
      InvalidTimeoutSignature !TimeoutMessage
    | -- |The timeout message yields an invalid 'QuorumCertificate'.
      TimeoutMessageInvalidQC !TimeoutMessage

-- |Flag an offense by a baker. Currently, this does nothing.
flag :: Monad m => FlaggableOffense pv -> m ()
flag _ = return ()
