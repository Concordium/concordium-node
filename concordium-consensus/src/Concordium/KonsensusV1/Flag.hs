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

-- |Flag an offense by a baker. Currently, this does nothing.
flag :: Monad m => FlaggableOffense pv -> m ()
flag _ = return ()
