{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.TreeState.Types where

import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.TransactionTable
import Concordium.KonsensusV1.Types

-- |Status information for a finalized transaction.
data FinalizedTransactionStatus = FinalizedTransactionStatus
    { -- |Height of the finalized block that contains this transaction
      ftsBlockHeight :: !BlockHeight,
      -- |Index of the transaction in the block.
      ftsIndex :: !TransactionIndex
    }
    deriving (Eq, Show)

instance Serialize FinalizedTransactionStatus where
    put FinalizedTransactionStatus{..} = do
        put ftsBlockHeight
        put ftsIndex
    get = do
        ftsBlockHeight <- get
        ftsIndex <- get
        return FinalizedTransactionStatus{..}

-- |Metadata about a block that has been executed.
data BlockMetadata = BlockMetadata
    { -- |The height of the block.
      bmHeight :: !BlockHeight,
      -- |The time that the block is received by the
      -- consensus layer i.e. it has been deserialized.
      bmReceiveTime :: !UTCTime,
      -- |The time that the block has become live,
      -- i.e. it has been processed and current head of the chain.
      bmArriveTime :: !UTCTime
    }
    deriving (Eq, Show)

instance Serialize BlockMetadata where
    put BlockMetadata{..} = do
        put bmHeight
        putUTCPOSIXMicros bmReceiveTime
        putUTCPOSIXMicros bmArriveTime
      where
        putUTCPOSIXMicros = putWord64be . floor . (1_000_000 *) . utcTimeToPOSIXSeconds
    get = do
        bmHeight <- get
        bmReceiveTime <- getUTCPOSIXMicros
        bmArriveTime <- getUTCPOSIXMicros
        return BlockMetadata{..}
      where
        getUTCPOSIXMicros = posixSecondsToUTCTime . (/ 1_000_000) . realToFrac <$> getWord64be

-- |A pointer to a block that has been executed
-- and the resulting 'PBS.HashedPersistentBlockState'.
data BlockPointer (pv :: ProtocolVersion) = BlockPointer
    { -- |Metadata for the block.
      bpInfo :: !BlockMetadata,
      -- |The signed block.
      bpBlock :: !(Block pv),
      -- |The resulting state of executing the block.
      bpState :: !(PBS.HashedPersistentBlockState pv)
    }

instance HashableTo BlockHash (BlockPointer pv) where
    getHash BlockPointer{..} = getHash bpBlock

instance BlockData (BlockPointer pv) where
    type BakedBlockDataType (BlockPointer pv) = SignedBlock
    blockRound = blockRound . bpBlock
    blockEpoch = blockEpoch . bpBlock
    blockTimestamp = blockTimestamp . bpBlock
    blockBakedData = blockBakedData . bpBlock
    blockTransactions = blockTransactions . bpBlock
    blockStateHash = blockStateHash . bpBlock

instance Show (BlockPointer pv) where
    show BlockPointer{..} =
        "BlockPointer {bpInfo = "
            ++ show bpInfo
            ++ ", bpBlock = "
            ++ show bpBlock
            ++ ", bpState = ["
            ++ show (PBS.hpbsHash bpState)
            ++ "] }"

-- |A block that is pending its parent.
data PendingBlock = PendingBlock
    { -- |The block itself.
      pbBlock :: !SignedBlock,
      -- |The time that the block was received by the consensus.
      pbReceiveTime :: !UTCTime
    }
    deriving (Eq, Show)

instance HashableTo BlockHash PendingBlock where
    getHash PendingBlock{..} = getHash pbBlock

instance BlockData PendingBlock where
    type BakedBlockDataType PendingBlock = SignedBlock
    blockRound = blockRound . pbBlock
    blockEpoch = blockEpoch . pbBlock
    blockTimestamp = blockTimestamp . pbBlock
    blockBakedData = blockBakedData . pbBlock
    blockTransactions = blockTransactions . pbBlock
    blockStateHash = blockStateHash . pbBlock

instance BakedBlockData PendingBlock where
    blockQuorumCertificate = blockQuorumCertificate . pbBlock
    blockParent = blockParent . pbBlock
    blockBaker = blockBaker . pbBlock
    blockTimeoutCertificate = blockTimeoutCertificate . pbBlock
    blockEpochFinalizationEntry = blockEpochFinalizationEntry . pbBlock
    blockNonce = blockNonce . pbBlock
    blockSignature = blockSignature . pbBlock

-- |Status of a transaction.
data TransactionStatus
    = -- |The transaction is either pending (i.e. not in a block) or committed (i.e. in a
      -- non-finalized block).
      Live !LiveTransactionStatus
    | -- |The transaction is in a finalized block.
      Finalized !FinalizedTransactionStatus
    deriving (Eq, Show)

-- |The status of a block.
data BlockStatus pv
    = -- |The block is awaiting its parent to become part of chain.
      BlockPending !PendingBlock
    | -- |The block is alive i.e. head of chain.
      BlockAlive !(BlockPointer pv)
    | -- |The block is finalized.
      BlockFinalized !(BlockPointer pv)
    | -- |The block has been marked dead.
      BlockDead
    | -- |The block is unknown
      BlockUnknown
    deriving (Show)

-- |The status of a block as obtained without loading the block from disk.
data RecentBlockStatus pv
    = -- |The block is recent i.e. it is either 'Alive',
      -- 'Pending' or the last finalized block.
      RecentBlock !(BlockStatus pv)
    | -- |The block is a predecessor of the last finalized block.
      OldFinalized
    | -- |The block is unknown.
      Unknown
    deriving (Show)

-- |The current round status.
-- Note that it can be the case that both the 'QuorumSignatureMessage' and the
-- 'TimeoutSignatureMessage' are present.
-- This is the case if the consensus runner has first signed a block
-- but not enough quorum signature messages were retrieved before timeout.
data RoundStatus = RoundStatus
    { -- |The highest 'Epoch' that the consensus runner participated in.
      rsCurrentEpoch :: !Epoch,
      -- |The highest 'Round' that the consensus runner participated in.
      rsCurrentRound :: !Round,
      -- |The 'QuorumSignatureMessage's for the current 'Round'.
      rsCurrentQuorumSignatureMessages :: !(SignatureMessages QuorumSignatureMessage),
      -- |The 'TimeoutSignatureMessage's for the current 'Round'.
      rsCurrentTimeoutSignatureMessages :: !(SignatureMessages TimeoutSignatureMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed 'QuorumSignatureMessage'
      rsLastSignedQuourumSignatureMessage :: !(Option QuorumSignatureMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed timeout message.
      rsLastSignedTimeoutSignatureMessage :: !(Option TimeoutSignatureMessage),
      -- |The current timeout.
      rsCurrentTimeout :: !Duration,
      -- |The highest 'QuorumCertificate' seen so far.
      -- This is 'Nothing' if no rounds since genesis has
      -- been able to produce a 'QuorumCertificate'.
      rsHighestQC :: !(Option QuorumCertificate),
      -- |The current 'LeadershipElectionNonce'.
      rsLeadershipElectionNonce :: !LeadershipElectionNonce,
      -- |The latest 'Epoch' 'FinalizationEntry'.
      -- This will only be 'Nothing' in between the
      -- genesis block and the first explicitly finalized block.
      rsLatestEpochFinEntry :: !(Option FinalizationEntry),
      -- |The previous round timeout certificate if the previous round timed out.
      -- This is @Just (TimeoutCertificate, QuorumCertificate)@ if the previous round timed out or otherwise 'Nothing'.
      -- In the case of @Just@ then the associated 'QuorumCertificate' is the highest 'QuorumCertificate' at the time
      -- that the 'TimeoutCertificate' was built.
      rsPreviousRoundTC :: !(Option (TimeoutCertificate, QuorumCertificate))
    }
    deriving (Show, Eq)

-- |Advance the provided 'RoundStatus' to the provided 'Round'.
--
-- The consensus protocol can advance round in two ways.
-- 1. Via a QC i.e. @Right QuorumCertificate@
-- 2. Via a TC i.e. @Left (TimeoutCertificate, QuorumCertificate)@
--
-- All properties from the old 'RoundStatus' are being carried over to new 'RoundStatus'
-- except for the following.
-- * 'rsCurrentRound' will become the provided 'Round'.
-- * 'rsCurrentQuorumSignatureMessages' will be 'emptySignatureMessages'.
-- * 'rsCurrentTimeoutSignatureMessages' will be 'emptySignatureMessages'.
-- * 'rsPreviousRoundTC' will become 'Absent' if we're progressing via a 'QuorumCertificate' otherwise
--   it will become the values of the supplied @Left (TimeoutCertificate, QuorumCertificate)@.
-- * 'rsHighestQC' will become the supplied @Right QuorumCertificate@ otherwise it is carried over.
advanceRoundStatus ::
    -- |The round to advance to.
    Round ->
    -- |@Left (tc, qc)@ if consensus is advancing from a TC.
    -- @Right qc@ if consensus is advancing from a QC.
    Either (TimeoutCertificate, QuorumCertificate) QuorumCertificate ->
    -- |The 'RoundStatus' we are advancing from.
    RoundStatus ->
    -- |The advanced 'RoundStatus'.
    RoundStatus
advanceRoundStatus toRound (Left (tc, qc)) RoundStatus{..} =
    RoundStatus
        { rsCurrentRound = toRound,
          rsCurrentQuorumSignatureMessages = emptySignatureMessages,
          rsCurrentTimeoutSignatureMessages = emptySignatureMessages,
          rsPreviousRoundTC = Present (tc, qc),
          ..
        }
advanceRoundStatus toRound (Right qc) RoundStatus{..} =
    RoundStatus
        { rsCurrentRound = toRound,
          rsCurrentQuorumSignatureMessages = emptySignatureMessages,
          rsCurrentTimeoutSignatureMessages = emptySignatureMessages,
          rsHighestQC = Present qc,
          rsPreviousRoundTC = Absent,
          ..
        }

-- |Advance the proived 'RoundStatus' to the provided 'Epoch'.
-- In particular this does the following to the provided 'RoundStatus'
--
-- * Set the 'rsCurrentEpoch' to the provided 'Epoch'
-- * Set the 'rsLatestEpochFinEntry' to the provided 'FinalizationEntry'.
-- * Set the 'rsLeadershipElectionNonce' to the provided 'LeadershipElectionNonce'.
advanceRoundStatusEpoch ::
    -- |The 'Epoch' we advance to.
    Epoch ->
    -- |The 'FinalizationEntry' that witnesses the
    -- new 'Epoch'.
    FinalizationEntry ->
    -- |The new leader election nonce.
    LeadershipElectionNonce ->
    -- |The 'RoundStatus' we're progressing from.
    RoundStatus ->
    -- |The new 'RoundStatus'.
    RoundStatus
advanceRoundStatusEpoch toEpoch latestFinalizationEntry newLeadershipElectionNonce RoundStatus{..} =
    RoundStatus
        { rsCurrentEpoch = toEpoch,
          rsLatestEpochFinEntry = Present latestFinalizationEntry,
          rsLeadershipElectionNonce = newLeadershipElectionNonce,
          ..
        }

instance Serialize RoundStatus where
    put RoundStatus{..} = do
        put rsCurrentEpoch
        put rsCurrentRound
        put rsCurrentQuorumSignatureMessages
        put rsCurrentTimeoutSignatureMessages
        put rsLastSignedQuourumSignatureMessage
        put rsLastSignedTimeoutSignatureMessage
        put rsCurrentTimeout
        put rsHighestQC
        put rsLeadershipElectionNonce
        put rsLatestEpochFinEntry
        put rsPreviousRoundTC
    get = do
        rsCurrentEpoch <- get
        rsCurrentRound <- get
        rsCurrentQuorumSignatureMessages <- get
        rsCurrentTimeoutSignatureMessages <- get
        rsLastSignedQuourumSignatureMessage <- get
        rsLastSignedTimeoutSignatureMessage <- get
        rsCurrentTimeout <- get
        rsHighestQC <- get
        rsLeadershipElectionNonce <- get
        rsLatestEpochFinEntry <- get
        rsPreviousRoundTC <- get
        return RoundStatus{..}

-- |The 'RoundStatus' for consensus at genesis.
initialRoundStatus :: Duration -> LeadershipElectionNonce -> RoundStatus
initialRoundStatus baseTimeout leNonce =
    RoundStatus
        { rsCurrentEpoch = 0,
          rsCurrentRound = 0,
          rsCurrentQuorumSignatureMessages = emptySignatureMessages,
          rsCurrentTimeoutSignatureMessages = emptySignatureMessages,
          rsLastSignedQuourumSignatureMessage = Absent,
          rsLastSignedTimeoutSignatureMessage = Absent,
          rsCurrentTimeout = baseTimeout,
          rsHighestQC = Absent,
          rsLeadershipElectionNonce = leNonce,
          rsLatestEpochFinEntry = Absent,
          rsPreviousRoundTC = Absent
        }

-- |The sets of bakers and finalizers for an epoch/payday.
data BakersAndFinalizers = BakersAndFinalizers
    { -- |Bakers set.
      _bfBakers :: !FullBakers,
      -- |Finalizers set.
      _bfFinalizers :: !FinalizationCommittee
    }

makeLenses ''BakersAndFinalizers

-- |The bakers and finalizers associated with the current and next epoch (with respect to a
-- particular epoch).
data EpochBakers = EpochBakers
    { -- |The current epoch under consideration.
      _epochBakersEpoch :: !Epoch,
      -- |The bakers and finalizers for the previous epoch.
      -- (If the current epoch is 0, then this is the same as the bakers and finalizers for the
      -- current epoch.)
      _previousEpochBakers :: !BakersAndFinalizers,
      -- |The bakers and finalizers for the current epoch.
      _currentEpochBakers :: !BakersAndFinalizers,
      -- |The bakers and finalizers for the next epoch.
      _nextEpochBakers :: !BakersAndFinalizers,
      -- |The first epoch of the next payday. The set of bakers is fixed for an entire payday, and
      -- so the '_currentEpochBakers' apply for all epochs @e@ with
      -- @_epochBakersEpoch <= e < _nextPayday@.
      _nextPayday :: !Epoch
    }

makeClassy ''EpochBakers
