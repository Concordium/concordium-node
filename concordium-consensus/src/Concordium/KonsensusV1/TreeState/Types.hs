{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.TreeState.Types where

import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo

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
    { bmHeight :: !BlockHeight,
      bmReceiveTime :: !UTCTime,
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

-- |The `Eq` instance checks that the info and blocks are the same, and that the states have the
-- same hash. This is intended for testing purposes: for practical purposes, it should be sufficient
-- to check equality of block hashes.
instance Eq (BlockPointer pv) where
    bp1 == bp2 =
        bpInfo bp1 == bpInfo bp2
            && bpBlock bp1 == bpBlock bp2
            && PBS.hpbsHash (bpState bp1) == PBS.hpbsHash (bpState bp2)

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
    = -- |The transaction is either pending (i.e. not in a block) of committed (i.e. in a
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
    deriving (Eq, Show)

-- |The status of a block as obtained without loading the block from disk.
data RecentBlockStatus pv
    = -- |The block is recent i.e. it is either 'Alive',
      -- 'Pending' or the last finalized block.
      RecentBlock !(BlockStatus pv)
    | -- |The block is a predecessor of the last finalized block.
      OldFinalized
    | -- |The block is unknown.
      Unknown
    deriving (Eq, Show)

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
      -- |The 'QuourumSignatureMessage's for the current 'Round'.
      rsCurrentQuorumSignatureMessages :: !(SignatureMessages QuorumSignatureMessage),
      -- |The 'TimeoutSignatureMessage's for the current 'Round'.
      rsCurrentTimeoutSignatureMessages :: !(SignatureMessages TimeoutSignatureMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed 'QuorumSignatureMessage'
      rsLastSignedQuouromSignatureMessage :: !(Maybe QuorumSignatureMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed timeout message.
      rsLastSignedTimeoutSignatureMessage :: !(Maybe TimeoutSignatureMessage),
      -- |The current timeout.
      rsCurrentTimeout :: !Duration,
      -- |The highest 'QuorumCertificate' seen so far.
      -- This is 'Nothing' if no rounds since genesis has
      -- been able to produce a 'QuorumCertificate'.
      rsHighestQC :: !(Maybe QuorumCertificate),
      -- |The current 'LeadershipElectionNonce'.
      rsLeadershipElectionNonce :: !LeadershipElectionNonce,
      -- |The latest 'Epoch' 'FinalizationEntry'.
      -- This will only be 'Nothing' in between the
      -- genesis block and the first explicitly finalized block.
      rsLatestEpochFinEntry :: !(Maybe FinalizationEntry),
      -- |The previous round timeout certificate if the previous round timed out.
      -- This is @Just (TimeoutCertificate, QuorumCertificate)@ if the previous round timed out or otherwise 'Nothing'.
      -- In the case of @Just@ then the associated 'QuorumCertificate' is the highest 'QuorumCertificate' at the time
      -- that the 'TimeoutCertificate' was built.
      rsPreviousRoundTC :: !(Maybe (TimeoutCertificate, QuorumCertificate))
    }

instance Serialize RoundStatus where
    put RoundStatus{..} = do
        put rsCurrentEpoch
        put rsCurrentRound
        put rsCurrentQuorumSignatureMessages
        put rsCurrentTimeoutSignatureMessages
        put rsLastSignedQuouromSignatureMessage
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
        rsLastSignedQuouromSignatureMessage <- get
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
          rsLastSignedQuouromSignatureMessage = Nothing,
          rsLastSignedTimeoutSignatureMessage = Nothing,
          rsCurrentTimeout = baseTimeout,
          rsHighestQC = Nothing,
          rsLeadershipElectionNonce = leNonce,
          rsLatestEpochFinEntry = Nothing,
          rsPreviousRoundTC = Nothing
        }
