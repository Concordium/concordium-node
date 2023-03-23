{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Concordium.KonsensusV1.TreeState.Types where

import Data.Function
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Set as Set
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

-- |A class for structures that canonincally include 'BlockMetadata'.
class HasBlockMetadata bm where
    -- |Get the block metadata.
    blockMetadata :: bm -> BlockMetadata

    -- |The height of the block.
    blockHeight :: bm -> BlockHeight
    blockHeight = bmHeight . blockMetadata
    {-# INLINE blockHeight #-}

    -- |The time that the block is received by the consensus layer (i.e. when it is deserialized).
    blockReceiveTime :: bm -> UTCTime
    blockReceiveTime = bmReceiveTime . blockMetadata
    {-# INLINE blockReceiveTime #-}

    -- |The time that the block becomes live (i.e. it is processed and added to the chain).
    blockArriveTime :: bm -> UTCTime
    blockArriveTime = bmArriveTime . blockMetadata
    {-# INLINE blockArriveTime #-}

instance HasBlockMetadata BlockMetadata where
    blockMetadata = id

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

-- |Block pointer equality is defined on the block hash.
instance Eq (BlockPointer pv) where
    (==) = on (==) (getHash @BlockHash)

instance BlockData (BlockPointer pv) where
    type BakedBlockDataType (BlockPointer pv) = SignedBlock
    blockRound = blockRound . bpBlock
    blockEpoch = blockEpoch . bpBlock
    blockTimestamp = blockTimestamp . bpBlock
    blockBakedData = blockBakedData . bpBlock
    blockTransactions = blockTransactions . bpBlock
    blockTransactionCount = blockTransactionCount . bpBlock
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

instance HasBlockMetadata (BlockPointer pv) where
    blockMetadata = bpInfo

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
    blockTransactionCount = blockTransactionCount . pbBlock
    blockStateHash = blockStateHash . pbBlock

instance BakedBlockData PendingBlock where
    blockQuorumCertificate = blockQuorumCertificate . pbBlock
    blockParent = blockParent . pbBlock
    blockBaker = blockBaker . pbBlock
    blockTimeoutCertificate = blockTimeoutCertificate . pbBlock
    blockEpochFinalizationEntry = blockEpochFinalizationEntry . pbBlock
    blockNonce = blockNonce . pbBlock
    blockSignature = blockSignature . pbBlock
    blockTransactionOutcomesHash = blockTransactionOutcomesHash . pbBlock

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
    | -- |The block is alive.
      BlockAlive !(BlockPointer pv)
    | -- |The block is finalized.
      BlockFinalized !(BlockPointer pv)
    | -- |The block has been marked dead.
      BlockDead
    | -- |The block is unknown
      BlockUnknown
    deriving (Show)

-- |Get the 'BlockPointer' from a 'BlockStatus' for a live or finalized block.
-- Returns 'Nothing' if the block is pending, dead or unknown.
blockStatusBlock :: BlockStatus pv -> Maybe (BlockPointer pv)
blockStatusBlock (BlockAlive b) = Just b
blockStatusBlock (BlockFinalized b) = Just b
blockStatusBlock _ = Nothing

-- |A (unidirectional) pattern for matching a block status that is either alive or finalized.
pattern BlockAliveOrFinalized :: BlockPointer pv -> BlockStatus pv
pattern BlockAliveOrFinalized b <- (blockStatusBlock -> Just b)

-- |The status of a block as obtained without loading the block from disk.
data RecentBlockStatus pv
    = -- |The block is recent i.e. it is either 'Alive',
      -- 'Pending' or the last finalized block.
      RecentBlock !(BlockStatus pv)
    | -- |The block is a predecessor of the last finalized block.
      OldFinalized
    deriving (Show)

-- |The current round status.
-- Note that it can be the case that both the 'QuorumSignatureMessage' and the
-- 'TimeoutSignatureMessage' are present.
-- This is the case if the consensus runner has first signed a block
-- but not enough quorum signature messages were retrieved before timeout.
data RoundStatus = RoundStatus
    { -- |The highest 'Round' that the consensus runner participated in.
      _rsCurrentRound :: !Round,
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed 'QuorumMessage'
      _rsLastSignedQuorumMessage :: !(Option QuorumMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed timeout message.
      _rsLastSignedTimeoutMessage :: !(Option TimeoutMessage),
      -- |The highest 'QuorumCertificate' seen so far.
      -- This contains the empty quorom certificate
      -- (having round 0, epoch 0, empty quorom signature and empty finalizer set)
      -- if no rounds since genesis has been able to produce a 'QuorumCertificate'.
      -- Note: this can potentially be a QC for a block that is not present, but in that case we
      -- should have a finalization entry that contains the QC.
      _rsHighestQC :: !QuorumCertificate,
      -- |The previous round timeout certificate if the previous round timed out.
      -- This is @Just (TimeoutCertificate, QuorumCertificate)@ if the previous round timed out or otherwise 'Nothing'.
      -- In the case of @Just@ then the associated 'QuorumCertificate' is the highest 'QuorumCertificate' at the time
      -- that the 'TimeoutCertificate' was built.
      _rsPreviousRoundTC :: !(Option (TimeoutCertificate, QuorumCertificate))
    }
    deriving (Show, Eq)

makeLenses ''RoundStatus

instance Serialize RoundStatus where
    put RoundStatus{..} = do
        put _rsCurrentRound
        put _rsLastSignedQuorumMessage
        put _rsLastSignedTimeoutMessage
        put _rsHighestQC
        put _rsPreviousRoundTC
    get = do
        _rsCurrentRound <- get
        _rsLastSignedQuorumMessage <- get
        _rsLastSignedTimeoutMessage <- get
        _rsHighestQC <- get
        _rsPreviousRoundTC <- get
        return RoundStatus{..}

-- |The 'RoundStatus' for consensus at genesis.
initialRoundStatus :: BlockHash -> RoundStatus
initialRoundStatus genesisHash =
    RoundStatus
        { _rsCurrentRound = 0,
          _rsLastSignedQuorumMessage = Absent,
          _rsLastSignedTimeoutMessage = Absent,
          _rsHighestQC = genesisQuorumCertificate genesisHash,
          _rsPreviousRoundTC = Absent
        }

-- |The last signed round (according to a given 'RoundStatus') for which we have produced a
-- quorum or timeout signature message.
rsLastSignedRound :: RoundStatus -> Round
rsLastSignedRound RoundStatus{..} =
    max
        (ofOption 0 qmRound _rsLastSignedQuorumMessage)
        (ofOption 0 (tmRound . tmBody) _rsLastSignedTimeoutMessage)

-- |The next signable round is the round after the latest round for which we have
rsNextSignableRound :: RoundStatus -> Round
rsNextSignableRound = (1 +) . rsLastSignedRound

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
      _currentEpoch :: !Epoch,
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
      -- @_currentEpoch <= e < _nextPayday@.
      _nextPayday :: !Epoch
    }

makeClassy ''EpochBakers

-- |Quorum messages collected for a round.
data QuorumMessages = QuorumMessages
    { -- |Map of finalizer indices to signature messages.
      _smFinalizerToQuorumMessage :: !(Map.Map FinalizerIndex QuorumMessage),
      -- |Accumulated weights and the aggregated signature for the blocks signed off by quorum signature message.
      -- The 'VoterPower' here is in relation to the running 'Epoch'.
      _smBlockToWeightsAndSignatures :: !(Map.Map BlockHash (VoterPower, QuorumSignature, Set.Set FinalizerIndex))
    }
    deriving (Eq, Show)

makeLenses ''QuorumMessages

-- |Construct an empty 'QuorumMessages'
emptyQuorumMessages :: QuorumMessages
emptyQuorumMessages = QuorumMessages Map.empty Map.empty

-- |A collection of timeout messages for at most two consecutive epochs.
-- INVARIANTS:
--  * 'tmFirstEpochTimeouts' is never empty.
--  * All timeout messages in 'tmFirstEpochTimeouts' have epoch 'tmFirstEpoch' and finalizer index
--    matching the key in the map.
--  * All timeout messages in 'tmSecondEpochTimeouts' have epoch @tmFirstEpoch + 1@ and finalizer
--    index matching the key in the map.
data TimeoutMessages
    = -- |Timeout messages for one epoch or two consecutive epochs.
      TimeoutMessages
      { -- |First epoch for which we have timeout messages.
        tmFirstEpoch :: Epoch,
        -- |Timeout messages for epoch 'tmFirstEpoch' indexed by the 'FinalizerIndex'.
        tmFirstEpochTimeouts :: !(Map.Map FinalizerIndex TimeoutMessage),
        -- |Timeout messages for epoch @tmFirstEpoch + 1@ indexed by the 'FinalizerIndex'.
        tmSecondEpochTimeouts :: !(Map.Map FinalizerIndex TimeoutMessage)
      }
