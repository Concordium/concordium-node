{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Concordium.KonsensusV1.TreeState.Types where

import qualified Data.ByteString as BS
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
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
      -- consensus layer.
      -- Hence this timestamp indicates a point in time just before
      -- the block is being deserialized and further processed.
      bmReceiveTime :: !UTCTime,
      -- |The time that the block has become live,
      -- i.e. it has been processed and current head of the chain.
      bmArriveTime :: !UTCTime,
      -- |Energy cost of all transactions in the block.
      bmEnergyCost :: !Energy,
      -- |Size of the transaction data in bytes.
      bmTransactionsSize :: !Word64
    }
    deriving (Eq, Show)

instance Serialize BlockMetadata where
    put BlockMetadata{..} = do
        put bmHeight
        putUTCPOSIXMicros bmReceiveTime
        putUTCPOSIXMicros bmArriveTime
        put bmEnergyCost
        putWord64be bmTransactionsSize
      where
        putUTCPOSIXMicros = putWord64be . floor . (1_000_000 *) . utcTimeToPOSIXSeconds
    get = do
        bmHeight <- get
        bmReceiveTime <- getUTCPOSIXMicros
        bmArriveTime <- getUTCPOSIXMicros
        bmEnergyCost <- get
        bmTransactionsSize <- getWord64be
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

    -- |The time that the block is received by the consensus layer (i.e. just before it is deserialized and processed).
    blockReceiveTime :: bm -> UTCTime
    blockReceiveTime = bmReceiveTime . blockMetadata
    {-# INLINE blockReceiveTime #-}

    -- |The time that the block becomes live (i.e. it is processed and added to the chain).
    blockArriveTime :: bm -> UTCTime
    blockArriveTime = bmArriveTime . blockMetadata
    {-# INLINE blockArriveTime #-}

    -- |The total energy usage in executing the transactions in the block.
    blockEnergyCost :: bm -> Energy
    blockEnergyCost = bmEnergyCost . blockMetadata
    {-# INLINE blockEnergyCost #-}

    -- |The size in bytes of the transactions in the block.
    blockTransactionsSize :: bm -> Word64
    blockTransactionsSize = bmTransactionsSize . blockMetadata
    {-# INLINE blockTransactionsSize #-}

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

deserializeExactVersionedPendingBlock :: SProtocolVersion pv -> BS.ByteString -> UTCTime -> Either String PendingBlock
deserializeExactVersionedPendingBlock spv blockBS recTime =
    case runGet (getSignedBlock spv (utcTimeToTransactionTime recTime)) blockBS of
        Left err -> Left $ "Block deserialization failed: " ++ err
        Right signedBlock -> Right $ PendingBlock signedBlock recTime

-- |Status of a transaction.
data TransactionStatus
    = -- |The transaction is either pending (i.e. not in a block) or committed (i.e. in a
      -- non-finalized block).
      Live !LiveTransactionStatus
    | -- |The transaction is in a finalized block.
      Finalized !FinalizedTransactionStatus
    deriving (Eq, Show)

-- |The status of a block.
-- Note as we use a COMPLETE pragma below for aggregating the 'BlockAlive' and 'BlockFinalized'
-- in a pattern match, then if 'BlockStatus pv' is to be modified the complete pragma MUST also be
-- checked whether it is still sufficient.
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
-- Note as we use a COMPLETE pragma for the 'BlockStatus pv' variants (see below)
-- then it MUST be considered if this function has to change if the type ('BlockStatus pv')
-- is to be modified.
blockStatusBlock :: BlockStatus pv -> Maybe (BlockPointer pv)
blockStatusBlock (BlockAlive b) = Just b
blockStatusBlock (BlockFinalized b) = Just b
blockStatusBlock _ = Nothing

-- |Returns 'True' just when the 'BlockStatus' is either 'BlockPending' or 'BlockUnknown'.
isPendingOrUnknown :: BlockStatus pv -> Bool
isPendingOrUnknown BlockPending{} = True
isPendingOrUnknown BlockUnknown = True
isPendingOrUnknown _ = False

-- |A (unidirectional) pattern for matching a block status that is either alive or finalized.
pattern BlockAliveOrFinalized :: BlockPointer pv -> BlockStatus pv
pattern BlockAliveOrFinalized b <- (blockStatusBlock -> Just b)

-- |A (unidirectional) pattern for matching a block status that is either pending or unknown.
pattern BlockPendingOrUnknown :: BlockStatus pv
pattern BlockPendingOrUnknown <- (isPendingOrUnknown -> True)

-- This tells GHC that these patterns are complete for 'BlockStatus'.
{-# COMPLETE BlockPending, BlockAliveOrFinalized, BlockDead, BlockUnknown #-}
{-# COMPLETE BlockPendingOrUnknown, BlockAlive, BlockFinalized, BlockDead #-}
{-# COMPLETE BlockPendingOrUnknown, BlockAliveOrFinalized, BlockDead #-}

-- |The status of a block as obtained without loading the block from disk.
data RecentBlockStatus pv
    = -- |The block is recent i.e. it is either 'Alive',
      -- 'Pending' or the last finalized block.
      RecentBlock !(BlockStatus pv)
    | -- |The block is a predecessor of the last finalized block.
      OldFinalized
    deriving (Show)

-- |Round status information that is persisted to the database.
data PersistentRoundStatus = PersistentRoundStatus
    { -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed 'QuorumMessage'
      _prsLastSignedQuorumMessage :: !(Option QuorumMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed timeout message.
      _prsLastSignedTimeoutMessage :: !(Option TimeoutMessage),
      -- |The round number of the last round for which we baked a block, or 0 if we have never
      -- baked a block.
      _prsLastBakedRound :: !Round
    }
    deriving (Eq, Show)

makeLenses ''PersistentRoundStatus

instance Serialize PersistentRoundStatus where
    put PersistentRoundStatus{..} = do
        put _prsLastSignedQuorumMessage
        put _prsLastSignedTimeoutMessage
        put _prsLastBakedRound
    get = do
        _prsLastSignedQuorumMessage <- get
        _prsLastSignedTimeoutMessage <- get
        _prsLastBakedRound <- get
        return PersistentRoundStatus{..}

-- |The 'PersistentRoundStatus' at genesis.
initialPersistentRoundStatus :: PersistentRoundStatus
initialPersistentRoundStatus =
    PersistentRoundStatus
        { _prsLastSignedQuorumMessage = Absent,
          _prsLastSignedTimeoutMessage = Absent,
          _prsLastBakedRound = 0
        }

-- |The last signed round (according to a given 'RoundStatus') for which we have produced a
-- quorum or timeout signature message.
prsLastSignedRound :: PersistentRoundStatus -> Round
prsLastSignedRound PersistentRoundStatus{..} =
    max
        (ofOption 0 qmRound _prsLastSignedQuorumMessage)
        (ofOption 0 (tmRound . tmBody) _prsLastSignedTimeoutMessage)

-- |The next signable round is the round after the latest round for which we have
prsNextSignableRound :: PersistentRoundStatus -> Round
prsNextSignableRound = (1 +) . prsLastSignedRound

-- |A valid quorum certificate together with the block that is certified.
--
-- * @qcBlock cbQuorumCertificate == getHash cbQuorumBlock@
-- * @qcRound cbQuorumCertificate == blockRound cbQuorumBlock@
-- * @qcEpoch cbQuorumCertificate == blockEpoch cbQuorumBlock@
data CertifiedBlock (pv :: ProtocolVersion) = CertifiedBlock
    { -- |A valid quorum certificate.
      cbQuorumCertificate :: !QuorumCertificate,
      -- |The certified block.
      cbQuorumBlock :: !(BlockPointer pv)
    }
    deriving (Eq, Show)

-- |The 'Round' number of a certified block.
cbRound :: CertifiedBlock pv -> Round
cbRound = qcRound . cbQuorumCertificate

-- |The 'Epoch' number of a certified block
cbEpoch :: CertifiedBlock pv -> Epoch
cbEpoch = qcEpoch . cbQuorumCertificate

-- |Details of a round timeout that can be used to produce a new block in round
-- @tcRound trTimeoutCertificate + 1@. We require that the 'QuorumCertificate' and
-- 'TimeoutCertificate' are valid and they are compatible in the following sense:
--
--   * @cbRound rtCertifiedBlock < tcRound rtTimeoutCertificate@
--   * @cbRound rtCertifiedBlock >= tcMaxRound rtTimeoutCertificate@
--   * @cbEpoch rtCertifiedBlock >= tcMaxEpoch rtTimeoutCertificate@
--   * @cbEpoch rtCertifiedBlock <= 2 + tcMinEpoch rtTimeoutCertificate@
data RoundTimeout (pv :: ProtocolVersion) = RoundTimeout
    { -- |A timeout certificate.
      rtTimeoutCertificate :: !TimeoutCertificate,
      -- |Certified block for the highest known round that did not time out.
      rtCertifiedBlock :: !(CertifiedBlock pv)
    }
    deriving (Eq, Show)

-- |The current round status.
-- Note that it can be the case that both the 'QuorumSignatureMessage' and the
-- 'TimeoutSignatureMessage' are present.
-- This is the case if the consensus runner has first signed a block
-- but not enough quorum signature messages were retrieved before timeout.
--
-- INVARIANTS:
--
--  * @_rsCurrentRound > qcEpoch (cbQuorumCertificate _rsHighestCertifiedBlock)@.
--
--  * If @_rsPreviousRoundTimeout = Absent@ then
--    @_rsCurrentRound = 1 + qcEpoch (cbQuorumCertificate _rsHighestCertifiedBlock)@.
--
--  * If @_rsPreviousRoundTimeout = Present timeout@ then
--    @_rsCurrentRound = 1 + qcEpoch (rtQuorumCertificate timeout)@.
data RoundStatus (pv :: ProtocolVersion) = RoundStatus
    { -- |The current 'Round'.
      _rsCurrentRound :: !Round,
      -- |The 'CertifiedBlock' with the highest 'QuorumCertificate' we have seen so far.
      -- (At genesis, this contains the empty quorum certificate.)
      _rsHighestCertifiedBlock :: !(CertifiedBlock pv),
      -- |The previous round timeout certificate if the previous round timed out.
      -- This is @Present (timeoutCertificate, quorumCertificate)@ if the previous round timed out
      -- and otherwise 'Absent'. In the case of @Present@ then @quorumCertificate@ is the highest
      -- 'QuorumCertificate' at the time that the 'TimeoutCertificate' was built.
      _rsPreviousRoundTimeout :: !(Option (RoundTimeout pv)),
      -- |Flag that is 'True' if we should attempt to bake for the current round.
      -- This is set to 'True' when the round is advanced, and set to 'False' when we have attempted
      -- to bake for the round.
      _rsRoundEligibleToBake :: !Bool,
      -- |The current epoch.
      _rsCurrentEpoch :: !Epoch,
      -- |If present, an epoch finalization entry for @_currentEpoch - 1@. An entry MUST be
      -- present if @_currentEpoch > blockEpoch _lastFinalized@. Otherwise, an entry MAY be present,
      -- but is not required.
      --
      -- The purpose of this field is to support the creation of a block that is the first in a new
      -- epoch. It should
      _rsLastEpochFinalizationEntry :: !(Option FinalizationEntry),
      -- |The current duration to wait before a round times out.
      _rsCurrentTimeout :: !Duration
    }
    deriving (Eq, Show)

makeLenses ''RoundStatus

-- |The 'RoundStatus' for consensus at genesis.
initialRoundStatus ::
    -- |The base timeout.
    Duration ->
    -- |The 'BlockPointer' of the genesis block.
    BlockPointer pv ->
    -- |The initial 'RoundStatus'.
    RoundStatus pv
initialRoundStatus currentTimeout genesisBlock =
    RoundStatus
        { _rsCurrentRound = 1,
          _rsHighestCertifiedBlock =
            CertifiedBlock
                { cbQuorumCertificate = genesisQuorumCertificate (getHash genesisBlock),
                  cbQuorumBlock = genesisBlock
                },
          _rsPreviousRoundTimeout = Absent,
          _rsRoundEligibleToBake = True,
          _rsCurrentEpoch = 0,
          _rsLastEpochFinalizationEntry = Absent,
          _rsCurrentTimeout = currentTimeout
        }

-- |The sets of bakers and finalizers for an epoch/payday.
data BakersAndFinalizers = BakersAndFinalizers
    { -- |Bakers set.
      _bfBakers :: !FullBakers,
      -- |Finalizers set.
      _bfFinalizers :: !FinalizationCommittee
    }
    deriving (Eq, Show)

makeLenses ''BakersAndFinalizers

-- |The bakers and finalizers associated with the previous, current and next epochs (with respect
-- to a particular epoch).
data EpochBakers = EpochBakers
    { -- |The bakers and finalizers for the previous epoch.
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
      -- The 'VoterPower' here is in relation to the 'Epoch' of the block being finalized.
      _smBlockToWeightsAndSignatures :: !(Map.Map BlockHash (VoterPower, QuorumSignature, FinalizerSet))
    }
    deriving (Eq, Show)

makeLenses ''QuorumMessages

-- |Construct an empty 'QuorumMessages'
emptyQuorumMessages :: QuorumMessages
emptyQuorumMessages = QuorumMessages Map.empty Map.empty

-- |A collection of timeout messages with respect to the current round and for most two consecutive epochs.
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
    deriving (Eq, Show)
