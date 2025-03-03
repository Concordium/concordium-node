{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Concordium.KonsensusV1.TreeState.Types where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Function
import qualified Data.Map.Strict as Map
import qualified Data.ProtoLens.Combinators as Proto
import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Lens.Micro.Platform

import Concordium.GRPC2 (ToProto (..))
import Concordium.Types
import qualified Concordium.Types.Conditionally as Cond
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import qualified Proto.V2.Concordium.Types as Proto
import qualified Proto.V2.Concordium.Types_Fields as ProtoFields

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.TransactionTable
import Concordium.KonsensusV1.Types
import Concordium.Types.Option (Option (..), ofOption)

-- | Status information for a finalized transaction.
data FinalizedTransactionStatus = FinalizedTransactionStatus
    { -- | Height of the finalized block that contains this transaction
      ftsBlockHeight :: !BlockHeight,
      -- | Index of the transaction in the block.
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

-- | Metadata about a block that has been executed.
data BlockMetadata pv = BlockMetadata
    { -- | The height of the block.
      bmHeight :: !BlockHeight,
      -- | The time that the block is received by the
      --  consensus layer.
      --  Hence this timestamp indicates a point in time just before
      --  the block is being deserialized and further processed.
      bmReceiveTime :: !UTCTime,
      -- | The time that the block has become live,
      --  i.e. it has been processed and current head of the chain.
      bmArriveTime :: !UTCTime,
      -- | Energy cost of all transactions in the block.
      bmEnergyCost :: !Energy,
      -- | Size of the transaction data in bytes.
      bmTransactionsSize :: !Word64,
      -- | The block state hash, only present for P7 and onwards.
      bmBlockStateHash ::
        !( Cond.Conditionally
            (BlockStateHashInMetadata (BlockHashVersionFor pv))
            StateHash
         )
    }
    deriving (Eq, Show)

type instance BlockProtocolVersion (BlockMetadata pv) = pv

instance forall pv. (IsProtocolVersion pv) => Serialize (BlockMetadata pv) where
    put BlockMetadata{..} = do
        put bmHeight
        putUTCPOSIXMicros bmReceiveTime
        putUTCPOSIXMicros bmArriveTime
        put bmEnergyCost
        putWord64be bmTransactionsSize
        mapM_ put bmBlockStateHash
      where
        putUTCPOSIXMicros = putWord64be . floor . (1_000_000 *) . utcTimeToPOSIXSeconds
    get = do
        bmHeight <- get
        bmReceiveTime <- getUTCPOSIXMicros
        bmArriveTime <- getUTCPOSIXMicros
        bmEnergyCost <- get
        bmTransactionsSize <- getWord64be
        bmBlockStateHash <-
            Cond.conditionallyA
                (sBlockStateHashInMetadata (sBlockHashVersionFor (protocolVersion @pv)))
                get
        return BlockMetadata{..}
      where
        getUTCPOSIXMicros = posixSecondsToUTCTime . (/ 1_000_000) . realToFrac <$> getWord64be

-- | A class for structures that include 'BlockMetadata'.
class HasBlockMetadata bm where
    -- | Get the block metadata.
    blockMetadata :: bm -> BlockMetadata (BlockProtocolVersion bm)

    -- | The height of the block.
    blockHeight :: bm -> BlockHeight
    blockHeight = bmHeight . blockMetadata
    {-# INLINE blockHeight #-}

    -- | The time that the block is received by the consensus layer (i.e. just before it is deserialized and processed).
    blockReceiveTime :: bm -> UTCTime
    blockReceiveTime = bmReceiveTime . blockMetadata
    {-# INLINE blockReceiveTime #-}

    -- | The time that the block becomes live (i.e. it is processed and added to the chain).
    blockArriveTime :: bm -> UTCTime
    blockArriveTime = bmArriveTime . blockMetadata
    {-# INLINE blockArriveTime #-}

    -- | The total energy usage in executing the transactions in the block.
    blockEnergyCost :: bm -> Energy
    blockEnergyCost = bmEnergyCost . blockMetadata
    {-# INLINE blockEnergyCost #-}

    -- | The size in bytes of the transactions in the block.
    blockTransactionsSize :: bm -> Word64
    blockTransactionsSize = bmTransactionsSize . blockMetadata
    {-# INLINE blockTransactionsSize #-}

instance HasBlockMetadata (BlockMetadata pv) where
    blockMetadata = id

-- | A pointer to a block that has been executed
--  and the resulting 'PBS.HashedPersistentBlockState'.
data BlockPointer (pv :: ProtocolVersion) = BlockPointer
    { -- | Metadata for the block.
      bpInfo :: !(BlockMetadata pv),
      -- | The signed block.
      bpBlock :: !(Block pv),
      -- | The resulting state of executing the block.
      bpState :: !(PBS.HashedPersistentBlockState pv)
    }

type instance BlockProtocolVersion (BlockPointer pv) = pv

instance HashableTo BlockHash (BlockPointer pv) where
    getHash BlockPointer{..} = getHash bpBlock

-- | Block pointer equality is defined on the block hash.
instance Eq (BlockPointer pv) where
    (==) = on (==) (getHash @BlockHash)

instance BlockData (BlockPointer pv) where
    type BakedBlockDataType (BlockPointer pv) = SignedBlock pv
    blockRound = blockRound . bpBlock
    blockEpoch = blockEpoch . bpBlock
    blockTimestamp = blockTimestamp . bpBlock
    blockBakedData = blockBakedData . bpBlock
    blockTransactions = blockTransactions . bpBlock
    blockTransaction i = blockTransaction i . bpBlock
    blockTransactionCount = blockTransactionCount . bpBlock

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

-- | A block that is pending its parent.
data PendingBlock (pv :: ProtocolVersion) = PendingBlock
    { -- | The block itself.
      pbBlock :: !(SignedBlock pv),
      -- | The time that the block was received by the consensus.
      pbReceiveTime :: !UTCTime
    }
    deriving (Eq, Show)

type instance BlockProtocolVersion (PendingBlock pv) = pv

instance HashableTo BlockHash (PendingBlock pv) where
    getHash PendingBlock{..} = getHash pbBlock

instance BlockData (PendingBlock pv) where
    type BakedBlockDataType (PendingBlock pv) = SignedBlock pv
    blockRound = blockRound . pbBlock
    blockEpoch = blockEpoch . pbBlock
    blockTimestamp = blockTimestamp . pbBlock
    blockBakedData = blockBakedData . pbBlock
    blockTransactions = blockTransactions . pbBlock
    blockTransaction i = blockTransaction i . pbBlock
    blockTransactionCount = blockTransactionCount . pbBlock

instance BakedBlockData (PendingBlock pv) where
    blockQuorumCertificate = blockQuorumCertificate . pbBlock
    blockParent = blockParent . pbBlock
    blockBaker = blockBaker . pbBlock
    blockTimeoutCertificate = blockTimeoutCertificate . pbBlock
    blockEpochFinalizationEntry = blockEpochFinalizationEntry . pbBlock
    blockNonce = blockNonce . pbBlock
    blockSignature = blockSignature . pbBlock
    blockDerivableHashes = blockDerivableHashes . pbBlock

deserializeExactVersionedPendingBlock ::
    (IsProtocolVersion pv) =>
    BS.ByteString ->
    UTCTime ->
    Either String (PendingBlock pv)
deserializeExactVersionedPendingBlock blockBS recTime =
    case runGet (getSignedBlock (utcTimeToTransactionTime recTime)) blockBS of
        Left err -> Left $ "Block deserialization failed: " ++ err
        Right signedBlock -> Right $ PendingBlock signedBlock recTime

-- | Status of a transaction.
data TransactionStatus
    = -- | The transaction is either pending (i.e. not in a block) or committed (i.e. in a
      --  non-finalized block).
      Live !LiveTransactionStatus
    | -- | The transaction is in a finalized block.
      Finalized !FinalizedTransactionStatus
    deriving (Eq, Show)

-- | The status of a block.
--  Note as we use a COMPLETE pragma below for aggregating the 'BlockAlive' and 'BlockFinalized'
--  in a pattern match, then if 'BlockStatus pv' is to be modified the complete pragma MUST also be
--  checked whether it is still sufficient.
data BlockStatus pv
    = -- | The block is alive.
      BlockAlive !(BlockPointer pv)
    | -- | The block is finalized.
      BlockFinalized !(BlockPointer pv)
    | -- | The block has been marked dead.
      BlockDead
    | -- | The block is unknown
      BlockUnknown
    deriving (Show)

-- | Get the 'BlockPointer' from a 'BlockStatus' for a live or finalized block.
--  Returns 'Nothing' if the block is pending, dead or unknown.
--  Note as we use a COMPLETE pragma for the 'BlockStatus pv' variants (see below)
--  then it MUST be considered if this function has to change if the type ('BlockStatus pv')
--  is to be modified.
blockStatusBlock :: BlockStatus pv -> Maybe (BlockPointer pv)
blockStatusBlock (BlockAlive b) = Just b
blockStatusBlock (BlockFinalized b) = Just b
blockStatusBlock _ = Nothing

-- | A (unidirectional) pattern for matching a block status that is either alive or finalized.
pattern BlockAliveOrFinalized :: BlockPointer pv -> BlockStatus pv
pattern BlockAliveOrFinalized b <- (blockStatusBlock -> Just b)

-- This tells GHC that these patterns are complete for 'BlockStatus'.
{-# COMPLETE BlockUnknown, BlockAliveOrFinalized, BlockDead #-}

-- | The status of a block as obtained without loading the block from disk.
data RecentBlockStatus pv
    = -- | The block is recent i.e. it is either 'Alive',
      --  'Pending' or the last finalized block.
      RecentBlock !(BlockStatus pv)
    | -- | The block is a predecessor of the last finalized block.
      OldFinalized
    deriving (Show)

-- | Round status information that is persisted to the database.
data PersistentRoundStatus = PersistentRoundStatus
    { -- | If the consensus runner is part of the finalization committee,
      --  then this will yield the last signed 'QuorumMessage'
      _prsLastSignedQuorumMessage :: !(Option QuorumMessage),
      -- | If the consensus runner is part of the finalization committee,
      --  then this will yield the last signed timeout message.
      _prsLastSignedTimeoutMessage :: !(Option TimeoutMessage),
      -- | The round number of the last round for which we baked a block, or 0 if we have never
      --  baked a block.
      _prsLastBakedRound :: !Round,
      -- | The latest timeout certificate we have seen. This can be absent if we have a quorum
      --  certificate for a more recent round.
      _prsLatestTimeout :: !(Option TimeoutCertificate)
    }
    deriving (Eq, Show)

makeLenses ''PersistentRoundStatus

instance Serialize PersistentRoundStatus where
    put PersistentRoundStatus{..} = do
        put _prsLastSignedQuorumMessage
        put _prsLastSignedTimeoutMessage
        put _prsLastBakedRound
        put _prsLatestTimeout
    get = do
        _prsLastSignedQuorumMessage <- get
        _prsLastSignedTimeoutMessage <- get
        _prsLastBakedRound <- get
        _prsLatestTimeout <- get
        return PersistentRoundStatus{..}

instance ToProto PersistentRoundStatus where
    type Output PersistentRoundStatus = Proto.PersistentRoundStatus
    toProto PersistentRoundStatus{..} = Proto.make $ do
        mapM_ (assign ProtoFields.lastSignedQuorumMessage . toProto) _prsLastSignedQuorumMessage
        mapM_ (assign ProtoFields.lastSignedTimeoutMessage . toProto) _prsLastSignedTimeoutMessage
        ProtoFields.lastBakedRound .= toProto _prsLastBakedRound
        mapM_ (assign ProtoFields.latestTimeout . toProto) _prsLatestTimeout

-- | The 'PersistentRoundStatus' at genesis.
initialPersistentRoundStatus :: PersistentRoundStatus
initialPersistentRoundStatus =
    PersistentRoundStatus
        { _prsLastSignedQuorumMessage = Absent,
          _prsLastSignedTimeoutMessage = Absent,
          _prsLastBakedRound = 0,
          _prsLatestTimeout = Absent
        }

-- | The last signed round (according to a given 'RoundStatus') for which we have produced a
--  quorum or timeout signature message.
prsLastSignedRound :: PersistentRoundStatus -> Round
prsLastSignedRound PersistentRoundStatus{..} =
    max
        (ofOption 0 qmRound _prsLastSignedQuorumMessage)
        (ofOption 0 (tmRound . tmBody) _prsLastSignedTimeoutMessage)

-- | The next signable round is the round after the latest round for which we have produced a
--  quorum or timeout signature message.
prsNextSignableRound :: PersistentRoundStatus -> Round
prsNextSignableRound = (1 +) . prsLastSignedRound

-- | A valid quorum certificate together with the block that is certified.
--
--  * @qcBlock cbQuorumCertificate == getHash cbQuorumBlock@
--  * @qcRound cbQuorumCertificate == blockRound cbQuorumBlock@
--  * @qcEpoch cbQuorumCertificate == blockEpoch cbQuorumBlock@
data CertifiedBlock (pv :: ProtocolVersion) = CertifiedBlock
    { -- | A valid quorum certificate.
      cbQuorumCertificate :: !QuorumCertificate,
      -- | The certified block.
      cbQuorumBlock :: !(BlockPointer pv)
    }
    deriving (Eq, Show)

-- | The 'Round' number of a certified block.
cbRound :: CertifiedBlock pv -> Round
cbRound = qcRound . cbQuorumCertificate

-- | The 'Epoch' number of a certified block
cbEpoch :: CertifiedBlock pv -> Epoch
cbEpoch = qcEpoch . cbQuorumCertificate

-- | Details of a round timeout that can be used to produce a new block in round
--  @tcRound trTimeoutCertificate + 1@. We require that the 'QuorumCertificate' and
--  'TimeoutCertificate' are valid and they are compatible in the following sense:
--
--    * @cbRound rtCertifiedBlock < tcRound rtTimeoutCertificate@
--    * @cbRound rtCertifiedBlock >= tcMaxRound rtTimeoutCertificate@
--    * @cbEpoch rtCertifiedBlock >= tcMaxEpoch rtTimeoutCertificate@
--    * @cbEpoch rtCertifiedBlock <= 2 + tcMinEpoch rtTimeoutCertificate@
data RoundTimeout (pv :: ProtocolVersion) = RoundTimeout
    { -- | A timeout certificate.
      rtTimeoutCertificate :: !TimeoutCertificate,
      -- | Certified block for the highest known round that did not time out.
      rtCertifiedBlock :: !(CertifiedBlock pv)
    }
    deriving (Eq, Show)

instance ToProto (RoundTimeout pv) where
    type Output (RoundTimeout pv) = Proto.RoundTimeout
    toProto RoundTimeout{..} = Proto.make $ do
        ProtoFields.timeoutCertificate .= toProto rtTimeoutCertificate
        ProtoFields.quorumCertificate .= toProto (cbQuorumCertificate rtCertifiedBlock)

-- | The current round status.
--  Note that it can be the case that both the 'QuorumSignatureMessage' and the
--  'TimeoutSignatureMessage' are present.
--  This is the case if the consensus runner has first signed a block
--  but not enough quorum signature messages were retrieved before timeout.
--
--  INVARIANTS:
--
--   * @_rsCurrentRound > qcRound (cbQuorumCertificate _rsHighestCertifiedBlock)@.
--
--   * If @_rsPreviousRoundTimeout = Absent@ then
--     @_rsCurrentRound = 1 + qcRound (cbQuorumCertificate _rsHighestCertifiedBlock)@.
--
--   * If @_rsPreviousRoundTimeout = Present timeout@ then
--     @_rsCurrentRound = 1 + qcRound (rtQuorumCertificate timeout)@.
data RoundStatus (pv :: ProtocolVersion) = RoundStatus
    { -- | The current 'Round'. If the previous round did not time out, this should be
      --  @1 + cbRound _rsHighestCertifiedBlock@. Otherwise, it should be
      --  @1 + tcRound timeoutCertificate@.
      _rsCurrentRound :: !Round,
      -- | The highest round for which we have sent a finalization message.
      _rsHighestCertifiedBlock :: !(CertifiedBlock pv),
      -- | The previous round timeout certificate if the previous round timed out.
      --  This is @Present (timeoutCertificate, quorumCertificate)@ if the previous round timed out
      --  and otherwise 'Absent'. In the case of @Present@ then @quorumCertificate@ is the highest
      --  'QuorumCertificate' at the time that the 'TimeoutCertificate' was built.
      _rsPreviousRoundTimeout :: !(Option (RoundTimeout pv)),
      -- | Flag that is 'True' if we should attempt to bake for the current round.
      --  This is set to 'True' when the round is advanced, and set to 'False' when we have attempted
      --  to bake for the round.
      _rsRoundEligibleToBake :: !Bool,
      -- | The current epoch. This should either be the same as the epoch of the last finalized
      --  block (if its timestamp is before the trigger block time) or the next epoch from the last
      --  finalized block (if its timestamp is at least the trigger block time).
      _rsCurrentEpoch :: !Epoch,
      -- | If present, an epoch finalization entry for @_currentEpoch - 1@. An entry MUST be
      --  present if @_currentEpoch > blockEpoch _lastFinalized@. Otherwise, an entry MAY be present,
      --  but is not required.
      --
      --  The purpose of this field is to support the creation of a block that is the first in a new
      --  epoch.
      _rsLastEpochFinalizationEntry :: !(Option (FinalizationEntry pv)),
      -- | The current duration to wait before a round times out.
      _rsCurrentTimeout :: !Duration
    }
    deriving (Eq, Show)

makeLenses ''RoundStatus

instance ToProto (RoundStatus pv) where
    type Output (RoundStatus pv) = Proto.RoundStatus
    toProto RoundStatus{..} = Proto.make $ do
        ProtoFields.currentRound .= toProto _rsCurrentRound
        ProtoFields.highestCertifiedBlock .= toProto (cbQuorumCertificate _rsHighestCertifiedBlock)
        forM_ _rsPreviousRoundTimeout $ \timeout ->
            ProtoFields.previousRoundTimeout .= toProto timeout
        ProtoFields.roundEligibleToBake .= _rsRoundEligibleToBake
        ProtoFields.currentEpoch .= toProto _rsCurrentEpoch
        forM_ _rsLastEpochFinalizationEntry $ \finEntry ->
            ProtoFields.lastEpochFinalizationEntry .= toProto finEntry
        ProtoFields.currentTimeout .= toProto _rsCurrentTimeout

-- | The 'RoundStatus' for consensus at genesis.
initialRoundStatus ::
    -- | The base timeout.
    Duration ->
    -- | The 'BlockPointer' of the genesis block.
    BlockPointer pv ->
    -- | The initial 'RoundStatus'.
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

-- | The sets of bakers and finalizers for an epoch/payday.
data BakersAndFinalizers = BakersAndFinalizers
    { -- | Bakers set.
      _bfBakers :: !FullBakers,
      -- | Finalizers set.
      _bfFinalizers :: !FinalizationCommittee,
      -- | Hash computed from the BLS verify key and weight of each finalizer included in the set above.
      _bfFinalizerHash :: !FinalizationCommitteeHash
    }
    deriving (Eq, Show)

makeLenses ''BakersAndFinalizers

instance ToProto BakersAndFinalizers where
    type Output BakersAndFinalizers = Proto.BakersAndFinalizers
    toProto BakersAndFinalizers{..} = Proto.make $ do
        ProtoFields.bakers .= fmap toProto (toList . fullBakerInfos $ _bfBakers)
        ProtoFields.finalizers
            .= fmap (toProto . finalizerBakerId) (toList . committeeFinalizers $ _bfFinalizers)
        ProtoFields.bakerTotalStake .= toProto (bakerTotalStake _bfBakers)
        ProtoFields.finalizerTotalStake
            .= toProto (weightAsAmount $ committeeTotalWeight _bfFinalizers)
        ProtoFields.finalizationCommitteeHash .= toProto _bfFinalizerHash
      where
        weightAsAmount (VoterPower w) = Amount w

-- | The bakers and finalizers associated with the previous, current and next epochs (with respect
--  to a particular epoch). Note that the current epoch referred to here is typically the epoch
--  of the last finalized block, which is distinct from the current epoch as recorded in the
--  'RoundStatus' structure.
data EpochBakers = EpochBakers
    { -- | The bakers and finalizers for the previous epoch.
      --  (If the current epoch is 0, then this is the same as the bakers and finalizers for the
      --  current epoch.)
      _previousEpochBakers :: !BakersAndFinalizers,
      -- | The bakers and finalizers for the current epoch.
      _currentEpochBakers :: !BakersAndFinalizers,
      -- | The bakers and finalizers for the next epoch.
      _nextEpochBakers :: !BakersAndFinalizers,
      -- | The first epoch of the next payday. The set of bakers is fixed for an entire payday, and
      --  so the '_currentEpochBakers' apply for all epochs @e@ with
      --  @_currentEpoch <= e < _nextPayday@.
      _nextPayday :: !Epoch
    }

makeClassy ''EpochBakers

instance ToProto EpochBakers where
    type Output EpochBakers = Proto.EpochBakers
    toProto EpochBakers{..} = Proto.make $ do
        ProtoFields.previousEpochBakers .= toProto _previousEpochBakers
        unless (_currentEpochBakers == _previousEpochBakers) $
            ProtoFields.currentEpochBakers .= toProto _currentEpochBakers
        unless (_nextEpochBakers == _currentEpochBakers) $
            ProtoFields.nextEpochBakers .= toProto _nextEpochBakers
        ProtoFields.nextPayday .= toProto _nextPayday

-- | Quorum messages collected for a round.
data QuorumMessages = QuorumMessages
    { -- | Map of baker ids to signature messages.
      _smBakerIdToQuorumMessage :: !(Map.Map BakerId QuorumMessage),
      -- | Accumulated weights and the aggregated signature for the blocks signed off by quorum signature message.
      --  The 'VoterPower' here is in relation to the 'Epoch' of the block being finalized.
      _smBlockToWeightsAndSignatures :: !(Map.Map BlockHash (VoterPower, QuorumSignature, FinalizerSet))
    }
    deriving (Eq, Show)

makeLenses ''QuorumMessages

-- | Construct an empty 'QuorumMessages'
emptyQuorumMessages :: QuorumMessages
emptyQuorumMessages = QuorumMessages Map.empty Map.empty

-- | A collection of timeout messages with respect to the current round and for most two consecutive epochs.
--  INVARIANTS:
--   * 'tmFirstEpochTimeouts' is never empty.
--   * All timeout messages in 'tmFirstEpochTimeouts' have epoch 'tmFirstEpoch' and finalizer index
--     matching the key in the map.
--   * All timeout messages in 'tmSecondEpochTimeouts' have epoch @tmFirstEpoch + 1@ and finalizer
--     index matching the key in the map.
--  IMPORTANT NOTE: A timeout message "has epoch @e@" here if
--  @qcEpoch (tmQuorumCertificate tmBody) == e@. That is, it is independent of @tmEpoch@.
data TimeoutMessages
    = -- | Timeout messages for one epoch or two consecutive epochs.
      TimeoutMessages
      { -- | First epoch for which we have timeout messages.
        tmFirstEpoch :: Epoch,
        -- | Timeout messages for epoch 'tmFirstEpoch' indexed by the 'FinalizerIndex'.
        tmFirstEpochTimeouts :: !(Map.Map FinalizerIndex TimeoutMessage),
        -- | Timeout messages for epoch @tmFirstEpoch + 1@ indexed by the 'FinalizerIndex'.
        tmSecondEpochTimeouts :: !(Map.Map FinalizerIndex TimeoutMessage)
      }
    deriving (Eq, Show)

instance ToProto TimeoutMessages where
    type Output TimeoutMessages = Proto.TimeoutMessages
    toProto TimeoutMessages{..} = Proto.make $ do
        ProtoFields.firstEpoch .= toProto tmFirstEpoch
        ProtoFields.firstEpochTimeouts
            .= fmap toProto (toList tmFirstEpochTimeouts)
        ProtoFields.secondEpochTimeouts
            .= fmap toProto (toList tmSecondEpochTimeouts)
