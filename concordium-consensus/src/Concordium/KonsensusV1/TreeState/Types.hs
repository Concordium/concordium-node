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

import Concordium.GlobalState.Parameters
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

-- |Result of adding a 'VerifiedTransaction' to the transaction store.
data AddBlockItemResult
    = -- |The transaction was added to the transaction store.
      Added
    | -- |The transaction was not added as it is
      -- already contained in the transaction store.
      Duplicate
    | -- |The transaction was not added as it yielded
      -- an old nonce for the sender for the transaction.
      -- I.e. the 'BlockItem' consisted of a account nonce that was
      -- less than the current finalized account nonce for the account.
      OldNonce

-- |A pointer to a block that has been executed
-- and the resulting 'PBS.HashedPersistentBlockStat'.
data BlockPointer (pv :: ProtocolVersion) = BlockPointer
    { -- |Metadata for the block.
      _bpInfo :: !BlockMetadata,
      -- |The signed block.
      _bpBlock :: !(Block pv),
      -- |The resulting state of executing the block.
      _bpState :: !(PBS.HashedPersistentBlockState pv)
    }

instance HashableTo BlockHash (BlockPointer pv) where
    getHash BlockPointer{..} = getHash _bpBlock

-- |Constraint that the protocol version @pv@ is associated with the version 1 consensus.
type IsConsensusV1 (pv :: ProtocolVersion) =
    ConsensusParametersVersionFor (ChainParametersVersionFor pv) ~ 'ConsensusParametersVersion1

-- |Status of a transaction.
data TransactionStatus
    = -- |The transaction is either pending (i.e. not in a block) of committed (i.e. in a
      -- non-finalized block).
      Live !LiveTransactionStatus
    | -- |The transaction is in a finalized block.
      Finalized !FinalizedTransactionStatus

-- |The status of a block.
data BlockStatus pv
    = -- |The block is awaiting its parent to become part of chain.
      BlockPending !SignedBlock
    | -- |The block is alive i.e. head of chain.
      BlockAlive !(BlockPointer pv)
    | -- |The block is finalized.
      BlockFinalized !(BlockPointer pv)
    | -- |The block has been marked dead.
      BlockDead

instance Show (BlockStatus pv) where
    show (BlockPending _) = "Pending"
    show (BlockAlive _) = "Alive"
    show (BlockFinalized _) = "Finalized"
    show BlockDead = "Dead"

-- |Get the status of a block if it recent
-- otherwise if it is a predecessor of the last finalized block
-- get a witness on that i.e. 'OldFinalized'.
data RecentBlockStatus pv
    = -- |The block is recent i.e. it is either 'Alive',
      -- 'Pending' or the last finalized block.
      RecentBlock !(BlockStatus pv)
    | -- |The block is a predecessor of the last finalized block.
      OldFinalized
    | -- |The block is unknown.
      Unknown

-- |Metadata about a block that has been executed.
data BlockMetadata = BlockMetadata
    { bmHeight :: !BlockHeight,
      bmReceiveTime :: !UTCTime,
      bmArriveTime :: !UTCTime
    }

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
          rsLastSignedQuouromSignatureMessage = Nothing,
          rsLastSignedTimeoutSignatureMessage = Nothing,
          rsCurrentTimeout = baseTimeout,
          rsHighestQC = Nothing,
          rsLeadershipElectionNonce = leNonce,
          rsLatestEpochFinEntry = Nothing,
          rsPreviousRoundTC = Nothing
        }
