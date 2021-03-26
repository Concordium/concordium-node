module Concordium.Afgjort.Monad where

import qualified Data.Sequence as Seq

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Types

import Concordium.Afgjort.Finalize.Types
import Concordium.Skov.Monad (UpdateResult)

class FinalizationOutputMonad m where
    broadcastFinalizationMessage :: FinalizationMessage -> m ()
    broadcastFinalizationMessage = broadcastFinalizationPseudoMessage . FPMMessage
    {-# INLINE broadcastFinalizationMessage #-}
    broadcastFinalizationPseudoMessage :: FinalizationPseudoMessage -> m ()


class (Monad m) => FinalizationMonad m where
    -- |Notify finalization that a new block has been added to
    -- the block tree.
    finalizationBlockArrival :: BlockPointerType m -> m ()
    -- |Notify finalization that a new block has become final.
    -- This should never be called with the genesis block. The
    -- block that is passed in must be the block that is finalized
    -- by the finalization record. This should only be called once
    -- per finalization, and finalizations must occur in order.
    finalizationBlockFinal :: FinalizationRecord -> BlockPointerType m -> m ()
    -- |Notify finalization that a finalization message has been received.
    -- The result can be one of the following:
    --
    -- * 'ResultStale': the message is for an old finalization round.
    -- * 'ResultUnverifiable': the message could not be verified because it pertains to a future round.
    -- * 'ResultDuplicate': the message was received before.
    -- * 'ResultPendingFinalization': the message is for a future round, but we're not throwing it away.
    -- * 'ResultInvalid': the message is not valid.
    -- * 'ResultSuccess': the message was received and not found invalid.
    -- * 'ResultIncorrectFinalizationSession': the finalization session of the message is not what is expected.
    -- * 'ResultPendingBlock': the message suggests we are missing blocks; attempt catch up.
    -- * 'ResultConsensusShutDown': the consensus has been shut down (due to an update).
    finalizationReceiveMessage :: FinalizationPseudoMessage -> m UpdateResult
    -- |Handle receipt of a finalization record.
    --
    -- If consensus has already shut down (due to a protocol update), returns 'ResultConsensusShutDown'.
    --
    -- If the record is for a finalization index that is settled (i.e. the finalization
    -- record appears in a finalized block) then this returns 'ResultStale'.
    --
    -- If the record is for a finalization index where a valid finalization record is already
    -- known, then one of the following applies:
    --
    --   * If the record is invalid, returns 'ResultInvalid'.
    --   * If the record is valid and contains new signatures, stores the record and returns 'ResultSuccess'.
    --   * If @validateDuplicate@ is not set or the record is valid, returns 'ResultDuplicate'.
    --
    -- When more than one case could apply, it is unspecified which is chosen. It is intended that
    -- 'ResultSuccess' should be used wherever possible, but 'ResultDuplicate' can be returned in any
    -- case.
    --
    -- If the record is for the next finalization index:
    --
    --   * If the record is valid and for a known block, that block is finalized and 'ResultSuccess' returned.
    --   * If the record is invalid, 'ResultInvalid' is returned.
    --   * If the block is unknown, then 'ResultUnverifiable' is returned.
    --
    -- If the record is for a future finalization index (that is not next), 'ResultUnverifiable' is returned
    -- and the record is discarded.
    finalizationReceiveRecord ::
        Bool -- ^ @validateDuplicate@
        -> FinalizationRecord
         -> m UpdateResult
    -- |Get the (best available) finalization record for a given finalization index
    -- that is not settled.
    finalizationUnsettledRecordAt :: FinalizationIndex -> m (Maybe (FinalizationSessionId, FinalizationCommittee, FinalizationRecord))
    -- |Return the finalization records for the unsettled finalized blocks with
    -- finalization index greater than the specified value.
    finalizationUnsettledRecords :: FinalizationIndex -> m (Seq.Seq FinalizationRecord)
    -- |Return @True@ if we are a member of the current finalization committee.
    isFinalizationCommitteeMember :: m Bool