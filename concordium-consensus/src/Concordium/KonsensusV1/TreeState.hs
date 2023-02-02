{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Concordium.KonsensusV1.TreeState where

import qualified Data.Map.Strict as Map

import Concordium.Types
import Concordium.Types.Parameters
import Concordium.Types.Transactions
import Concordium.Types.Updates

import Concordium.KonsensusV1.Types

import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Types
import qualified Concordium.TransactionVerification as TVer

-- |Constraint for for ''ConsensusParametersVersion1' based on
-- the protocol version @pv@.
type IsConsensusV1 (pv :: ProtocolVersion) =
    ConsensusParametersVersionFor (ChainParametersVersionFor pv) ~ 'ConsensusParametersVersion1

-- |Tree state for 'ConsensusParametersVersion1'
-- Tree state operations are guarded by a RW lock.
class
    ( Monad m,
      IsConsensusV1 (MPV m)
    ) =>
    TreeStateMonad m
    where
    -- * Pending blocks

    --

    -- |Add a 'SignedBlock' to the block table and assign
    -- it status 'Pending' as it is awaiting its parent.
    addPendingBlock :: SignedBlock -> m ()

    -- |Mark a pending block to be live.
    -- Set the status of the block to be 'Alive'.
    -- Note that this will also update the consensus statistics.
    markPendingBlockLive :: BlockHash -> m ()

    -- |Mark a pending block as dead
    -- This will also expunge the contents from memory.
    markPendingBlockDead :: BlockHash -> m ()

    -- |Get a list of pending blocks of a block
    -- removing the children pending blocks from the pending blocks *something*
    takePendingChildren :: m [SignedBlock]

    -- * Live blocks

    -- |Mark a live block as dead.
    -- This also:
    --     * Drop the transaction results.
    --     * Drop the associated block state.
    markLiveBlockDead :: BlockHash -> m ()

    -- * Finalized blocks

    -- |Get the last finalized block.
    -- Acquires a read lock.
    getLastFinalized :: m SignedBlock

    -- |Get the block height of the last finalized block.
    -- Acquires a read lock.
    getLastFinalizedHeight :: m BlockHeight

    -- * Block statuses

    -- |Get the current 'BlockStatus' of a block.
    -- Note. If the block is older than the last finalized block,
    -- then this will incur a disk lookup.
    getBlockStatus :: m (Maybe (BlockStatus (BlockPointerType m) SignedBlock))

    -- |Get the current 'RecentBlockStatus' of a block.
    -- If the block is older than the last finalized block then
    -- the block then only this fact is returned and not
    -- the block hash ,...
    getRecentBlockStatus :: m (RecentBlockStatus () SignedBlock)

    -- * Pending transactions and focus block.

    --

    -- |Get the focus block.
    -- This is probably the best block, but if
    -- we're pruning a branch this will become the parent block
    getFocusBlock :: m ()

    -- |Update the focus block
    -- If we're pruning a block then we must also update the transaction statuses
    -- of the ones comitted to a pending state.
    setFocusBlock :: m ()

    -- |Get the pending transactions
    -- I.e. transactions that have not yet been committed to a block.
    -- Note. pending transactions are after the focus block has been executed.
    getPendingTransactions :: m ()

    -- |Set the pending transactions
    setPendingTransactions :: m ()

    -- |Add a pending transaction to the transaction table.
    addPendingTransaction :: m ()

    -- * Quorum- and Timeout Certificates

    --

    -- |Gets the quorum signatures for the current round.
    getQuorumSignatureMessages :: m [QuorumSignatureMessage]

    -- |Add quorum message for the current round.
    addQuorumSignature :: QuorumSignatureMessage -> m ()

    -- |Create a timeout message.
    -- Creates and signs a timeout message.
    -- The signed timeout message is returned.
    -- Note. if one have already signed the current round then
    -- that signature and message is returned.
    getTimeoutMessages :: m [TimeoutSignatureMessage]

    -- |Add a timeout message for the current round.
    -- If enough signatures have been received then this
    -- will create and store the timeout certificate.
    -- Also this will progress the round if the latter is the case.
    addTimeoutMessage :: TimeoutSignatureMessage -> m ()

    -- |Get the current 'Epoch' and 'Round'
    getRound :: m (Epoch, Round)

    -- * Transactions

    --

    -- |Lookup a transaction by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe undefined) -- implement some transaction status.

    -- |Purge the transaction table.
    -- Expunge transactions which are marked
    -- as dead from the transaction table.
    purgeTransactionTable :: m ()

    -- * Account transactions

    --

    -- |Get the next account nonce for an account.
    -- Returns a tuple consisting of the successor of the
    -- current account nonce and a boolean value indicating
    -- that there are no pending or comitted (but only finalized) transactions
    -- tied to this account.
    getNextAccountNonce :: m (Nonce, Bool)

    -- * Chain updates

    --

    -- |Get the non finalized chain updates.
    -- This returns a map from update sequence numbers to the
    -- the corresponding chain updates groups.
    -- The chain update groups are ordered by increasing
    -- sequence number.
    getNonFinalizedChainUpdates :: UpdateType -> UpdateSequenceNumber -> m [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]

    -- * Credential deployments

    --

    -- |Get the non finalized credentials
    -- Is this really necessary to expose so raw?
    -- Same thoughts as for the getPendingTransactions.
    getNonFinalizedCredentials :: m ()

    -- * Protocol update

    --

    -- |Clear resources tied to the
    -- old protocol.
    --
    -- * Clear all non finalized blocks from the block table.
    -- * Clear pending blocks
    -- * Rollback all committed transactions,
    -- i.e. they are now in a 'Received' state.
    clearAfterProtocolUpdate :: m ()

    -- * Statistics and parameters

    --

    -- |Get consensus statistics.
    -- Note that the actual statistics are updated by 'markPendingBlockLive'.
    getConsensusStatistics :: m ()

    -- |Get the runtime parameters.
    getRuntimeParameters :: m ()
