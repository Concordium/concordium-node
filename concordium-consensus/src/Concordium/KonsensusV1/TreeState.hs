{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Concordium.KonsensusV1.TreeState where

import Concordium.Types
import Concordium.Types.Parameters

import Concordium.KonsensusV1.Types

import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Types

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
    -- * Block operations

    --

    -- |Add a 'SignedBlock' to the block table and assign
    -- it status 'Pending' as it is awaiting its parent.
    -- Acquires a write lock.
    addPendingBlock :: SignedBlock -> m ()

    -- |Mark a pending block to be live.
    -- Set the status of the block to be 'Alive'.
    -- Note that this will also update the consensus statistics.
    -- Acquires a write lock.
    makeLiveBlock :: BlockHash -> m ()

    -- |Mark a pending block as dead
    -- Acquires a write lock.
    markPendingBlockDead :: BlockHash -> m ()

    -- |Mark a live block as dead.
    -- Mark it as dead.
    -- Drop the transaction results.
    -- Purge the block state assoicated.
    -- Acquires a write lock.
    markLiveBlockDead :: BlockHash -> m ()

    -- |Attach a 'QuorumCertificate' to the block.
    -- Precondition: The 'QuorumCertificate' must be pointing
    -- to the block provided.

    -- * Block queries

    --

    -- |Get a list of pending blocks of a block
    -- removing the children pending blocks from the pending blocks *something*
    -- Acquires a read lock.
    takePendingChildren :: m [SignedBlock]

    -- |Get the last finalized block.
    -- Acquires a read lock.
    getLastFinalized :: m SignedBlock

    -- |Get the block height of the last finalized block.
    -- Acquires a read lock.
    getLastFinalizedHeight :: m BlockHeight

    -- |Get the current 'BlockStatus' of a block.
    -- Note. If the block is older than the last finalized block,
    -- then this will incur a disk lookup.
    -- Acquires a read lock.
    getBlockStatus :: m (Maybe (BlockStatus (BlockPointerType m) SignedBlock))

    -- |Get the current 'RecentBlockStatus' of a block.
    -- If the block is older than the last finalized block then
    -- the block then only this fact is returned and not
    -- the block hash ,...
    -- Acquires a read lock.
    getRecentBlockStatus :: m (RecentBlockStatus () SignedBlock)

    -- * Pending transactions and focus block.

    --

    -- |Get the focus block.
    -- This might be the best block but if
    -- we're pruning a branch this will become the parent block
    -- Acquires a read lock.
    getFocusBlock :: m ()

    -- |Update the focus block
    -- If we're pruning a block then we must also update the transaction statuses
    -- of the ones comitted to a pending state.
    -- Acquires a write lock.
    setFocusBlock :: m ()

    -- NOTE. Are these actually required to be exposed?
    -- I.e. Should we expose the pending transactions at all?
    -- How about if we only exposed the get/set focus block above
    -- and then had another function dedicated for baking i.e.
    -- acquiring transactions for the block.
    -- See 'makeBlock' below.

    -- |Get the pending transactions
    -- I.e. transactions that have not yet been included in a block.
    -- Note. pending transactions are after the focus block has been executed.
    -- Acquires a read lock.
    getPendingTransactions :: m ()

    -- Alternative approach to expose the raw ptt, non finalized credential deployments,
    -- non finalized chain updates
    -- The alternative approach aims to solve 2 goals.
    -- 1. Less stuff exposed in the API.
    -- 2. Easier reuse of pointers to data structures behind
    -- the tree state abstraction such as pending transactions, non finalized
    -- credential deployments and non finalized chain updates.
    -- Less opportunity to retain multiple copies of the same thing in memory.

    -- |Make a block with given the inputs.
    -- This should be used by the baker making a block.
    -- The pending transactions, credential deployments and chain updates are given by this function.
    -- Acquires a write lock.
    makeBlock ::
        -- |Interface to the non finalized chain updates
        ( () -> -- Interface to the pending transaction table.
          () -> -- Interface to the non finalized credentials
          () -- Inteface to the non finalized chain updates
        ) ->
        -- |The other parameters the baker provides.
        () ->
        -- |The pending block to relay to peers and mark as alive.
        m ()

    -- |Given a block hash and .. provide
    -- the context required for executing the block.
    -- This should be used by a participant who received the block.
    -- This provides the context of the pending transactions,
    -- non finalized credential deployments and non finalized chain updates.
    getExecutableContext ::
        () -> -- the block hash and some more..
        ( () -> -- Interface to the pending transaction table.
          () -> -- Interface to the non finalized credentials.
          () -- Interface to the non finalized chain updates.
        ) ->
        m () -- some block execution result.

    -- End alternative approach

    -- |Set the pending transactions
    -- Acquires a write lock.
    setPendingTransactions :: m ()

    -- * Quorum- and Timeout Certificates

    --

    -- |Create a signature (i.e. a signature to be aggregated in
    -- a quorum certificate)
    -- Note. if one have already signed the current round then
    -- that signature and message is returned so
    -- one can relay it.
    -- Acquires a write lock.
    makeQuorumSignature :: m ()

    -- |Add quorum message for current round.
    -- If enough signatures have been received then this
    -- will create and store the quorum certificate.
    -- Also this will progress the round if the latter is the case.
    -- Acquires a write lock.
    addQuorumSignature :: m ()

    -- |Create a timeout message.
    -- Creates and signs a timeout message.
    -- The signed timeout message is returned.
    -- Note. if one have already signed the current round then
    -- that signature and message is returned.
    -- Acquires a write lock.
    makeTimeoutMessage :: m ()

    -- |Add a timeout message for the current round.
    -- If enough signatures have been received then this
    -- will create and store the timeout certificate.
    -- Also this will progress the round if the latter is the case.
    -- Acquires a write lock.
    addTimeoutMessage :: m ()

    -- |Get the current round and epoch.
    -- Acquires a read lock.
    getRound :: m Round

    -- * Transactions

    --

    -- |Add a pending transaction to the transaction table.
    -- Acquires a write lock.
    addPendingTransaction :: m ()

    -- |Lookup a transaction by its hash.
    -- Acquires a read lock.
    lookupTransaction :: m ()

    -- |Purge the transaction table.
    -- Note. Should this actually be exposed or can
    -- we choose an opportune time from within the tree state...
    -- Acquires a write lock.
    purgeTransactionTable :: m ()

    -- * Account transactions

    --

    -- |Get the next account nonce for an account.
    -- Returns a tuple consisting of the successor of the
    -- current account nonce and a boolean value indicating
    -- that there are no pending or comitted (but only finalized) transactions
    -- tied to this account.
    -- Acquires a read lock.
    getNextAccountNonce :: m (Int, Bool)

    -- * Chain updates

    --

    -- |Get the non finalized chain updates.
    -- Is this really necessary to expose so raw?
    -- Same thoughts as for the getPendingTransactions.
    -- Acquires a read lock.
    getNonFinalizedChainUpdates :: m ()

    -- * Credential deployments

    --

    -- |Get the non finalized credentials
    -- Is this really necessary to expose so raw?
    -- Same thoughts as for the getPendingTransactions.
    -- Acquires a read lock.
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
    -- Acquires a write lock.
    clearAfterProtocolUpdate :: m ()

    -- * Statistics and parameters

    --

    -- |Get consensus statistics.
    -- Note that the actual statistics are updated by 'makeLiveBlock'.
    -- Acquires a read lock.
    getConsensusStatistics :: m ()

    -- |Get the runtime parameters.
    -- Acquires a read lock.
    getRuntimeParameters :: m ()
