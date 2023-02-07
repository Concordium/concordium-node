{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Concordium.KonsensusV1.TreeState where

import qualified Data.Map.Strict as Map
import Data.Time

import Concordium.GlobalState.Statistics
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Types
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types
import Concordium.Types.Parameters
import Concordium.Types.Transactions
import Concordium.Types.Updates

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

-- |Constraint for for ''ConsensusParametersVersion1' based on
-- the protocol version @pv@.
type IsConsensusV1 (pv :: ProtocolVersion) =
    ConsensusParametersVersionFor (ChainParametersVersionFor pv) ~ 'ConsensusParametersVersion1

-- |Tree state for 'ConsensusParametersVersion1'
-- A tree state instance should provide storage for the following:
-- Memory storage:
--     * Pending blocks store
--       Blocks which have not yet become part of the chain must be stored.
--     * Transaction store for transactions being part of a
--       a pending or alive block.
--       The pending transactions must take into account these types of transactions:
--         * Account transactions
--         * Chain updates
--         * Credential deployments
--       Invariant: The transaction store must only contain transactions that
--       have been verified.
--     * The quorum messages for the _current_ round
--     * The timeout messages for the _current_ round
-- Disk storage:
--     * Latest finalization entry
--       The latest finalization entry is required for catchup when a node
--       consensus instance has crashed.
--     * Current round, epoch and (latest quorum message signed || latest timeout message signed)
--       In case of restarting a consensus instance one must be
--       be able to starting
--     * Finalized blocks store
--       It should be possible to always lookup old finalized blocks.
--     * Finalized transactions store
--       It should be possible to always lookup old finalized transactions.
class
    ( Monad m,
      IsConsensusV1 (MPV m)
    ) =>
    MonadTreeState m
    where
    -- * Pending blocks

    --

    -- |Add a 'SignedBlock' to the block table and assign
    -- it status 'Pending' as it is awaiting its parent.
    -- The transactions of the block are also added to the transaction table.
    -- Note. This will also update the consensus statistics.
    addPendingBlock ::
        -- |The signed block to add to the pending blocks.
        SignedBlock ->
        m ()

    -- |Mark a pending block to be live.
    -- Set the status of the block to be 'Alive'.
    -- Note. This will also update the consensus statistics.
    markPendingBlockLive ::
        -- |The signed block to make live.
        SignedBlock ->
        -- |The parent block pointer
        BlockPointerType m ->
        -- |The current time
        UTCTime ->
        -- |The resulting block pointer
        m (BlockPointerType m)

    -- |Get a list of pending blocks of a block given
    -- the block pointer.
    -- This will remove the pending blocks from the pending table.
    takePendingChildren ::
        -- |The 'BlockHash' of the block
        -- to request child blocks for.
        BlockHash ->
        -- |The children blocks.
        -- When there is no branching
        -- then this will be a singleton.
        m [SignedBlock]

    -- |Marks a block as dead.
    -- If the block is 'Pending then this expunges the
    -- block from memory.
    -- If the block is 'Alive' then this must also:
    --     * Expunge the block from memory.
    --     * Drop the transaction results.
    --     * Drop the associated block state.
    markBlockDead ::
        -- |The 'BlockHash' of the block to mark as dead.
        BlockHash ->
        m ()

    -- * Finalized blocks

    -- |Get the last finalized block.
    getLastFinalized :: m SignedBlock

    -- |Get the block height of the last finalized block.
    getLastFinalizedHeight :: m BlockHeight

    -- * Block statuses

    -- |Get the 'BlockStatus' of a block.
    -- Note. If the block is older than the last finalized block,
    -- then this will incur a disk lookup.
    getBlockStatus ::
        -- |The 'BlockHash' of the block to request
        -- the 'BlockStatus' from.
        BlockHash ->
        -- |Returns 'Just BlockStatus' if the provided
        -- 'BlockHash' matches a block in the tree.
        -- Returns 'Nothing' if no block could be found.
        m (Maybe (BlockStatus (BlockPointerType m) SignedBlock))

    -- |Get the 'RecentBlockStatus' of a block.
    -- One should use this instead of 'getBlockStatus' if
    -- one does not require the actual contents and resulting state related
    -- to the block in case the block is a predecessor of the last finalized block.
    getRecentBlockStatus ::
        -- |The 'BlockHash' of the block to request
        -- the 'BlockStatus' from.
        BlockHash ->
        -- |Returns 'Just RecentBlockStatus' if the provided
        -- 'BlockHash' matches a block in the tree.
        -- Returns 'Nothing' if no block could be found.
        m (RecentBlockStatus (BlockPointerType m) SignedBlock)

    -- * Pending transactions and focus block.

    --

    -- |Get the focus block.
    -- This is probably the best block, but if
    -- we're pruning a branch this will become the parent block
    getFocusBlock :: m (BlockPointerType m)

    -- |Update the focus block
    -- If we're pruning a block then we must also update the transaction statuses
    -- of the ones comitted to a pending state.
    setFocusBlock ::
        -- |The pointer to the block that
        -- should become the "focus block".
        BlockPointerType m ->
        m ()

    -- |Get the pending transactions
    -- I.e. transactions that have not yet been committed to a block.
    -- Note. pending transactions are after the focus block has been executed.
    getPendingTransactions :: m PendingTransactionTable

    -- |Set the pending transactions
    setPendingTransactions :: PendingTransactionTable -> m ()

    -- * Quorum- and Timeout Certificates

    --

    -- |Gets the quorum signatures for the current round.
    getQuorumSignatureMessages :: m (SignatureMessages QuorumSignatureMessage)

    -- |Sets the quorum signature messages for the current round.
    setQuorumSignatureMessages :: SignatureMessages QuorumSignatureMessage -> m ()

    -- |Get the timeout messages for the current round.
    getTimeoutMessages :: m (SignatureMessages TimeoutSignatureMessage)

    -- |Sets the timeout messages for the current round.
    setTimeoutMessage :: SignatureMessages TimeoutSignatureMessage -> m ()

    -- * Round status.

    -- The 'RoundStatus' holds information about the current 'Round',
    -- but also enough information for progressing to the next round.

    --

    -- |Get the current 'RoundStatus'.
    getRoundStatus :: m RoundStatus

    -- |Set the current 'RoundStatus'.
    setRoundStatus :: RoundStatus -> m ()

    -- |

    -- * Transactions

    --

    -- |Add a verified transaction to the transaction table.
    addTransaction :: VerifiedBlockItem -> m AddBlockItemResult

    -- |Commit a batch of 'VerifiedBlockItem's.
    -- This should be used for commiting the transactions of a block received.
    commitTransactions :: [VerifiedBlockItem] -> m AddBlockItemResult

    -- |Lookup a transaction by its hash.
    lookupTransaction ::
        -- |Hash of the transaction to lookup.
        TransactionHash ->
        -- |The resulting transaction status.
        m (Maybe TransactionStatus)

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
    getNonFinalizedChainUpdates ::
        -- |The 'UpdateType' to retrieve.
        UpdateType ->
        -- |The starting sequence number.
        UpdateSequenceNumber ->
        -- |The resulting list of
        m [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]

    -- * Credential deployments

    --

    -- |Get a non finalized credential by its 'TransactionHash'
    -- This returns 'Nothing' in the case that the credential has already been finalized.
    getNonFinalizedCredential :: TransactionHash -> m (Maybe (CredentialDeploymentWithMeta, TVer.VerificationResult))

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
    getConsensusStatistics :: m ConsensusStatistics

    -- |Get the runtime parameters.
    getRuntimeParameters :: m ()

-- |The status of a block.
data BlockStatus bp sb
    = -- |The block is awaiting its parent to become part of chain.
      BlockPending sb
    | -- |The block is alive i.e. head of chain.
      BlockAlive !bp
    | -- |The block is finalized.
      -- The first pointer is to the block itself
      -- while the latter is for the block that finalized the
      -- block.
      BlockFinalized !bp
    | -- |The block has been marked dead.
      BlockDead
    deriving (Eq)

instance Show (BlockStatus bp sb) where
    show (BlockPending _) = "Pending"
    show (BlockAlive _) = "Alive"
    show (BlockFinalized _) = "Finalized"
    show BlockDead = "Dead"

-- |Get the status of a block if it recent
-- otherwise if it is a predecessor of the last finalized block
-- get a witness on that i.e. 'OldFinalized'.
data RecentBlockStatus bp sb
    = -- |The block is recent i.e. it is either 'Alive',
      -- 'Pending' or the last finalized block.
      RecentBlock !(BlockStatus bp sb)
    | -- |The block is a predecessor of the last finalized block.
      OldFinalized
    | -- |The block is unknown.
      Unknown
