{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Concordium.KonsensusV1.TreeState where

import Concordium.Types.Parameters
import Concordium.Types.ProtocolVersion

import Concordium.KonsensusV1.Types

import Concordium.GlobalState.Types
import Concordium.GlobalState.TreeState 


-- |Constraint for for ''ConsensusParametersVersion1' based on
-- the protocol version @pv@.
type IsConsensusV1 (pv :: ProtocolVersion) =
    ConsensusParametersVersionFor (ChainParametersVersionFor pv) ~ 'ConsensusParametersVersion1

-- |Tree state for 'ConsensusParametersVersion1'
class (Monad m,
       IsConsensusV1 (MPV m)
      ) => TreeStateMonad m where

  -- * Block operations
  --
  -- |Add pending block
  -- Adds the block to the block table and
  -- give it status 'Pending'.
  addPendingBlock :: m ()
  -- |Promote a pending block to be live.
  -- Set the status of the block to be 'Alive'.
  makeLiveBlock :: m ()
  -- |Mark a pending block as dead
  markPendingBlockDead :: m ()
    -- |Mark a live block as dead.
    -- Mark it as dead.
    -- Drop the transaction results.
    -- Purge the block state assoicated.
  markLiveBlockDead :: m ()
  -- |Attach a 'QuorumCertificate' to the block.
  -- Precondition: The 'QuorumCertificate' must be pointing
  -- to the block provided.


  -- * Block queries
  --
  -- |Get the last finalized block.
  getLastFinalized :: m ()
  -- |Get the current 'BlockStatus' of a block.
  -- Note. If the block is older than the last finalized block,
  -- then this will incur a disk lookup.
  getBlockStatus :: m (Maybe (BlockStatus () ()))  
  -- |Get the current 'RecentBlockStatus' of a block.
  -- If the block is older than the last finalized block then
  -- the block then only this fact is returned and not
  -- the block hash ,...
  getRecentBlockStatus :: m (RecentBlockStatus () ())

  -- *Quorum- and Timeout Certificates

  -- |Add a quorum certificate and
  -- if possible mark a predecessor as finalized.
  addQuorumCertificate :: m ()
  -- |Add a timeout certificate
  addTimeoutCertificate :: m ()

  -- *Transactions
  --
  -- |Add a transaction to the transaction table.
  addTransaction :: m ()
  
  
