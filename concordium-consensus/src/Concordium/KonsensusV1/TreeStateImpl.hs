{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Concordium.KonsensusV1.TreeStateImpl where

import Data.IORef
import Control.Monad.Reader

import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Prio.Min as MPQ

import Lens.Micro.Platform
import Concordium.KonsensusV1.TreeState
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.GlobalState.Persistent.TreeState (DeadCache, emptyDeadCache)
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.TransactionTable

import qualified Concordium.GlobalState.Persistent.BlockState as PBS


-- |The block table.
data BlockTable bp bs = BlockTable {
    _deadCache :: !DeadCache,
    _liveMap :: HM.HashMap BlockHash (BlockStatus bp bs)
}
makeLenses ''BlockTable

-- |Create the empty block table.
emptyBlockTable :: BlockTable bp bs
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- |A pointer to a block that has been executed.
-- This serves as a pointer for the raw block and some metadata,
-- but it also for the resulting state of it (the block) being executed.
-- FIXME this is wrong..
type BlockPointerAndState pv = (PersistentBlockPointer pv (PBS.HashedPersistentBlockState pv))

-- |Data required to support 'TreeState'.
data KonsensusData (pv :: ProtocolVersion) = KonsensusData {
    -- |The 'QuorumSignatureMessage's for the current round.
    _currentQuouromSignatureMessages :: SignatureMessages QuorumSignatureMessage,
    -- |The 'TimeoutSignatureMessages's for the current round.
    _currentTimeoutSignatureMessages :: SignatureMessages TimeoutSignatureMessage,
    -- |Transactions.
    _transactionTable :: !TransactionTable,
    -- |Table of pending transactions i.e. transactions that has not yet become part
    -- of a block.
    _pendingTransactions :: !PendingTransactionTable,
    -- |The current focus block.
    -- The focus block is the block with the largest height included in the tree.
    _focusBlock :: !(BlockPointerAndState pv),
    -- |Runtime parameters.
    _runtimeParameters :: !RuntimeParameters,
    -- |Blocks which have been included in the tree.
    _blockTable :: !(BlockTable (BlockPointerAndState pv) SignedBlock),
    -- |Pending blocks i.e. blocks that have not yet been included in the tree.
    -- The entries of the pending blocks are keyed by the 'BlockHash' of their parent block.
    _pendingBlocks :: !(HM.HashMap BlockHash [SignedBlock]),
    -- |A priority search queue on the (pending block hash, parent of pending block hash) tuple.
    -- The queue in particular supports extracting the minimal 'Round'.
    _pendingBlocksQueue :: !(MPQ.MinPQueue Round (BlockHash, BlockHash)),
    -- |Pointer to the last finalized block.
    _lastFinalized :: !(BlockPointerAndState pv),
    -- |The current consensus statistics.
    _statistics :: !ConsensusStatistics
    
}
makeLenses ''KonsensusData

-- |A 'KonsensusData pv' wrapped in an 'IORef', where @pv@ is the
-- current 'ProtocolVersion'
newtype KonsensState pv = KonsensState (IORef (KonsensusData pv))

-- |Create the 'HasKonsensusState' constraint and a lens.
makeClassy ''KonsensState

