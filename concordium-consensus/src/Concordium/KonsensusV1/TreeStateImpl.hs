{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.TreeStateImpl where

import Control.Monad.Reader
import Data.IORef
import Data.Kind (Type)

import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Prio.Min as MPQ

import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.TreeState (DeadCache, emptyDeadCache)
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.TransactionTable
import Concordium.KonsensusV1.TreeState
import Concordium.KonsensusV1.Types
import Concordium.Types
import Lens.Micro.Platform

import qualified Concordium.GlobalState.Persistent.BlockState as PBS

-- |The block table.
data BlockTable bp bs = BlockTable
    { _deadCache :: !DeadCache,
      _liveMap :: HM.HashMap BlockHash (BlockStatus bp bs)
    }

makeLenses ''BlockTable

-- |Create the empty block table.
emptyBlockTable :: BlockTable bp bs
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- |A pointer to a block that has been executed
-- and the resulting 'PBS.HashedPersistentBlockStat'.
data BlockPointer (pv :: ProtocolVersion) = BlockPointer
    { -- |Metadata for the block.
      _bpInfo :: !BasicBlockPointerData,
      -- |Pointer to the parent block.
      _bpParent :: !(BlockPointer pv),
      -- |The signed block.
      _bpBlock :: !SignedBlock,
      -- |The resulting state of executing the block.
      _bpState :: !(PBS.HashedPersistentBlockState pv)
    }

-- |Data required to support 'TreeState'.
data SkovData (pv :: ProtocolVersion) = SkovData
    { -- |The 'QuorumSignatureMessage's for the current round.
      _currentQuouromSignatureMessages :: !(SignatureMessages QuorumSignatureMessage),
      -- |The 'TimeoutSignatureMessages's for the current round.
      _currentTimeoutSignatureMessages :: !(SignatureMessages TimeoutSignatureMessage),
      -- |Transactions.
      _transactionTable :: !TransactionTable,
      -- |Table of pending transactions i.e. transactions that has not yet become part
      -- of a block.
      _pendingTransactions :: !PendingTransactionTable,
      -- |The current focus block.
      -- The focus block is the block with the largest height included in the tree.
      _focusBlock :: !(BlockPointer pv),
      -- |Runtime parameters.
      _runtimeParameters :: !RuntimeParameters,
      -- |Blocks which have been included in the tree.
      _blockTable :: !(BlockTable (BlockPointer pv) SignedBlock),
      -- |Pending blocks i.e. blocks that have not yet been included in the tree.
      -- The entries of the pending blocks are keyed by the 'BlockHash' of their parent block.
      _pendingBlocks :: !(HM.HashMap BlockHash [SignedBlock]),
      -- |A priority search queue on the (pending block hash, parent of pending block hash) tuple.
      -- The queue in particular supports extracting the minimal 'Round'.
      _pendingBlocksQueue :: !(MPQ.MinPQueue Round (BlockHash, BlockHash)),
      -- |Pointer to the last finalized block.
      _lastFinalized :: !(BlockPointer pv),
      -- |The current consensus statistics.
      _statistics :: !ConsensusStatistics
    }

makeLenses ''SkovData

-- |A 'SkovData pv' wrapped in an 'IORef', where @pv@ is the
-- current 'ProtocolVersion'
newtype SkovState (pv :: ProtocolVersion) = SkovState (IORef (SkovData pv))

-- |Create the 'HasSkovState' @HasSkovState pv@ constraint.
makeClassy ''SkovState

-- |'TreeStateWrapper' for running an action @a@ on the 'MonadTreeState'.
newtype TreeStateWrapper (pv :: ProtocolVersion) (m :: Type -> Type) (a :: Type) = TreeStateWrapper {runTreeStateWrapper :: m a}
    deriving newtype (Functor, Applicative, Monad)

-- |'MonadReader' instance for 'TreeStateWrapper'.
deriving instance MonadReader r m => MonadReader r (TreeStateWrapper pv m)

instance (Monad m, IsProtocolVersion pv, HasSkovState r pv) => MonadTreeState (TreeStateWrapper pv m)
