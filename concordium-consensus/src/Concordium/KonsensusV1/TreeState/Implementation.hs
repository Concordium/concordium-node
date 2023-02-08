{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.TreeState.Implementation where

import Control.Monad.Reader
import Data.IORef
import Data.Kind (Type)

import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Prio.Min as MPQ

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.TreeState (DeadCache, emptyDeadCache, insertDeadCache, memberDeadCache)
import qualified Concordium.GlobalState.Statistics as Stats
import Concordium.GlobalState.TransactionTable
import Concordium.KonsensusV1.TreeState
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.HashableTo
import Lens.Micro.Platform

-- |Status of a block that is held in memory i.e.
-- the block is either pending or alive.
-- Parameterized by the 'BlockPointer' and the 'SignedBlock'.
data InMemoryBlockStatus bp sb
    = -- |The block is awaiting its parent to become part of chain.
      MemBlockPending !sb
    | -- |The block is alive i.e. head of chain.
      MemBlockAlive !bp
    deriving (Eq)

-- |The block table yields blocks that are
-- either alive or pending.
-- Furthermore it holds a fixed size cache of hashes
-- blocks marked as dead.
data BlockTable bp bs = BlockTable
    { _deadCache :: !DeadCache,
      _liveMap :: HM.HashMap BlockHash (InMemoryBlockStatus bp bs)
    }

makeLenses ''BlockTable

-- |Create the empty block table.
emptyBlockTable :: BlockTable bp bs
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- |Data required to support 'TreeState'.
data SkovData (pv :: ProtocolVersion) = SkovData
    { -- |The 'QuorumSignatureMessage's for the current round.
      _currentQuouromSignatureMessages :: !(SignatureMessages QuorumSignatureMessage),
      -- |The 'TimeoutSignatureMessages's for the current round.
      _currentTimeoutSignatureMessages :: !(SignatureMessages TimeoutSignatureMessage),
      -- |The 'RoundStatus' for the current 'Round'.
      _roundStatus :: !RoundStatus,
      -- |Transactions.
      _transactionTable :: !TransactionTable,
      -- |The purge counter for the 'TransactionTable'
      _transactionTablePurgeCounter :: !Int,
      -- |Table of pending transactions i.e. transactions that has not yet become part
      -- of a block.
      _pendingTransactions :: !PendingTransactionTable,
      -- |The current focus block.
      -- The focus block is the block with the largest height included in the tree.
      _focusBlock :: !(BlockPointer pv),
      -- |Runtime parameters.
      _runtimeParameters :: !RuntimeParameters,
      -- |Blocks which have been included in the tree or marked as dead.
      _blockTable :: !(BlockTable (BlockPointer pv) SignedBlock),
      -- |Pending blocks i.e. blocks that have not yet been included in the tree.
      -- The entries of the pending blocks are keyed by the 'BlockHash' of their parent block.
      _pendingBlocksTable :: !(HM.HashMap BlockHash [SignedBlock]),
      -- |A priority search queue on the (pending block hash, parent of pending block hash) tuple.
      -- The queue in particular supports extracting the minimal 'Round'.
      _pendingBlocksQueue :: !(MPQ.MinPQueue Round (BlockHash, BlockHash)),
      -- |Pointer to the last finalized block.
      _lastFinalized :: !(BlockPointer pv),
      -- |The current consensus statistics.
      _statistics :: !Stats.ConsensusStatistics
    }

makeLenses ''SkovData

-- |A 'SkovData pv' wrapped in an 'IORef', where @pv@ is the
-- current 'ProtocolVersion'
newtype SkovState (pv :: ProtocolVersion) = SkovState (IORef (SkovData pv))

-- |Create the 'HasSkovState' @HasSkovState pv@ constraint.
makeClassy ''SkovState

-- |'TreeStateWrapper' for implementing 'MonadTreeState'.
newtype TreeStateWrapper (pv :: ProtocolVersion) (m :: Type -> Type) (a :: Type) = TreeStateWrapper {runTreeStateWrapper :: m a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)

-- |There must be an instance for 'LowLevel.MonadTreeStateStore TreeStateWrapper' in the transformer stack somewhere.
deriving instance (LowLevel.MonadTreeStateStore m, MPV m ~ pv) => LowLevel.MonadTreeStateStore (TreeStateWrapper pv m)

-- |'MonadReader' instance for 'TreeStateWrapper'.
deriving instance MonadReader r m => MonadReader r (TreeStateWrapper pv m)

-- |'MonadProtocolVersion' instance for 'TreeStateWrapper'.
instance IsProtocolVersion pv => MonadProtocolVersion (TreeStateWrapper pv m) where
    type MPV (TreeStateWrapper pv m) = pv

-- |Verify the transactions included in a 'SignedBlock'.
-- Return @Right [VerifiedBlockItem]@ if all transactions could be
-- successfully verified otherwise @Left ()@.
-- todo: Implement and decide what should left be..
verifyBlockItems :: SignedBlock -> Either () [VerifiedBlockItem]
verifyBlockItems sb = undefined
  where
    txs = (bbTransactions . sbBlock) sb

-- |Mark a block as dead.
-- This adds the 'BlockHash' to the 'DeadCache' and if present also
-- expunges it from the @liveMap@ of the 'BlockTable'.
markDead :: BlockHash -> BlockTable (BlockPointer pv) SignedBlock -> BlockTable (BlockPointer pv) SignedBlock
markDead blockHash BlockTable{..} =
    let deadCache' = insertDeadCache blockHash _deadCache
        liveMap' = HM.delete blockHash _liveMap
    in  BlockTable{_deadCache = deadCache', _liveMap = liveMap'}

-- |Commit verified transactions to the transaction table.
-- If a transaction is at least the 'nextNonce' in the 'PendingTransactionTable' then
-- it should be added otherwise not.
-- todo implement.
commitVerifiedTransactions :: [VerifiedBlockItem] -> TransactionTable -> PendingTransactionTable -> (TransactionTable, PendingTransactionTable)
commitVerifiedTransactions = undefined

-- TODO: Add a transaction verifier
instance forall m pv r. (MonadIO m, MonadReader r m, IsConsensusV1 pv, HasSkovState r (MPV m), r ~ SkovState pv, LowLevel.MonadTreeStateStore m, MPV m ~ pv) => MonadTreeState (TreeStateWrapper pv m) where
    addPendingBlock sb = do
        (SkovState ioref) <- ask
        SkovData{_blockTable = bt, ..} <- liftIO $ readIORef ioref
        -- Verify the transactions of the block.
        -- If all transactions can be successfully pre-verified then we add it,
        -- otherwise we reject the block and mark it as dead.
        case verifyBlockItems sb of
            Left _ -> do
                let blockTable' = markDead blockHash bt
                liftIO $ writeIORef ioref $! SkovData{_blockTable = blockTable', ..}
                return BlockDead
            Right verifiedBlockItems -> do
                let pendingBlocksQueue' = MPQ.insert theRound (blockHash, parentHash) $! _pendingBlocksQueue
                    pendingBlocksTable' = HM.adjust (sb :) parentHash _pendingBlocksTable
                    -- Commit the transactions and add them to the 'PendingTransactionTable' if they are eligible.
                    -- A transaction is eligible for entering the pending transactions if it's nonce is at least the 'nextNonce'.
                    (transactionTable', pendingTransactions') = commitVerifiedTransactions verifiedBlockItems _transactionTable _pendingTransactions
                liftIO $
                    writeIORef ioref $!
                        SkovData
                            { _pendingBlocksQueue = pendingBlocksQueue',
                              _pendingBlocksTable = pendingBlocksTable',
                              _transactionTable = transactionTable',
                              _pendingTransactions = pendingTransactions',
                              _blockTable = bt,
                              ..
                            }
                return $! BlockPending sb
      where
        blockHash = getHash sb
        bakedBlock = sbBlock sb
        theRound = bbRound bakedBlock
        parentHash = bhParent $! bbBlockHeader bakedBlock

    markPendingBlockLive = undefined
    takePendingChildren = undefined
    markBlockDead blockHash = do
        (SkovState ioref) <- ask
        SkovData{_blockTable = bt, ..} <- liftIO $ readIORef ioref
        let blockTable' = markDead blockHash bt
        liftIO $ writeIORef ioref $! SkovData{_blockTable = blockTable', ..}
        return ()

    getLastFinalized = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return $! _lastFinalized

    getBlockStatus blockHash = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        -- We check whether the block is the last finalized block.
        if (bmHash . _bpInfo) _lastFinalized == blockHash
            then return $! Just $! BlockFinalized _lastFinalized
            else -- We check if the block is the focus block

                if (bmHash . _bpInfo) _focusBlock == blockHash
                    then return $! Just $! BlockAlive _focusBlock
                    else -- Now we lookup in the livemap of the block table,
                    -- this will return if the block is pending or if it's alive on some other
                    -- branch.
                    -- If it is not present in the live store then we check whether the block
                    -- has been marked as dead or we finally try to look in the persistent block store.
                    case HM.lookup blockHash (_liveMap _blockTable) of
                        Just status ->
                            case status of
                                MemBlockPending sb -> return $! Just $! BlockPending sb
                                MemBlockAlive bp -> return $! Just $! BlockAlive bp
                        Nothing ->
                            -- Check if the block marked as dead before we look into the
                            -- persistent block store.
                            if memberDeadCache blockHash (_deadCache _blockTable)
                                then return $ Just BlockDead
                                else do
                                    -- Now we look in the LMDB to check if the block is stored.
                                    -- Only finalized blocks are persisted so we return a 'BlockFinalized' in case
                                    -- we find the block in the persistent block store.
                                    LowLevel.lookupBlock blockHash >>= \case
                                        Nothing -> return Nothing
                                        Just storedBlock -> return $! Just $! BlockFinalized $! mkBlockPointer storedBlock
      where
        -- Create a block pointer from a stored block.
        mkBlockPointer LowLevel.StoredBlock{..} =
            let
                _bpInfo = stbInfo
                _bpBlock = stbBlock
                _bpState = undefined -- todo
            in
                BlockPointer{..}

    getRecentBlockStatus = undefined
    getFocusBlock = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _focusBlock

    setFocusBlock focusBlock' = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        liftIO $ writeIORef ioref $! SkovData{_focusBlock = focusBlock', ..}
        return ()

    getPendingTransactions = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _pendingTransactions

    getQuorumSignatureMessages = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _currentQuouromSignatureMessages

    setQuorumSignatureMessages currentQuouromSignatureMessages' = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        liftIO $ writeIORef ioref $ SkovData{_currentQuouromSignatureMessages = currentQuouromSignatureMessages', ..}
        return ()
    getTimeoutMessages = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _currentTimeoutSignatureMessages

    setTimeoutMessage currentTimeoutSignatureMessages' = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        liftIO $ writeIORef ioref $ SkovData{_currentTimeoutSignatureMessages = currentTimeoutSignatureMessages', ..}
        return ()

    getRoundStatus = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _roundStatus
    setRoundStatus roundStatus' = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        liftIO $ writeIORef ioref $! SkovData{_roundStatus = roundStatus', ..}
        return ()

    addTransaction = undefined
    lookupTransaction = undefined
    purgeTransactionTable = undefined
    getNextAccountNonce = undefined
    getNonFinalizedChainUpdates = undefined
    getNonFinalizedCredential = undefined
    clearAfterProtocolUpdate = undefined
    getConsensusStatistics = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _statistics
    getRuntimeParameters = do
        (SkovState ioref) <- ask
        SkovData{..} <- liftIO $ readIORef ioref
        return _runtimeParameters
