{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a simple in-memory version of the low-level tree state.
--  This is intended for testing purposes, for instance, where the overhead of creating an LMDB
--  database is excessive.
module Concordium.KonsensusV1.TreeState.LowLevel.Memory where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.Persistent.BlobStore (MBSStore)
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- | A low-level tree state database. This manages the storage and indexing of blocks and
--  transactions, as well as recording persisted state of the consensus in the form of the latest
--  finalization entry and current round status.
data LowLevelDB store pv = LowLevelDB
    { -- | Index of finalized blocks by height.
      lldbFinalizedBlocks :: !(Map.Map BlockHeight BlockHash),
      -- | Table of certified blocks by hash.
      lldbBlocks :: !(HM.HashMap BlockHash (StoredBlock store pv)),
      -- | Table of finalized transactions by hash.
      lldbTransactions :: !(HM.HashMap TransactionHash FinalizedTransactionStatus),
      -- | The last finalization entry (if any).
      lldbLatestFinalizationEntry :: !(Maybe (FinalizationEntry pv)),
      -- | Table of quorum certificates for non-finalized blocks.
      lldbNonFinalizedQuorumCertificates :: !(Map.Map Round QuorumCertificate),
      -- | The current round status.
      lldbRoundStatus :: !PersistentRoundStatus
    }

-- | An initial 'LowLevelDB' with the supplied genesis block and round status, but otherwise with
--  no blocks, no transactions and no finalization entry.
--  The genesis block should have height 0; this is not checked.
initialLowLevelDB :: StoredBlock store pv -> PersistentRoundStatus -> LowLevelDB store pv
initialLowLevelDB genBlock roundStatus =
    LowLevelDB
        { lldbFinalizedBlocks = Map.singleton 0 (getHash genBlock),
          lldbBlocks = HM.singleton (getHash genBlock) genBlock,
          lldbTransactions = HM.empty,
          lldbLatestFinalizationEntry = Nothing,
          lldbNonFinalizedQuorumCertificates = Map.empty,
          lldbRoundStatus = roundStatus
        }

-- | Update a 'LowLevelDB' by adding the given block to 'lldbFinalizedBlocks' and all of its
--  transactions to 'lldbTransactions'. Note, this does not add the block to 'lldbBlocks'.
finalizeBlock :: LowLevelDB store pv -> StoredBlock store pv -> LowLevelDB store pv
finalizeBlock db@LowLevelDB{..} sb =
    db
        { lldbFinalizedBlocks = Map.insert height (getHash sb) lldbFinalizedBlocks,
          lldbTransactions = foldl' insertTx lldbTransactions (zip (blockTransactions sb) [0 ..])
        }
  where
    height = blockHeight sb
    insertTx txs (tx, ti) = HM.insert (getHash tx) (FinalizedTransactionStatus height ti) txs

-- | Helper functions for implementing 'writeFinalizedBlocks'.
doWriteFinalizedBlocks ::
    -- | Newly-finalized blocks in order.
    [StoredBlock store pv] ->
    -- | Finalization entry for the last of the finalized blocks.
    FinalizationEntry pv ->
    LowLevelDB store pv ->
    LowLevelDB store pv
doWriteFinalizedBlocks finBlocks finEntry =
    flip (foldl' finalizeBlock) finBlocks . processFinEntry
  where
    processFinEntry db@LowLevelDB{..} =
        db
            { lldbLatestFinalizationEntry = Just finEntry,
              lldbNonFinalizedQuorumCertificates = keepQCs,
              lldbBlocks =
                flip (foldl' (\m b -> HM.insert (getHash b) b m)) finBlocks
                    . flip (foldl' (\m qc -> HM.delete (qcBlock qc) m)) (Map.elems removeQCs)
                    $ lldbBlocks
            }
      where
        (removeQCs, keepQCs) =
            Map.split
                (qcRound (feFinalizedQuorumCertificate finEntry))
                lldbNonFinalizedQuorumCertificates

-- | Helper function for implementing 'writeCertifiedBlock'.
doWriteCertifiedBlock ::
    -- | Newly-certified block.
    StoredBlock store pv ->
    -- | QC on the certified block.
    QuorumCertificate ->
    LowLevelDB store pv ->
    LowLevelDB store pv
doWriteCertifiedBlock certBlock qc db@LowLevelDB{..} =
    db
        { lldbBlocks =
            HM.insert (getHash certBlock) certBlock lldbBlocks,
          lldbNonFinalizedQuorumCertificates =
            Map.insert (qcRound qc) qc lldbNonFinalizedQuorumCertificates
        }

-- | The class 'HasMemoryLLDB' is implemented by a context in which a 'LowLevelDB' state is
--  maintained in an 'IORef'. This provides access to the low-level database when the monad implements
--  @MonadReader r@ and @MonadIO@.
class HasMemoryLLDB store pv r | r -> store pv where
    theMemoryLLDB :: r -> IORef (LowLevelDB store pv)

-- | Helper for reading the low level DB.
readLLDB :: (MonadReader r m, HasMemoryLLDB store pv r, MonadIO m) => m (LowLevelDB store pv)
readLLDB = liftIO . readIORef =<< asks theMemoryLLDB

-- | Helper for updating the low level DB.
withLLDB :: (MonadReader r m, HasMemoryLLDB store pv r, MonadIO m) => (LowLevelDB store pv -> (LowLevelDB store pv, a)) -> m a
withLLDB f = do
    ref <- asks theMemoryLLDB
    liftIO $ atomicModifyIORef' ref f

-- | Helper for updating the low level DB.
withLLDB_ :: (MonadReader r m, HasMemoryLLDB store pv r, MonadIO m) => (LowLevelDB store pv -> LowLevelDB store pv) -> m ()
withLLDB_ f = withLLDB $ (,()) . f

-- | A newtype wrapper that provides an instance of 'MonadTreeStateStore' where the underlying monad
--  provides a context for accessing the low-level state. That is, it implements @MonadIO@ and
--  @MonadReader r@ for @r@ with @HasMemoryLLDB pv r@.
newtype MemoryLLDBM store (pv :: ProtocolVersion) m a = MemoryLLDBM {runMemoryLLDBM :: m a}
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (MonadReader r m) => MonadReader r (MemoryLLDBM store pv m)

instance (IsProtocolVersion pv) => MonadProtocolVersion (MemoryLLDBM store pv m) where
    type MPV (MemoryLLDBM store pv m) = pv

type instance MBSStore (MemoryLLDBM store pv m) = store

instance (IsProtocolVersion pv, MonadReader r m, HasMemoryLLDB store pv r, MonadIO m) => MonadTreeStateStore (MemoryLLDBM store pv m) where
    lookupBlock bh =
        readLLDB <&> HM.lookup bh . lldbBlocks
    memberBlock = fmap isJust . lookupBlock
    lookupFirstBlock =
        readLLDB <&> \db -> do
            (_, firstHash) <- Map.lookupMin (lldbFinalizedBlocks db)
            HM.lookup firstHash (lldbBlocks db)
    lookupLastFinalizedBlock =
        readLLDB <&> \db -> do
            (_, lastHash) <- Map.lookupMax (lldbFinalizedBlocks db)
            HM.lookup lastHash (lldbBlocks db)
    lookupBlockByHeight h =
        readLLDB <&> \db -> do
            hsh <- Map.lookup h (lldbFinalizedBlocks db)
            HM.lookup hsh (lldbBlocks db)
    lookupTransaction th =
        readLLDB <&> HM.lookup th . lldbTransactions
    memberTransaction = fmap isJust . lookupTransaction

    writeFinalizedBlocks finBlocks finEntry =
        withLLDB_ $ doWriteFinalizedBlocks finBlocks finEntry

    writeCertifiedBlock certBlock qc =
        withLLDB_ $ doWriteCertifiedBlock certBlock qc

    writeCertifiedBlockWithFinalization finBlocks certBlock finEntry =
        withLLDB_ $
            doWriteCertifiedBlock certBlock qc
                . doWriteFinalizedBlocks finBlocks finEntry
      where
        qc = feSuccessorQuorumCertificate finEntry

    lookupCertifiedBlocks =
        readLLDB <&> \LowLevelDB{..} ->
            toBlock lldbBlocks <$> Map.elems lldbNonFinalizedQuorumCertificates
      where
        toBlock blocks qc = case HM.lookup (qcBlock qc) blocks of
            Nothing -> error $ "Missing block for QC: " <> show (qcBlock qc)
            Just b -> (b, qc)

    lookupLatestFinalizationEntry =
        readLLDB <&> lldbLatestFinalizationEntry

    lookupCurrentRoundStatus =
        readLLDB <&> lldbRoundStatus

    writeCurrentRoundStatus rs =
        withLLDB_ $ \db -> db{lldbRoundStatus = rs}
