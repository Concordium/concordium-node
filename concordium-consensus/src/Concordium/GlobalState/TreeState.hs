{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.TreeState(
    module Concordium.GlobalState.Classes,
    module Concordium.GlobalState.Types,
    module Concordium.GlobalState.Block,
    module Concordium.GlobalState.TreeState
) where

import qualified Data.Sequence as Seq
import Data.Time
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import System.IO
import Lens.Micro.Platform
import Control.Monad.Reader
import Data.Maybe (isJust)

import Concordium.GlobalState.Block (BlockData(..), BlockPendingData (..), PendingBlock(..))
import Concordium.GlobalState.BlockPointer (BlockPointerData(..))
import Concordium.GlobalState.Types
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TransactionTable
import Concordium.Types.Transactions as Transactions
import Concordium.Types.Execution(TransactionIndex)
import Concordium.GlobalState.Statistics
import Concordium.Types.HashableTo
import Concordium.Types
import Concordium.Types.Updates
import Concordium.GlobalState.AccountTransactionIndex

import Data.ByteString
import Concordium.Logger
import Data.Serialize as S
import Concordium.Common.Version (Version)
import qualified Concordium.GlobalState.Block as B
import Data.Bits
import qualified Concordium.TransactionVerification as TVer

data BlockStatus bp pb =
    BlockAlive !bp
    | BlockDead
    | BlockFinalized !bp !FinalizationRecord
    | BlockPending !pb
  deriving(Eq)

instance Show (BlockStatus bp pb) where
    show (BlockAlive _) = "Alive"
    show (BlockDead) = "Dead"
    show (BlockFinalized _ _) = "Finalized"
    show (BlockPending _) = "Pending"

-- |Branches of a tree represented as a sequence, ordered by height above the last
-- finalized block, of lists of block pointers.  The blocks in the branches should
-- be exactly the live blocks.  If a block is in the branches, then either it is at
-- the lowest level and its parent is the last finalized block, or its parent is also
-- in the branches at the level below.
type Branches m = Seq.Seq [BlockPointerType m]

-- |Result of trying to add a transaction to the transaction table.
data AddTransactionResult =
  -- |Transaction is a duplicate of the given transaction.
  Duplicate !BlockItem |
  -- |The transaction was newly added.
  Added !BlockItem !TVer.VerificationResult |
  -- |The nonce of the transaction is not later than the last finalized transaction for the sender.
  -- The transaction is not added to the table.
  ObsoleteNonce |
  -- |The transaction was not added as it could not be deemed verifiable.
  -- The `NotAdded` contains the `VerificationResult`
  NotAdded !TVer.VerificationResult
  deriving(Eq, Show)

-- |Monad that provides operations for working with the low-level tree state.
-- These operations are abstracted where possible to allow for a range of implementation
-- choices.
class (Eq (BlockPointerType m),
       Ord (BlockPointerType m),
       HashableTo BlockHash (BlockPointerType m),
       BlockData (BlockPointerType m),
       BlockPointerData (BlockPointerType m),
       BlockStateStorage m,
       BlockPointerMonad m,
       B.EncodeBlock pv (BlockPointerType m),
       PerAccountDBOperations m,
       Monad m,
       IsProtocolVersion pv)
      => TreeStateMonad pv m | m -> pv where

    -- * 'PendingBlock' operations
    -- |Create and sign a 'PendingBlock`.
    makePendingBlock ::
        BakerSignPrivateKey -- ^Key for signing the new block
        -> Slot             -- ^Block slot (must be non-zero)
        -> BlockHash        -- ^Hash of parent block
        -> BakerId          -- ^Identifier of block baker
        -> BlockProof       -- ^Block proof
        -> BlockNonce       -- ^Block nonce
        -> BlockFinalizationData
                            -- ^Finalization data
        -> [BlockItem]      -- ^List of transactions
        -> StateHash                  -- ^Statehash of the block.
        -> TransactionOutcomesHash     -- ^TransactionOutcomesHash of block.
        -> UTCTime          -- ^Block receive time
        -> m PendingBlock

    -- * Operations on the block table
    -- |Get the current status of a block.
    getBlockStatus :: BlockHash -> m (Maybe (BlockStatus (BlockPointerType m) PendingBlock))
    -- |Make a live 'BlockPointer' from a 'PendingBlock'.
    -- The parent and last finalized pointers must be correct.
    makeLiveBlock ::
        PendingBlock                         -- ^Block to make live
        -> BlockPointerType m                -- ^Parent block pointer
        -> BlockPointerType m                -- ^Last finalized block pointer
        -> BlockState m                      -- ^Block state
        -> ATIStorage m                      -- ^This block's account -> transaction index.
        -> UTCTime                           -- ^Block arrival time
        -> Energy                            -- ^Energy cost of the transactions in the block.
        -> m (BlockPointerType m)
    -- |Mark a block as dead. This should only be used directly if there are no other state invariants
    -- which should be maintained. See 'markLiveBlockDead' for an alternative method which maintains more invariants.
    markDead :: BlockHash -> m ()
    -- |Mark a live block as dead. In addition, purge the block state and maintain invariants in the
    -- transaction table by purging all transaction outcomes that refer to this block.
    -- This has a default implementation in terms of 'markDead', 'purgeBlockState' and 'markDeadTransaction'.
    markLiveBlockDead :: BlockPointerType m -> m ()
    markLiveBlockDead bp = do
      let bh = getHash bp
      -- Mark the block dead
      markDead bh
      -- remove the block state
      purgeBlockState =<< blockState bp
      -- and remove the status of all transactions in this block
      mapM_ (markDeadTransaction bh) (blockTransactions bp)

    -- |Mark a block as finalized (by a particular 'FinalizationRecord').
    --
    -- Precondition: The block must be alive.
    markFinalized :: BlockHash -> FinalizationRecord -> m ()
    -- |Mark a block as pending (i.e. awaiting parent)
    markPending :: PendingBlock -> m ()
    -- |Mark every live or pending block as dead.
    markAllNonFinalizedDead :: m ()
    -- * Queries on genesis block
    -- |Get the genesis 'BlockPointer'.
    getGenesisBlockPointer :: m (BlockPointerType m)
    -- |Get the 'GenesisData'.
    getGenesisData :: m (GenesisData pv)
    -- * Operations on the finalization list
    -- |Get the last finalized block.
    getLastFinalized :: m (BlockPointerType m, FinalizationRecord)
    -- |Get the slot number of the last finalized block
    getLastFinalizedSlot :: m Slot
    getLastFinalizedSlot = blockSlot . fst <$> getLastFinalized
    -- |Get the height of the last finalized block
    getLastFinalizedHeight :: m BlockHeight
    getLastFinalizedHeight = bpHeight . fst <$> getLastFinalized
    -- |Get the next finalization index.
    getNextFinalizationIndex :: m FinalizationIndex
    getNextFinalizationIndex = (+1) . finalizationIndex . snd <$> getLastFinalized
    -- |Add a block and finalization record to the finalization list.
    -- The block must be the one finalized by the record, and the finalization
    -- index must be the next finalization index.  These are not checked.
    addFinalization :: BlockPointerType m -> FinalizationRecord -> m ()
    -- |Get the block that is finalized at the given index.
    -- Returns 'Nothing' if no such block exists.
    getFinalizedAtIndex :: FinalizationIndex -> m (Maybe (BlockPointerType m))
    -- |Get the finalization record at the given index, if any.
    getRecordAtIndex :: FinalizationIndex -> m (Maybe FinalizationRecord)

    -- |Get the block that is finalized at the given height, if any.
    getFinalizedAtHeight :: BlockHeight -> m (Maybe (BlockPointerType m))

    -- * Operations on branches
    -- |Get the branches.
    getBranches :: m (Branches m)
    -- |Set the branches.
    putBranches :: Branches m -> m ()
    -- * Operations on blocks that are pending the arrival of other blocks
    --
    -- $pendingBlocks
    -- Pending blocks are conceptually stored in a min priority search queue,
    -- where multiple blocks may have the same key, which is their parent,
    -- and the priority is the block's slot number.
    -- When a block arrives (possibly dead), its pending children are removed
    -- from the queue and handled.  This uses 'takePendingChildren'.
    -- When a block is finalized, all pending blocks with a lower or equal slot
    -- number can be handled (they will become dead, since they can no longer
    -- join the tree).  This uses 'takeNextPendingUntil'.
    --
    -- |Return a list of the blocks that are pending the given parent block,
    -- removing them from the pending table.
    takePendingChildren :: BlockHash -> m [PendingBlock]
    -- |Add a pending block, that is pending on the arrival of its parent.
    addPendingBlock :: PendingBlock -> m ()
    -- |Return the next block that is pending its parent with slot number
    -- less than or equal to the given value, removing it from the pending
    -- table.  Returns 'Nothing' if there is no such pending block.
    takeNextPendingUntil :: Slot -> m (Maybe PendingBlock)
    -- |Remove all blocks from the pending table. This is only intended to
    -- be used on Skov shut down, when these blocks are no longer needed.
    wipePendingBlocks :: m ()

    -- * Operations on the pending transaction table
    --
    -- $pendingTransactions
    -- We maintain a 'PendingTransactionTable' for a particular block that is
    -- the focus block.  (Ideally, this should be the best block, however, it
    -- shouldn't be a problem if it's not.)
    -- |Return the focus block.
    getFocusBlock :: m (BlockPointerType m)
    -- |Update the focus block.
    putFocusBlock :: BlockPointerType m -> m ()
    -- |Get the pending transactions after execution of the focus block.
    getPendingTransactions :: m PendingTransactionTable
    -- |Set the pending transactions after execution of the focus block.
    putPendingTransactions :: PendingTransactionTable -> m ()

    -- * Operations on the transaction table
    -- |Get non-finalized transactions for the given account starting at the given nonce (inclusive).
    -- These are returned as an ordered list of pairs of nonce and non-empty set of transactions
    -- with that nonce. Transaction groups are ordered by increasing nonce.

    getAccountNonFinalized ::
      AccountAddressEq
      -> Nonce
      -> m [(Nonce, Map.Map Transaction TVer.VerificationResult)]
    -- |Get the successor of the largest known account for the given account
    -- The function should return 'True' in the second component if and only if
    -- all (known) transactions from this account are finalized.
    getNextAccountNonce :: AccountAddressEq -> m (Nonce, Bool)

    -- |Get a credential which has not yet been finalized, i.e., it is correct for this function
    -- to return 'Nothing' if the requested credential has already been finalized.
    getCredential :: TransactionHash -> m (Maybe (CredentialDeploymentWithMeta, TVer.VerificationResult))

    -- |Get non-finalized chain updates of a given type starting at the given sequence number
    -- (inclusive). This are returned as an ordered list of pairs of sequence number and
    -- non-empty set of updates with that sequence number. Update groups are ordered by
    -- increasing sequence number.
    getNonFinalizedChainUpdates ::
      UpdateType
      -> UpdateSequenceNumber
      -> m [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]

    -- |Finalize a list of transactions on a given block. Per account, the transactions must be in
    -- continuous sequence by nonce, starting from the next available non-finalized
    -- nonce.
    finalizeTransactions :: BlockHash -> Slot -> [BlockItem] -> m ()
    -- |Mark a transaction as committed on a block with the given slot number,
    -- as well as add any additional outcomes for the given block (outcomes are given
    -- as the index of the transaction in the given block).
    -- This will prevent it from being purged while the slot number exceeds
    -- that of the last finalized block.
    commitTransaction :: Slot -> BlockHash -> BlockItem -> TransactionIndex -> m () 
    -- |@addCommitTransaction tr verResCtx timestamp slot@ verifies a transaction within the
    -- given context and adds the transaction and marks it committed
    -- for the given slot number. If the transaction was deemed verifiable in the future.
    -- By default the transaction is created in the 'Received' state,
    -- but if the transaction is already in the table the outcomes are retained.
    -- See documentation of 'AddTransactionResult' for meaning of the return value.
    -- The time is indicative of the receive time of the transaction. It is used to prioritize transactions
    -- when constructing a block.
    -- Before the transaction is added the transaction will be verified. If it is not ok then it will return
    -- `NotAdded` together with the verification result.
    -- The scheduler will need to consult the resulting `VerificationResult` and based on that carry out the correct
    -- verification before executing the transaction.
    addCommitTransaction :: BlockItem -> Context (BlockState m) -> Timestamp -> Slot -> m AddTransactionResult
    -- |Purge a transaction from the transaction table if its last committed slot
    -- number does not exceed the slot number of the last finalized block.
    -- (A transaction that has been committed to a finalized block should not be purged.)
    -- Returns @True@ if and only if the transaction is purged.
    purgeTransaction :: BlockItem -> m Bool
    -- |Get the `VerificationResult` for a `BlockItem` if such one exist.
    -- A `VerificationResult` exists for `Received` and `Committed` transactions while 
    -- finalized transactions will yield a `Nothing`.
    getNonFinalizedTransactionVerificationResult :: BlockItem -> m (Maybe TVer.VerificationResult)

    -- |Purge transactions from the transaction table and pending transactions.
    -- Transactions are purged only if they are not included in a live block, and
    -- have either expired or arrived longer ago than the transaction keep alive time.
    --
    -- If the first argument is @False@, the transaction table is only purged if
    -- 'rpInsertionsBeforeTransactionPurged' transactions have been inserted since
    -- the last purge.  If it is true, the table is purged regardless.
    --
    -- WARNING: This function violates the independence of the tree state components.
    -- In particular, the following invariants are assumed and maintained:
    --
    --   * Every 'BlockItem' in the transaction table that is not included in a live
    --     or finalized block is referenced in the pending transaction table.  That is,
    --     for a basic transaction the '_pttWithSender' table contains an entry for
    --     the sender where the nonce of the transaction falls within the range,
    --     and for a credential deployment the transaction hash is included in '_pttDeployCredential'.
    --
    --   * The low nonce for each entry in '_pttWithSender' is at least the last finalized
    --     nonce recorded in the account's non-finalized transactions in the transaction
    --     table.
    --
    --   * The finalization list must reflect the current last finalized block.
    --
    --   * The pending transaction table only references transactions that are in the
    --     transaction table.  That is, the high nonce in a range is a tight bound and
    --     the deploy credential hashes correspond to transactions in the table.
    --
    --   * No non-finalized block is considered live or will become live if its slot
    --     is less than or equal to the slot number of the last finalized block.
    --
    --   * If a transaction is known to be in any block that is not finalized or dead,
    --     then 'commitTransaction' or 'addCommitTransaction' has been called with a
    --     slot number at least as high as the slot number of the block.
    purgeTransactionTable :: Bool -- ^ Whether to ignore the amount of insertions and forcedly perform a purge
                          -> UTCTime -- ^ Current time
                          -> m ()

    -- |Mark a transaction as no longer on a given block. This is used when a block is
    -- marked as dead.
    markDeadTransaction :: BlockHash -> BlockItem -> m ()
    -- |Lookup a transaction status by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe TransactionStatus)
    -- |Remove and return all non-finalized transactions from the transaction table.
    -- This is intended for use in shutting down the Skov, since it disregards whether
    -- transactions are present in blocks.
    wipeNonFinalizedTransactions :: m [BlockItem]


    -- * Operations on statistics
    -- |Get the current consensus statistics.
    getConsensusStatistics :: m ConsensusStatistics
    -- |Set the consensus statistics.
    putConsensusStatistics :: ConsensusStatistics -> m ()

    -- |Get other runtime parameters that are implementation detail, and hence do
    -- not belong to genesis data.
    getRuntimeParameters :: m RuntimeParameters

instance (Monad (t m), MonadTrans t, TreeStateMonad pv m) => TreeStateMonad pv (MGSTrans t m) where
    makePendingBlock key slot parent bid pf n lastFin trs statehash transactionOutcomesHash = lift . makePendingBlock key slot parent bid pf n lastFin trs statehash transactionOutcomesHash
    getBlockStatus = lift . getBlockStatus
    makeLiveBlock b parent lastFin st ati time = lift . makeLiveBlock b parent lastFin st ati time
    markDead = lift . markDead
    markFinalized bh = lift . markFinalized bh
    markPending = lift . markPending
    markAllNonFinalizedDead = lift markAllNonFinalizedDead
    getGenesisBlockPointer = lift getGenesisBlockPointer
    getGenesisData = lift getGenesisData
    getLastFinalized = lift getLastFinalized
    getLastFinalizedSlot = lift getLastFinalizedSlot
    getLastFinalizedHeight = lift getLastFinalizedHeight
    getNextFinalizationIndex = lift getNextFinalizationIndex
    addFinalization bp = lift . addFinalization bp
    getFinalizedAtIndex = lift . getFinalizedAtIndex
    getRecordAtIndex = lift . getRecordAtIndex
    getFinalizedAtHeight = lift . getFinalizedAtHeight
    getBranches = lift getBranches
    putBranches = lift . putBranches
    takePendingChildren = lift . takePendingChildren
    addPendingBlock = lift . addPendingBlock
    takeNextPendingUntil = lift . takeNextPendingUntil
    wipePendingBlocks = lift wipePendingBlocks
    getFocusBlock = lift getFocusBlock
    putFocusBlock = lift . putFocusBlock
    getPendingTransactions = lift getPendingTransactions
    putPendingTransactions = lift . putPendingTransactions
    getAccountNonFinalized acc = lift . getAccountNonFinalized acc
    getNextAccountNonce = lift . getNextAccountNonce
    getCredential = lift . getCredential
    getNonFinalizedChainUpdates uty = lift . getNonFinalizedChainUpdates uty
    finalizeTransactions bh slot = lift . finalizeTransactions bh slot
    commitTransaction slot bh tr = lift . commitTransaction slot bh tr
    addCommitTransaction tr ctx ts slot = lift $ addCommitTransaction tr ctx ts slot
    purgeTransaction = lift . purgeTransaction
    wipeNonFinalizedTransactions = lift wipeNonFinalizedTransactions
    markDeadTransaction bh = lift . markDeadTransaction bh
    lookupTransaction = lift . lookupTransaction
    getConsensusStatistics = lift getConsensusStatistics
    putConsensusStatistics = lift . putConsensusStatistics
    getRuntimeParameters = lift getRuntimeParameters
    purgeTransactionTable tm = lift . (purgeTransactionTable tm)
    getNonFinalizedTransactionVerificationResult = lift . getNonFinalizedTransactionVerificationResult
    {-# INLINE makePendingBlock #-}
    {-# INLINE getBlockStatus #-}
    {-# INLINE makeLiveBlock #-}
    {-# INLINE markDead #-}
    {-# INLINE markFinalized #-}
    {-# INLINE markPending #-}
    {-# INLINE markAllNonFinalizedDead #-}
    {-# INLINE getGenesisBlockPointer #-}
    {-# INLINE getGenesisData #-}
    {-# INLINE getLastFinalized #-}
    {-# INLINE getLastFinalizedSlot #-}
    {-# INLINE getLastFinalizedHeight #-}
    {-# INLINE getNextFinalizationIndex #-}
    {-# INLINE addFinalization #-}
    {-# INLINE getFinalizedAtIndex #-}
    {-# INLINE getRecordAtIndex #-}
    {-# INLINE getFinalizedAtHeight #-}
    {-# INLINE getBranches #-}
    {-# INLINE putBranches #-}
    {-# INLINE takePendingChildren #-}
    {-# INLINE addPendingBlock #-}
    {-# INLINE takeNextPendingUntil #-}
    {-# INLINE wipePendingBlocks #-}
    {-# INLINE getFocusBlock #-}
    {-# INLINE putFocusBlock #-}
    {-# INLINE getPendingTransactions #-}
    {-# INLINE putPendingTransactions #-}
    {-# INLINE getAccountNonFinalized #-}
    {-# INLINE getNextAccountNonce #-}
    {-# INLINE getCredential #-}
    {-# INLINE getNonFinalizedChainUpdates #-}
    {-# INLINE finalizeTransactions #-}
    {-# INLINE commitTransaction #-}
    {-# INLINE addCommitTransaction #-}
    {-# INLINE purgeTransaction #-}
    {-# INLINE wipeNonFinalizedTransactions #-}
    {-# INLINE lookupTransaction #-}
    {-# INLINE markDeadTransaction #-}
    {-# INLINE getConsensusStatistics #-}
    {-# INLINE putConsensusStatistics #-}
    {-# INLINE getRuntimeParameters #-}
    {-# INLINE purgeTransactionTable #-}
    {-# INLINE getNonFinalizedTransactionVerificationResult #-}

deriving via (MGSTrans MaybeT m) instance TreeStateMonad pv m => TreeStateMonad pv (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance TreeStateMonad pv m => TreeStateMonad pv (ExceptT e m)

data ImportingResult a = SerializationFail | Success | OtherError a deriving (Show)

-- |Read the header of the export file.
-- The header consists of
--
-- - version number that determines the format of all the subsequent blocks in the file.
--
-- The function returns, if successful, the version, and the unconsumed input.
readHeader :: ByteString -> Either String Version
readHeader = S.runGet S.get

getVersionBytes :: Handle -> IO ByteString
getVersionBytes h = do
  b <- hGet h 1
  if testBit (Data.ByteString.head b) 7
    then
    append b <$> getVersionBytes h
    else
    return b

-- |Read a block file in V2 format, invoking the supplied
-- callback on each block.
readBlocksV2 :: (Show a) => Handle
           -> UTCTime
           -> LogMethod IO
           -> LogSource
           -> (PendingBlock -> IO (ImportingResult a))
           -> IO (ImportingResult a)
readBlocksV2 h tm logm logLvl continuation = do
  v <- getVersionBytes h
  case readHeader v of
      Left err -> do
        logm logLvl LLError $ "Error deserializing header: " ++ err
        return SerializationFail
      Right version
          | version == 2 -> loopV2
          | otherwise -> do
              logm logLvl LLError $ "Unsupported version: " ++ show version
              return SerializationFail
  where loopV2 = do
          isEof <- hIsEOF h
          if isEof
            then return Success
            else do
            len <- S.runGet S.getWord64be <$> hGet h 8
            case len of
              Left _ -> return SerializationFail
              Right l -> do
                blockBS <- hGet h (fromIntegral l)
                result <- importBlockV2 blockBS tm logm logLvl continuation
                case result of
                  Success -> loopV2
                  err -> do -- stop processing at first error that we encounter.
                    logm External LLError $ "Error importing block: " ++ show err
                    return err

-- |Handle loading a single block.
importBlockV2 :: ByteString
              -> UTCTime
              -> LogMethod IO
              -> LogSource
              -> (PendingBlock -> IO (ImportingResult a))
              -> IO (ImportingResult a)
importBlockV2 blockBS tm logm logLvl continuation =
  case B.deserializePendingBlock SP1 blockBS tm of
    Left err -> do
      logm logLvl LLError $ "Can't deserialize block: " ++ show err
      return SerializationFail
    Right block -> continuation block

-- | Exists so we can have the `ProtocolVersion` in the `Reader` computation ctx. 
newtype ProtocolVersionedReaderT (pv :: ProtocolVersion) r m a = ProtocolVersionedReaderT {runProtocolVersionedReaderT :: ReaderT r m a}
  deriving (Functor, Applicative, Monad, MonadReader r, MonadTrans)

instance BlockStateTypes (ProtocolVersionedReaderT pv r m) where
    type BlockState (ProtocolVersionedReaderT pv r m) = BlockState m
    type UpdatableBlockState (ProtocolVersionedReaderT pv r m) = UpdatableBlockState m
    type Account (ProtocolVersionedReaderT pv r m) = Account m
    type ContractState (ProtocolVersionedReaderT pv r m) = ContractState m

-- |The Context of which a transaction is verified within 
-- in the reader based instance.
-- The `Context` contains the `BlockState`, the maximum energy of a block and
-- also whether the transaction was received individually or as part of a block.
--
-- The `Context` is used for verifying the transaction in a deferred manner.
-- That is, the verification process will only take place if the transaction is not already contained
-- in the `TransactionTable`.
-- Note. The `Context` is created when the transaction is received by `doReceiveTransactionInternal` and
-- the actual verification is carried out within `addCommitTransaction` when it has been checked
-- that the transaction does not already exist in the `TransactionTable`.
data Context t = Context {
  _ctxBs :: t,
  _ctxMaxBlockEnergy :: !Energy,
  -- |Whether the transaction was received from a block or individually.
  _isTransactionFromBlock :: Bool
  }
makeLenses ''Context

instance (Monad m,
          BlockStateQuery m,
          AccountOperations m,
          TreeStateMonad pv m,
          r ~ Context (BlockState m)) => TVer.TransactionVerifier pv (ProtocolVersionedReaderT pv r m) where
  {-# INLINE getIdentityProvider #-}
  getIdentityProvider ipId = do
    ctx <- ask
    lift (getIdentityProvider (ctx ^. ctxBs) ipId)
  {-# INLINE getAnonymityRevokers #-}
  getAnonymityRevokers arrIds = do
    ctx <- ask
    lift (getAnonymityRevokers (ctx ^. ctxBs) arrIds)
  {-# INLINE getCryptographicParameters #-}
  getCryptographicParameters = do
    ctx <- ask
    lift (getCryptographicParameters (ctx ^. ctxBs))
  {-# INLINE registrationIdExists #-}
  registrationIdExists regId = do
    ctx <- ask
    lift $ isJust <$> getAccountByCredId (ctx ^. ctxBs) regId
  {-# INLINE getAccount #-}
  getAccount aaddr = do
    ctx <- ask
    fmap snd <$> lift (getAccount (ctx ^. ctxBs) aaddr)
  {-# INLINE getNextUpdateSequenceNumber #-}
  getNextUpdateSequenceNumber uType = do
     ctx <- ask
     lift (getNextUpdateSequenceNumber (ctx ^. ctxBs) uType)
  {-# INLINE getUpdateKeysCollection #-}
  getUpdateKeysCollection = do
    ctx <- ask
    lift (getUpdateKeysCollection (ctx ^. ctxBs))
  {-# INLINE getAccountAvailableAmount #-}
  getAccountAvailableAmount = lift . getAccountAvailableAmount
  {-# INLINE getNextAccountNonce #-}
  getNextAccountNonce acc = do
    ctx <- ask
    -- If the transaction was received as part of a block
    -- then we check the account nonce from the `BlockState` in the context
    -- Otherwise if the transaction was received individually then we
    -- check the transaction table for the nonce.
    if ctx ^. isTransactionFromBlock
    then lift (getAccountNonce acc)
    else do
      aaddr <- lift (getAccountCanonicalAddress acc)
      lift (fst <$> getNextAccountNonce (accountAddressEmbed aaddr))
  {-# INLINE getAccountVerificationKeys #-}
  getAccountVerificationKeys = lift . getAccountVerificationKeys
  {-# INLINE energyToCcd #-}
  energyToCcd v = do
    ctx <- ask
    rate <- lift (getEnergyRate (ctx ^. ctxBs))
    return (computeCost rate v)
  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = do
    ctx <- ask
    return (ctx ^. ctxMaxBlockEnergy)
  {-# INLINE checkExactNonce #-}
  checkExactNonce = do
    ctx <- ask
    pure $ not (ctx ^. isTransactionFromBlock)
