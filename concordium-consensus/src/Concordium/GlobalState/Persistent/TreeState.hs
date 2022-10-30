{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}
-- FIXME: This is to suppress compiler warnings for derived instances of BlockStateOperations.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- |This module provides a monad that is an instance of both `LMDBStoreMonad`, `LMDBQueryMonad`,
-- and `TreeStateMonad` effectively adding persistence to the tree state.
module Concordium.GlobalState.Persistent.TreeState where

import Concordium.GlobalState.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlockPointer as PB
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.PurgeTransactions
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions as T
import Concordium.Types.Updates
import Control.Exception hiding (handle)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Typeable
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import Concordium.Utils
import System.Mem.Weak
import Concordium.Logger
import Control.Monad.Except
import System.FilePath
import System.Directory
import System.IO.Error
import qualified Concordium.TransactionVerification as TVer

-- * Exceptions

-- |Exceptions that can be thrown while the TreeState is being initialized
data InitException =
  -- |Block state path is a directory.
  BlockStatePathDir
  -- |Cannot get the read/write permissions for the block state file.
  | BlockStatePermissionError
  -- |Cannot get the read/write permissions for the tree state file.
  | TreeStatePermissionError
  -- |Generic database opening error
  | DatabaseOpeningError !IOError
  -- |Genesis block not in the database, but it should be.
  | GenesisBlockNotInDataBaseError
  -- |Genesis block incorrect.
  | GenesisBlockIncorrect {
      -- |Hash of genesis as in the database.
      ieIs :: !BlockHash
      }
  | DatabaseInvariantViolation !String
  -- |The database version is not correct.
  | IncorrectDatabaseVersion !String
  deriving(Show, Typeable)

instance Exception InitException where
  displayException BlockStatePathDir = "Block state path points to a directory, not a file."
  displayException BlockStatePermissionError = "Cannot get read and write permissions to the block state file."
  displayException TreeStatePermissionError = "Cannot get read and write permissions to the tree state file."
  displayException (DatabaseOpeningError err) = "Database error: " ++ displayException err
  displayException GenesisBlockNotInDataBaseError = "Genesis block not in the database."
  displayException (GenesisBlockIncorrect bh) =
    "Incorrect genesis block. Genesis block in the database does not match the genesis data. Hash of the database genesis block is: " ++ show bh
  displayException (DatabaseInvariantViolation err) =
    "Database invariant violation: " ++ err
  displayException (IncorrectDatabaseVersion err) = "Incorrect database version: " ++ err

logExceptionAndThrowTS :: (MonadLogger m, MonadIO m, Exception e) => e -> m a
logExceptionAndThrowTS = logExceptionAndThrow TreeState

logErrorAndThrowTS :: (MonadLogger m, MonadIO m) => String -> m a
logErrorAndThrowTS = logErrorAndThrow TreeState

--------------------------------------------------------------------------------

-- * Persistent version of the Skov Data

-- |BlockStatus as recorded in the persistent implementation
data PersistentBlockStatus pv bs =
    BlockAlive !(PersistentBlockPointer pv bs)
    | BlockPending !PendingBlock
  deriving(Eq, Show)

-- |Cache of dead blocks, which are blocks which have been either pending or
-- alive once, but have been marked dead by finalization, or which we have
-- received and deemed as invalid. The intention is that this acts as a kind of
-- FIFO cache, except that inserting a duplicate into the cache has no effect on
-- it.
--
-- One question is why this is even needed and it is indeed unclear that it is
-- needed. It should not be necessary for correctness, but it likely does help
-- performance in some cases.
--
-- - Live blocks are only marked dead upon finalization (this is done
--   transitively, i.e., a descendant of a dead block is marked as dead). Blocks
--   can only arrive if they are no more than 30s in the future, and are above
--   (i.e., later than) the last finalized block. This cache helps in the case
--   where a branch is declared dead, and either duplicate blocks from that
--   branch, or successors of blocks on that branch, arrive. Without the cache
--   they would end up (potentially) in the pending state, with the cache they
--   might be deemed as duplicate (but they might be deemed duplicate at the
--   network already) or dead quickly. If finalization is reliable then even if
--   we get duplicate pending blocks they will be relatively quickly marked dead
--   themselves. The node itself has deduplication at the network layer.
--
-- - If a block is received and immediately deemed invalid, then it is marked as
--   dead. In such a case it is good to remember this for some time in case the
--   network layer deduplication is insufficient.
data DeadCache = DeadCache
    { -- |Set of hashes currently in the cache.
      _dcHashes :: !(HS.HashSet BlockHash)
      -- |Queue of hashes. The beginning of the queue is the item that was
      -- inserted first (i.e., oldest hash).
    , _dcQueue :: !(Seq.Seq BlockHash)
    }
    deriving (Eq, Show)

-- |Maximum number of hashes to maintain in the cache. There is no particularly
-- strong reason for using 1000 other than it seems like it should be big enough
-- with realistic block times, as well as not too large so that cache
-- performance is still excellent.
deadCacheSize :: Int
deadCacheSize = 1000

emptyDeadCache :: DeadCache
emptyDeadCache = DeadCache HS.empty Seq.empty

-- |Update the cache with the given block hash. If the block hash is already in
-- the cache do nothing.
insertDeadCache :: BlockHash -> DeadCache -> DeadCache
insertDeadCache !bh dc@DeadCache{..}
    | HS.member bh _dcHashes = dc
    | otherwise =
        let newHashes = HS.insert bh _dcHashes
        in if HS.size newHashes > deadCacheSize
           then
             let (newHashes', newQueue) = case _dcQueue of
                   h Seq.:<| q -> (HS.delete h newHashes, q)
                   _ -> error "Invariant violation. Dead cache not consistent."
             in DeadCache
                { _dcHashes = newHashes'
                , _dcQueue = newQueue Seq.|> bh
                }
           else
             DeadCache
             { _dcHashes = newHashes
             , _dcQueue = _dcQueue Seq.|> bh
             }

-- |Return whether the given block hash is in the cache.
memberDeadCache :: BlockHash -> DeadCache -> Bool
memberDeadCache bh DeadCache{..} = HS.member bh _dcHashes

-- |A table of live and non-finalized blocks together with a small cache of dead
-- ones.
data BlockTable pv bs = BlockTable
  { _deadCache :: !DeadCache,
    _liveMap :: !(HM.HashMap BlockHash (PersistentBlockStatus pv bs))
  } deriving(Eq, Show)
makeLenses ''BlockTable

-- |A block table that does not contain any blocks, and has an empty cache of
-- dead blocks.
emptyBlockTable :: BlockTable pv bs
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- |Skov data for the persistent tree state version that also holds the database handlers.
-- The first type parameter, @pv@, is the protocol version.
-- The second type parameter, @ati@, is a type determining the account transaction index to use.
-- The third type parameter, @bs@, is the type of block states.
data SkovPersistentData (pv :: ProtocolVersion) bs = SkovPersistentData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(BlockTable pv bs),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |Pointer to the last finalized block
    _lastFinalized :: !(PersistentBlockPointer pv bs),
    -- |Pointer to the last finalization record
    _lastFinalizationRecord :: !FinalizationRecord,
    -- |Branches of the tree by height above the last finalized block
    _branches :: !(Seq.Seq [PersistentBlockPointer pv bs]),
    -- |Genesis data
    _genesisData :: !GenesisConfiguration,
    -- |Block pointer to genesis block
    _genesisBlockPointer :: !(PersistentBlockPointer pv bs),
    -- |Current focus block
    _focusBlock :: !(PersistentBlockPointer pv bs),
    -- |Pending transaction table
    _pendingTransactions :: !PendingTransactionTable,
    -- |Transaction table
    _transactionTable :: !TransactionTable,
    -- |Transaction table purge counter
    _transactionTablePurgeCounter :: !Int,
    -- |Consensus statistics
    _statistics :: !ConsensusStatistics,
    -- |Runtime parameters
    _runtimeParameters :: !RuntimeParameters,
    -- |Tree state directory
    _treeStateDirectory :: !FilePath,
    -- | Database handlers
    _db :: !(DatabaseHandlers pv (TS.BlockStatePointer bs)),
    -- |State where we store the initial state for the new protocol update.
    -- TODO: This is not an ideal solution, but seems simplest in terms of abstractions.
    -- If we only had the one state implementation this would not be necessary, and we could simply
    -- return the value in the 'updateRegenesis' function. However as it is, it is challenging to properly
    -- specify the types of these values due to the way the relevant types are parameterized.
    _nextGenesisInitialState :: !(Maybe bs)
}
makeLenses ''SkovPersistentData

instance (bsp ~ TS.BlockStatePointer bs)
    => HasDatabaseHandlers pv bsp (SkovPersistentData pv bs) where
  dbHandlers = db

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovPersistentDataDefault
    :: (IsProtocolVersion pv, FixedSizeSerialization (TS.BlockStatePointer bs), BlockStateQuery m, bs ~ BlockState m, MonadIO m)
    => FilePath
    -> GenesisConfiguration
    -> bs
    -> TS.BlockStatePointer bs -- ^How to serialize the block state reference for inclusion in the table.
    -> TransactionTable
    -> Maybe PendingTransactionTable
    -> m (SkovPersistentData pv bs)
initialSkovPersistentDataDefault = initialSkovPersistentData defaultRuntimeParameters

-- |Create an initial 'SkovPersistentData'.
-- The state does not need to be activated if the supplied 'TransactionTable' is correctly
-- initialised with the nonces and sequence numbers of the accounts and update types.
initialSkovPersistentData
    :: (IsProtocolVersion pv, FixedSizeSerialization (TS.BlockStatePointer bs), BlockStateQuery m, bs ~ BlockState m, MonadIO m)
    => RuntimeParameters
    -- ^Runtime parameters
    -> FilePath
    -- ^Tree state directory
    -> GenesisConfiguration
    -- ^Genesis data
    -> bs
    -- ^Genesis state
    -> TS.BlockStatePointer bs
    -- ^Genesis block state
    -> TransactionTable
    -- ^The table of transactions to start the configuration with. If this
    -- transaction table has any non-finalized transactions then the pending
    -- table corresponding to those non-finalized transactions must be supplied.
    -- This table should never have "Committed" transactions.
    -> Maybe PendingTransactionTable
    -- ^The initial pending transaction table. If the supplied __transaction
    -- table__ has transactions that are not finalized the pending table must be
    -- supplied to record these, satisfying the usual properties. See
    -- documentation of the 'PendingTransactionTable' for details.
    -> m (SkovPersistentData pv bs)
initialSkovPersistentData rp treeStateDir gd genState serState genTT mPending = do
  gb <- makeGenesisPersistentBlockPointer gd genState
  let gbh = bpHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  initialDb <- liftIO $ initializeDatabase gb serState treeStateDir
  return SkovPersistentData {
            _blockTable = emptyBlockTable,
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _lastFinalized = gb,
            _lastFinalizationRecord = gbfin,
            _branches = Seq.empty,
            _genesisData = gd,
            _genesisBlockPointer = gb,
            _focusBlock = gb,
            _pendingTransactions = fromMaybe emptyPendingTransactionTable mPending,
            _transactionTable = genTT,
            _transactionTablePurgeCounter = 0,
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _treeStateDirectory = treeStateDir,
            _db = initialDb,
            _nextGenesisInitialState = Nothing
        }

--------------------------------------------------------------------------------

-- * Initialization functions

--- |Check the permissions in the required files.  Returns 'True' if the database already exists,
--- 'False' if it does not exist, or is inaccessible.  If the database exists only partially, then it
--- is deleted to allow for creating it again through `newGenesis`.
checkExistingDatabase :: forall m. (MonadLogger m, MonadIO m) =>
    -- |Tree state path
    FilePath ->
    -- |Block state file
    FilePath ->
    m Bool
checkExistingDatabase treeStateDir blockStateFile = do
  let treeStateFile = treeStateDir </> "data.mdb"
  bsPathEx <- liftIO $ doesPathExist blockStateFile
  tsPathEx <- liftIO $ doesPathExist treeStateFile

  -- Check whether a path is a normal file that is readable and writable
  let checkRWFile :: FilePath -> InitException -> m ()
      checkRWFile path exc = do
        fileEx <- liftIO $ doesFileExist path
        unless fileEx $ logExceptionAndThrowTS BlockStatePathDir
        mperms <- liftIO $ catchJust (guard . isPermissionError)
                           (Just <$> getPermissions path)
                           (const $ return Nothing)
        case mperms of
          Nothing -> logExceptionAndThrowTS exc
          Just perms ->
            unless (readable perms && writable perms) $ do
            logExceptionAndThrowTS exc

  -- if both files exist we check whether they are both readable and writable.
  -- In case only one of them exists we raise an appropriate exception. We don't want to delete any data.
  if | bsPathEx && tsPathEx -> do
         -- check whether it is a normal file and whether we have the right permissions
         checkRWFile blockStateFile BlockStatePermissionError
         checkRWFile treeStateFile TreeStatePermissionError
         mapM_ (logEvent TreeState LLTrace) ["Existing database found.", "TreeState filepath: " ++ show blockStateFile, "BlockState filepath: " ++ show treeStateFile]
         return True
     | bsPathEx -> do
         logEvent GlobalState LLWarning "Block state file exists, but tree state database does not. Deleting the block state file."
         liftIO $ removeFile blockStateFile
         return False
     | tsPathEx -> do
         logEvent GlobalState LLWarning "Tree state database exists, but block state file does not. Deleting the tree state database."
         liftIO . removeDirectoryRecursive $ treeStateDir
         return False
     | otherwise ->
         return False

-- |Try to load an existing instance of skov persistent data.
-- This function will raise an exception if it detects invariant violation in the
-- existing state.
-- This can be for a number of reasons, but what is checked currently is
--
--   * blocks cannot be deserialized.
--   * database does not contain a block at height 0, i.e., genesis block
--   * database does not contain the right genesis block (one that would match genesis data)
--   * hash under which the genesis block is stored is not the computed hash of the genesis block
--   * missing finalization record for the last stored block in the database.
--
-- The state that is loaded is usable for queries (except the next nonce query),
-- but it is not usable for active consensus operation. For that,
-- 'activateSkovPersistentData' should be called which establishes the necessary
-- invariants in the transaction table, and caches the relevant state.
--
-- The reason for the split design is that activating the state is very time
-- consuming, and it is not needed when starting a node on a chain which had
-- multiple protocol updates.
loadSkovPersistentData :: forall pv. (IsProtocolVersion pv)
                       => RuntimeParameters
                       -> FilePath -- ^Tree state directory
                       -> PBS.PersistentBlockStateContext pv
                       -> LogIO (SkovPersistentData pv (PBS.HashedPersistentBlockState pv))
loadSkovPersistentData rp _treeStateDirectory pbsc = do
  -- we open the environment first.
  -- It might be that the database is bigger than the default environment size.
  -- This seems to not be an issue while we only read from the database,
  -- and on insertions we resize the environment anyhow.
  -- But this behaviour of LMDB is poorly documented, so we might experience issues.
  _db <- either (logExceptionAndThrowTS . DatabaseOpeningError) return =<<
          liftIO (try $ databaseHandlers _treeStateDirectory)

  -- Check that the database version matches what we expect.
  liftIO (checkDatabaseVersion _db) >>=
      either (logExceptionAndThrowTS . IncorrectDatabaseVersion) return

  -- Unroll the treestate if the last finalized blockstate is corrupted. If the last finalized
  -- blockstate is not corrupted, the treestate is unchanged.
  unrollTreeStateWhile _db (liftIO . isBlockStateCorrupted) >>= \case
    Left e -> logExceptionAndThrowTS . DatabaseInvariantViolation $
              "The block state database is corrupt. Recovery attempt failed: " <> e
    Right (_lastFinalizationRecord, lfStoredBlock) -> do
      -- Truncate the blobstore beyond the last finalized blockstate.
      liftIO $ truncateBlobStore (bscBlobStore . PBS.pbscBlobStore $ pbsc) (sbState lfStoredBlock)
      -- Get the genesis block.
      genStoredBlock <- maybe (logExceptionAndThrowTS GenesisBlockNotInDataBaseError) return =<<
              liftIO (getFirstBlock _db)
      _genesisBlockPointer <- liftIO $ makeBlockPointer genStoredBlock
      _genesisData <- case _bpBlock _genesisBlockPointer of
        GenesisBlock gd' -> return gd'
        _ -> logExceptionAndThrowTS (DatabaseInvariantViolation "Block at height 0 is not a genesis block.")
      -- Get the last finalized block.
      _lastFinalized <- liftIO (makeBlockPointer lfStoredBlock)
      return SkovPersistentData {
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _branches = Seq.empty,
            _focusBlock = _lastFinalized,
            _pendingTransactions = emptyPendingTransactionTable,
            _transactionTable = emptyTransactionTable,
            _transactionTablePurgeCounter = 0,
            -- The best thing we can probably do is use the initial statistics,
            -- and make the meaning of those with respect to the last time
            -- consensus started.
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _blockTable = BlockTable {_deadCache = emptyDeadCache, _liveMap = HM.empty},
            _nextGenesisInitialState = Nothing,
            ..
        }

  where
    makeBlockPointer :: StoredBlock pv (TS.BlockStatePointer (PBS.PersistentBlockState pv)) -> IO (PersistentBlockPointer pv (PBS.HashedPersistentBlockState pv))
    makeBlockPointer StoredBlock{..} = do
      bstate <- runReaderT (PBS.runPersistentBlockStateMonad (loadBlockState (blockStateHash sbBlock) sbState)) pbsc
      makeBlockPointerFromPersistentBlock sbBlock bstate sbInfo
    isBlockStateCorrupted :: StoredBlock pv (TS.BlockStatePointer (PBS.PersistentBlockState pv)) -> IO Bool
    isBlockStateCorrupted block =
      not <$> runBlobStoreT (isValidBlobRef (sbState block)) pbsc

-- |Activate the state and make it usable for use by consensus. This concretely
-- means that the block state for the last finalized block is cached, and that
-- the transaction table invariants are established. The latter means that the
-- next nonce recorded in the pending table is correct for the focus block
-- (which is the last finalized block).
--
-- This function will raise an IO exception in the following scenarios
-- * in the block state, an account which is listed cannot be loaded
activateSkovPersistentData :: forall pv. (IsProtocolVersion pv)
                           => PBS.PersistentBlockStateContext pv
                           -> SkovPersistentData pv (PBS.HashedPersistentBlockState pv)
                           -> LogIO (SkovPersistentData pv (PBS.HashedPersistentBlockState pv))
activateSkovPersistentData pbsc uninitState = 
  runBlockState $ do
    logEvent GlobalState LLTrace "Caching last finalized block and initializing transaction table"
    let bps = _bpState $ _lastFinalized uninitState
    tt <- PBS.cacheStateAndGetTransactionTable bps
    logEvent GlobalState LLTrace "Done caching last finalized block"
    return $! uninitState{_transactionTable = tt}
  where 
    runBlockState a =  runReaderT (PBS.runPersistentBlockStateMonad @pv a) pbsc


-- |Close the database associated with a 'SkovPersistentData'.
-- The database should not be used after this.
closeSkovPersistentData :: SkovPersistentData pv bs -> IO ()
closeSkovPersistentData = closeDatabase . _db

-- |Newtype wrapper that provides an implementation of the TreeStateMonad using a persistent tree state.
-- The underlying Monad must provide instances for:
--
-- * `BlockStateTypes`
-- * `BlockStateQuery`
-- * `BlockStateOperations`
-- * `BlockStateStorage`
-- * `MonadLogger`
-- * `BirkParametersOperations`
-- * `MonadState (SkovPersistentData bs)`
-- * `PerAccountDBOperations`
--
-- This newtype establishes types for the @GlobalStateTypes@. The type variable @bs@ stands for the BlockState
-- type used in the implementation.
newtype PersistentTreeStateMonad bs m a = PersistentTreeStateMonad { runPersistentTreeStateMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, BlockStateTypes, MonadLogger, MonadError e,
            BlockStateQuery, AccountOperations, BlockStateOperations, BlockStateStorage,
            ContractStateOperations, ModuleQuery)

deriving instance (MonadProtocolVersion m) => MonadProtocolVersion (PersistentTreeStateMonad bs m)

deriving instance (Monad m, MonadState (SkovPersistentData pv bs) m)
         => MonadState (SkovPersistentData pv bs) (PersistentTreeStateMonad bs m)

instance (IsProtocolVersion pv, pv ~ MPV m) => GlobalStateTypes (PersistentTreeStateMonad bs m) where
    type BlockPointerType (PersistentTreeStateMonad bs m) = PersistentBlockPointer (MPV m) bs

getWeakPointer :: (MonadLogger (PersistentTreeStateMonad bs m),
                  MonadIO (PersistentTreeStateMonad bs m),
                   BlockStateStorage (PersistentTreeStateMonad bs m),
                   BlockState (PersistentTreeStateMonad bs m) ~ bs,
                   MonadState (SkovPersistentData (MPV m) bs) (PersistentTreeStateMonad bs m),
                   MonadProtocolVersion m)
               => Weak (PersistentBlockPointer (MPV m) bs)
               -> BlockHash
               -> String
               -> PersistentTreeStateMonad bs m (PersistentBlockPointer (MPV m) bs)
getWeakPointer weakPtr ptrHash name = do
        d <- liftIO $ deRefWeak weakPtr
        case d of
          Just v -> return v
          Nothing -> do
            lf <- use lastFinalized
            if ptrHash == getHash lf then
              return lf
            else
              -- Weak pointers are used for parent pointers. A block that is alive should always have
              -- a parent that is also alive, which means either actually in memory `BlockAlive` or already
              -- finalized. If we fail to dereference the weak pointer we should thus be able to directly look
              -- up the block from the block table.
              use (blockTable . liveMap . at' ptrHash) >>=
                 \case Just (BlockAlive bp) -> return bp
                       Nothing -> do
                         nb <- readBlock ptrHash
                         case nb of
                           Just sb -> constructBlock sb
                           Nothing -> do
                             logErrorAndThrowTS ("Could not retrieve " ++ name ++ " block even though it is meant to be finalized. Block hash: " ++ show ptrHash)
                       other ->
                         logErrorAndThrowTS ("Could not retrieve " ++ name ++ " block. Block hash: " ++ show ptrHash ++ ", block status " ++ show other)

instance (MonadLogger (PersistentTreeStateMonad bs m),
          Monad (PersistentTreeStateMonad bs m),
          MonadIO (PersistentTreeStateMonad bs m),
          TS.BlockState m ~ bs,
          BlockStateStorage (PersistentTreeStateMonad bs m),
          MonadState (SkovPersistentData (MPV m) bs) (PersistentTreeStateMonad bs m),
          MonadProtocolVersion m)
         => BlockPointerMonad (PersistentTreeStateMonad bs m) where
  blockState = return . _bpState
  bpParent block = case _bpBlock block of
      GenesisBlock{} -> return block
      NormalBlock bb -> getWeakPointer (_bpParent block) (blockPointer bb) "parent"
  bpLastFinalized block = getWeakPointer (_bpLastFinalized block) (_bpLastFinalizedHash (_bpInfo block)) "last finalized"

constructBlock :: (IsProtocolVersion pv,
                   MonadIO m,
                   BlockStateStorage m,
                   TS.BlockState m ~ bs)
               => StoredBlock pv (TS.BlockStatePointer bs) -> m (PersistentBlockPointer pv bs)
constructBlock StoredBlock{..} = do
  bstate <- loadBlockState (blockStateHash sbBlock) sbState
  makeBlockPointerFromPersistentBlock sbBlock bstate sbInfo

instance (MonadLogger (PersistentTreeStateMonad bs m),
          MonadIO (PersistentTreeStateMonad bs m),
          BlockState (PersistentTreeStateMonad bs m) ~ bs,
          BlockStateStorage (PersistentTreeStateMonad bs m),
          MonadState (SkovPersistentData (MPV m) bs) m,
          MonadProtocolVersion m)
         => TS.TreeStateMonad (PersistentTreeStateMonad bs m) where
    makePendingBlock key slot parent bid pf n lastFin trs stateHash transactionOutcomesHash time = do
        return $! makePendingBlock (signBlock key slot parent bid pf n lastFin trs stateHash transactionOutcomesHash) time
    getBlockStatus bh = do
      st <- use (blockTable . liveMap . at' bh)
      case st of
        Just (BlockAlive bp) -> return $ Just $ TS.BlockAlive bp
        Just (BlockPending bp) -> return $ Just $ TS.BlockPending bp
        Nothing -> do
            lf <- use lastFinalized
            if bh == bpHash lf then do
              lfr <- use lastFinalizationRecord
              return $ Just (TS.BlockFinalized lf lfr)
            else do
              gb <- use genesisBlockPointer
              if bh == bpHash gb then
                return $ Just (TS.BlockFinalized gb (FinalizationRecord 0 (bpHash gb) emptyFinalizationProof 0))
              else do
                b <- readBlock bh
                case b of
                  Nothing -> do
                    deadBlocks <- use (blockTable . deadCache)
                    return $! if memberDeadCache bh deadBlocks then Just TS.BlockDead else Nothing
                  Just sb -> do
                    fr <- readFinalizationRecord (sbFinalizationIndex sb)
                    case fr of
                      Just finr -> do
                        block <- constructBlock sb
                        return $ Just (TS.BlockFinalized block finr)
                      Nothing -> logErrorAndThrowTS $ "Lost finalization record that was stored" ++ show bh

    getRecentBlockStatus bh = do
      st <- use (blockTable . liveMap . at' bh)
      case st of
        Just (BlockAlive bp) -> return $ TS.RecentBlock (TS.BlockAlive bp)
        Just (BlockPending bp) -> return $ TS.RecentBlock (TS.BlockPending bp)
        Nothing -> do
            lf <- use lastFinalized
            if bh == bpHash lf then do
              lfr <- use lastFinalizationRecord
              return $ TS.RecentBlock (TS.BlockFinalized lf lfr)
            else do
              b <- memberBlockStore bh
              if b then
                return TS.OldFinalized
              else do
                  deadBlocks <- use (blockTable . deadCache)
                  return $! if memberDeadCache bh deadBlocks then TS.RecentBlock TS.BlockDead else TS.Unknown

    makeLiveBlock block parent lastFin st arrTime energy = do
            blockP <- makePersistentBlockPointerFromPendingBlock block parent lastFin st arrTime energy
            blockTable . liveMap . at' (getHash block) ?=! BlockAlive blockP
            return blockP
    markDead bh = do
            blockTable . liveMap . at' bh .=! Nothing
            blockTable . deadCache %=! insertDeadCache bh
    type MarkFin (PersistentTreeStateMonad bs m) = Maybe (StoredBlock (MPV m) (BlockStatePointer bs))
    markFinalized bh fr = use (blockTable . liveMap . at' bh) >>= \case
            Just (BlockAlive bp) -> do
              st <- saveBlockState (_bpState bp)
              -- NB: Removing the block from the in-memory cache only makes
              -- sense if no block lookups are done between the call to this
              -- function and 'wrapUpFinalization'. This is currently the case,
              -- and must remain the case in the future. That is, finalization
              -- must remain atomic, or this handling, and wrapUpFinalization
              -- must be changed.
              blockTable . liveMap . at' bh .=! Nothing
              return $ Just StoredBlock{
                  sbFinalizationIndex = finalizationIndex fr,
                  sbInfo = _bpInfo bp,
                  sbBlock = _bpBlock bp,
                  sbState = st
                }
            _ -> return Nothing
    markPending pb = blockTable . liveMap . at' (getHash pb) ?=! BlockPending pb
    
    getGenesisBlockPointer = use genesisBlockPointer
    getGenesisData = use genesisData
    getLastFinalized = do
      lf <- use lastFinalized
      lfr <- use lastFinalizationRecord
      return (lf, lfr)
    getLastFinalizedSlot = blockSlot <$> use lastFinalized
    getLastFinalizedHeight = bpHeight <$> use lastFinalized
    getNextFinalizationIndex = (+1) . finalizationIndex <$!> use lastFinalizationRecord
    addFinalization newFinBlock finRec = do
      lastFinalized .=! newFinBlock
      lastFinalizationRecord .=! finRec
    getFinalizedAtIndex finIndex = do
      lfr <- use lastFinalizationRecord
      if finIndex == finalizationIndex lfr then do
        preuse lastFinalized
      else do
        dfr <- readFinalizationRecord finIndex
        case dfr of
          Just diskFinRec -> do
             msb <- readBlock (finalizationBlockPointer diskFinRec)
             mapM constructBlock msb
          _ -> return Nothing

    getRecordAtIndex finIndex = do
      lfr <- use lastFinalizationRecord
      if finIndex == finalizationIndex lfr then
        return (Just lfr)
      else
        readFinalizationRecord finIndex

    getFinalizedAtHeight bHeight = do
      lfin <- use lastFinalized
      if bHeight == bpHeight lfin then do
        return $ Just lfin
      else do
        msb <- readFinalizedBlockAtHeight bHeight
        mapM constructBlock msb

    wrapupFinalization = writeFinalizationComposite

    getBranches = use branches
    putBranches brs = branches .= brs
    takePendingChildren bh = possiblyPendingTable . at' bh . non [] <<.= []
    addPendingBlock pb = do
        let parent = blockPointer (bbFields (pbBlock pb))
        possiblyPendingTable . at' parent . non [] %= (pb:)
        possiblyPendingQueue %= MPQ.insert (blockSlot (pbBlock pb)) (getHash pb, parent)
    takeNextPendingUntil slot = tnpu =<< use possiblyPendingQueue
        where
            tnpu ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, (pbh, parenth)), ppq') ->
                    if sl <= slot then do
                        (myPB, otherPBs) <- partition ((== pbh) . pbHash) <$> use (possiblyPendingTable . at' parenth . non [])
                        case myPB of
                            [] -> tnpu ppq'
                            (realPB : _) -> do
                                possiblyPendingTable . at' parenth . non [] .= otherPBs
                                possiblyPendingQueue .= ppq'
                                return (Just realPB)
                    else do
                        possiblyPendingQueue .= ppq
                        return Nothing
                Nothing -> do
                    possiblyPendingQueue .= ppq
                    return Nothing

    getFocusBlock = use focusBlock
    putFocusBlock bb = focusBlock .= bb
    getPendingTransactions = use pendingTransactions
    putPendingTransactions pts = pendingTransactions .= pts

    getAccountNonFinalized addr nnce = do
            use (transactionTable . ttNonFinalizedTransactions . at' addr) >>= \case
              Nothing -> return []
              Just anfts ->
                  let (_, atnnce, beyond) = Map.splitLookup nnce (anfts ^. anftMap)
                  in return $ case atnnce of
                      Nothing -> Map.toAscList beyond
                      Just s -> (nnce, s) : Map.toAscList beyond

    getNextAccountNonce addr =
        use (transactionTable . ttNonFinalizedTransactions . at' addr) >>= \case
                Nothing -> return (minNonce, True)
                Just anfts ->
                  case Map.lookupMax (anfts ^. anftMap) of
                    Nothing -> return (anfts ^. anftNextNonce, True)
                    Just (nonce, _) -> return (nonce + 1, False)

    -- only looking up the cached part is OK because the precondition of this method is that the
    -- transaction is not yet finalized
    getCredential txHash = do
      preuse (transactionTable . ttHashMap . ix txHash) >>= \case
        Just (WithMetadata{wmdData=CredentialDeployment{..},..}, status) ->
          case status of
            Received _ verRes -> return $ Just (WithMetadata{wmdData=biCred,..}, verRes)
            Committed _ verRes _ -> return $ Just (WithMetadata{wmdData=biCred,..}, verRes)
            _ -> return Nothing
        _ -> return Nothing

    getNonFinalizedChainUpdates uty sn = do
      use (transactionTable . ttNonFinalizedChainUpdates . at' uty) >>= \case
        Nothing -> return []
        Just nfcus ->
          let (_, atsn, beyond) = Map.splitLookup sn (nfcus ^. nfcuMap)
          in return $ case atsn of
            Nothing -> Map.toAscList beyond
            Just s ->
              let first = (sn, s)
                  rest = Map.toAscList beyond
              in first : rest

    addCommitTransaction bi@WithMetadata{..} verResCtx ts slot = do
      let trHash = wmdHash
      tt <- use transactionTable
      -- check if the transaction is in the transaction table cache
      case tt ^? ttHashMap . ix trHash of
          Nothing -> do
            -- If the transaction was not present in the `TransactionTable` then we verify it now
            -- adding it based on the verification result.
            -- Only transactions which can possible be valid at the stage of execution are being added
            -- to the `TransactionTable`.
            -- Verifying the transaction here as opposed to `doReceiveTransactionInternal` avoids
            -- verifying a transaction which have been both received individually and as part of a block twice.
            verRes <- TS.runTransactionVerifierT (TVer.verify ts bi) verResCtx
            if TVer.definitelyNotValid verRes (verResCtx ^. TS.isTransactionFromBlock) then return $ TS.NotAdded verRes
            else
              case wmdData of
                NormalTransaction tr -> do
                  let sender = accountAddressEmbed (transactionSender tr)
                      nonce = transactionNonce tr
                  if (tt ^. ttNonFinalizedTransactions . at' sender . non emptyANFT . anftNextNonce) <= nonce then do
                    transactionTablePurgeCounter += 1
                    let wmdtr = WithMetadata{wmdData=tr,..}
                    transactionTable .= (tt & (ttNonFinalizedTransactions . at' sender . non emptyANFT . anftMap . at' nonce . non Map.empty . at' wmdtr ?~ verRes)
                                            & (ttHashMap . at' trHash ?~ (bi, Received slot verRes)))
                    return (TS.Added bi verRes)
                  else return TS.ObsoleteNonce
                CredentialDeployment{} -> do
                  -- because we do not have nonce tracking for these transactions we need to check that
                  -- this transaction does not already exist in the on-disk storage.
                  finalizedP <- memberTransactionTable trHash
                  if finalizedP then
                    return TS.ObsoleteNonce
                  else do
                    transactionTable . ttHashMap . at' trHash ?= (bi, Received slot verRes)
                    return (TS.Added bi verRes)
                ChainUpdate cu -> do
                  let uty = updateType (uiPayload cu)
                      sn = updateSeqNumber (uiHeader cu)
                  if (tt ^. ttNonFinalizedChainUpdates . at' uty . non emptyNFCU . nfcuNextSequenceNumber) <= sn then do
                    transactionTablePurgeCounter += 1
                    let wmdcu = WithMetadata{wmdData=cu,..}
                    transactionTable .= (tt
                        & (ttNonFinalizedChainUpdates . at' uty . non emptyNFCU . nfcuMap . at' sn . non Map.empty . at' wmdcu ?~ verRes)
                        & (ttHashMap . at' trHash ?~ (bi, Received slot verRes)))
                    return (TS.Added bi verRes)
                  else return TS.ObsoleteNonce
          Just (bi', results) -> do
            -- The `Finalized` case is not reachable as the cause would be that a finalized transaction
            -- is also part of a later block which would be rejected when executing the block.
            let mVerRes = case results of
                 Received _ verRes -> Just verRes
                 Committed _ verRes _ -> Just verRes
                 Finalized {} -> Nothing
            -- if it is we update the maximum committed slot,
            -- unless the transaction is already finalized (this case is handled by updateSlot)
            -- In the current model this latter case should not happen; once a transaction is finalized
            -- it is written to disk (see finalizeTransactions below)
            when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at' trHash . mapped . _2 %= updateSlot slot
            return $ TS.Duplicate bi' mVerRes

    type FinTrans (PersistentTreeStateMonad bs m) = [(TransactionHash, FinalizedTransactionStatus)]
    finalizeTransactions bh slot txs = mapM finTrans txs
        where
            finTrans WithMetadata{wmdData=NormalTransaction tr,..} = do
                let nonce = transactionNonce tr
                    sender = accountAddressEmbed (transactionSender tr)
                anft <- use (transactionTable . ttNonFinalizedTransactions . at' sender . non emptyANFT)
                if anft ^. anftNextNonce == nonce
                then do
                    let nfn = anft ^. anftMap . at' nonce . non Map.empty
                        wmdtr = WithMetadata{wmdData=tr,..}
                    if Map.member wmdtr nfn
                    then do
                        -- Remove any other transactions with this nonce from the transaction table.
                        -- They can never be part of any other block after this point.
                        forM_ (Map.keys (Map.delete wmdtr nfn)) $
                          \deadTransaction -> transactionTable . ttHashMap . at' (getHash deadTransaction) .= Nothing
                        -- Mark the status of the transaction as finalized, and remove the data from the in-memory table.
                        ss <- deleteAndFinalizeStatus wmdHash
                        -- Update the non-finalized transactions for the sender
                        transactionTable . ttNonFinalizedTransactions . at' sender ?= (anft & (anftMap . at' nonce .~ Nothing)
                                                                                           & (anftNextNonce .~ nonce + 1))
                        return ss
                    else do
                       logErrorAndThrowTS $ "Tried to finalize transaction which is not known to be in the set of non-finalized transactions for the sender " ++ show sender
                else do
                 logErrorAndThrowTS $
                      "The recorded next nonce for the account " ++ show sender ++ " (" ++ show (anft ^. anftNextNonce) ++ ") doesn't match the one that is going to be finalized (" ++ show nonce ++ ")"
            finTrans WithMetadata{wmdData=CredentialDeployment{},..} =
                deleteAndFinalizeStatus wmdHash
            finTrans WithMetadata{wmdData=ChainUpdate cu,..} = do
                let sn = updateSeqNumber (uiHeader cu)
                    uty = updateType (uiPayload cu)
                nfcu <- use (transactionTable . ttNonFinalizedChainUpdates . at' uty . non emptyNFCU)
                unless (nfcu ^. nfcuNextSequenceNumber == sn) $ logErrorAndThrowTS $
                        "The recorded next sequence number for update type " ++ show uty ++ " (" ++ show (nfcu ^. nfcuNextSequenceNumber) ++ ") doesn't match the one that is going to be finalized (" ++ show sn ++ ")"
                let nfsn = nfcu ^. nfcuMap . at' sn . non Map.empty
                    wmdcu = WithMetadata{wmdData=cu,..}
                unless (Map.member wmdcu nfsn) $ logErrorAndThrowTS $
                        "Tried to finalize a chain update that is not known to be in the set of non-finalized chain updates of type " ++ show uty
                -- Remove any other updates with the same sequence number, since they weren't finalized
                forM_ (Map.keys (Map.delete wmdcu nfsn)) $
                  \deadUpdate -> transactionTable . ttHashMap . at' (getHash deadUpdate) .= Nothing
                -- Mark the status of the update as finalized, and remove the data from the in-memory table
                ss <- deleteAndFinalizeStatus wmdHash
                -- Update the non-finalized chain updates
                transactionTable . ttNonFinalizedChainUpdates . at' uty ?=
                  (nfcu & (nfcuMap . at' sn .~ Nothing) & (nfcuNextSequenceNumber .~ sn + 1))
                return ss


            deleteAndFinalizeStatus txHash = do
              status <- preuse (transactionTable . ttHashMap . ix txHash . _2)
              case status of
                Just Committed{..} -> do
                  -- delete the transaction from the table
                  transactionTable . ttHashMap . at' txHash .= Nothing
                  -- and write the status to disk
                  return (txHash, FinalizedTransactionStatus{ftsSlot=slot,
                                            ftsBlockHash=bh,
                                            ftsIndex=tsResults HM.! bh
                                            -- the previous lookup is safe; finalized transaction must be on a block
                                            })
                _ -> logErrorAndThrowTS "Transaction should exist and be in committed state when finalized."

    commitTransaction slot bh tr idx =
        -- add a transaction status. This only updates the cached version which is correct at' the moment
        -- because transactions are only written to disk on finalization, at' which point their
        -- statuses are no longer updated.
        transactionTable . ttHashMap . at' (getHash tr) %= fmap (_2 %~ addResult bh slot idx)


    purgeTransaction WithMetadata{..} =
        use (transactionTable . ttHashMap . at' wmdHash) >>= \case
            Nothing -> return True
            Just (_, results) -> do
                lastFinSlot <- blockSlot . _bpBlock . fst <$> TS.getLastFinalized
                if lastFinSlot >= results ^. tsSlot then do
                    -- remove from the table
                    transactionTable . ttHashMap . at' wmdHash .= Nothing
                    -- if the transaction is from a sender also delete the relevant
                    -- entry in the account non finalized table
                    case wmdData of
                      NormalTransaction tr -> do
                        let nonce = transactionNonce tr
                            sender = accountAddressEmbed (transactionSender tr)
                        transactionTable
                          . ttNonFinalizedTransactions
                          . at' sender
                          . non emptyANFT
                          . anftMap
                          . at' nonce
                          . non Map.empty %= Map.delete WithMetadata{wmdData=tr,..}
                      _ -> return () -- do nothing.
                    return True
                else return False

    markDeadTransaction bh tr =
      -- We only need to update the outcomes. The anf table nor the pending table need be updated
      -- here since a transaction should not be marked dead in a finalized block.
      transactionTable . ttHashMap . at' (getHash tr) . mapped . _2 %= markDeadResult bh
    lookupTransaction th = do
       ts <- preuse (transactionTable . ttHashMap . ix th . _2)
       case ts of
         Just t -> return $ Just t
         Nothing -> fmap finalizedToTransactionStatus <$> readTransactionStatus th

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .=! stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters

    purgeTransactionTable ignoreInsertions currentTime = do
      purgeCount <- use transactionTablePurgeCounter
      RuntimeParameters{..} <- use runtimeParameters
      when (ignoreInsertions || purgeCount > rpInsertionsBeforeTransactionPurge) $ do
        transactionTablePurgeCounter .= 0
        lastFinalizedSlot <- TS.getLastFinalizedSlot
        transactionTable' <- use transactionTable
        pendingTransactions' <- use pendingTransactions
        let
          currentTransactionTime = utcTimeToTransactionTime currentTime
          oldestArrivalTime = if currentTransactionTime > rpTransactionsKeepAliveTime
                                then currentTransactionTime - rpTransactionsKeepAliveTime
                                else 0
          currentTimestamp = utcTimeToTimestamp currentTime
          (newTT, newPT) = purgeTables lastFinalizedSlot oldestArrivalTime currentTimestamp transactionTable' pendingTransactions'
        transactionTable .= newTT
        pendingTransactions .= newPT

    clearOnProtocolUpdate = do
        -- clear the pending blocks
        possiblyPendingTable .=! HM.empty
        possiblyPendingQueue .=! MPQ.empty
        -- and non-finalized blocks
        branches .=! Seq.empty
        blockTable . liveMap .=! HM.empty
        blockTable . deadCache .=! emptyDeadCache
        -- mark all transactions that are not finalized as received since
        -- they are no longer part of any blocks.
        transactionTable
            . ttHashMap
            %=! HM.map
                ( \(bi, s) -> case s of
                    Committed{..} -> (bi, Received{..})
                    _ -> (bi, s)
                )

    clearAfterProtocolUpdate = do
        transactionTable .=! emptyTransactionTable
        pendingTransactions .=! emptyPendingTransactionTable
        nextGenesisInitialState .=! Nothing
        -- uncache any blocks that might still be cached.
        gp <- use genesisBlockPointer
        archiveBlockState (_bpState gp)
        fp <- use focusBlock
        archiveBlockState (_bpState fp)
        lf <- use lastFinalized
        archiveBlockState (_bpState lf)
        collapseCaches

    getNonFinalizedTransactionVerificationResult bi = do
      table <- use transactionTable
      return $ getNonFinalizedVerificationResult bi table

    storeFinalState bs = nextGenesisInitialState ?= bs
