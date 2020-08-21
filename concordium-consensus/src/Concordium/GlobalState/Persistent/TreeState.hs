{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}
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
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlockPointer as PB
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.PurgeTransactions
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions as T
import Concordium.Types.Updates
import Control.Exception hiding (handle, throwIO)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.List as List hiding (map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Serialize (Serialize(..))
import Lens.Micro.Platform
import Concordium.Utils
import System.Mem.Weak
import System.Directory
import System.IO.Error
import System.FilePath
import Concordium.GlobalState.SQL.AccountTransactionIndex
import Concordium.Logger
import Control.Monad.Except

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

logExceptionAndThrowTS :: (MonadLogger m, MonadIO m, Exception e) => e -> m a
logExceptionAndThrowTS = logExceptionAndThrow TreeState

logErrorAndThrowTS :: (MonadLogger m, MonadIO m) => String -> m a
logErrorAndThrowTS = logErrorAndThrow TreeState

--------------------------------------------------------------------------------

-- * Persistent version of the Skov Data

-- |BlockStatus as recorded in the persistent implementation
data PersistentBlockStatus ati bs =
    BlockAlive !(PersistentBlockPointer ati bs)
    | BlockDead
    | BlockFinalized !FinalizationIndex
    | BlockPending !PendingBlock
  deriving(Eq, Show)

-- |Skov data for the persistent tree state version that also holds the database handlers
data SkovPersistentData ati bs = SkovPersistentData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(HM.HashMap BlockHash (PersistentBlockStatus (ATIValues ati) bs)),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |Pointer to the last finalized block
    _lastFinalized :: !(PersistentBlockPointer (ATIValues ati) bs),
    -- |Pointer to the last finalization record
    _lastFinalizationRecord :: !FinalizationRecord,
    -- |Branches of the tree by height above the last finalized block
    _branches :: !(Seq.Seq [PersistentBlockPointer (ATIValues ati) bs]),
    -- |Genesis data
    _genesisData :: !GenesisData,
    -- |Block pointer to genesis block
    _genesisBlockPointer :: !(PersistentBlockPointer (ATIValues ati) bs),
    -- |Current focus block
    _focusBlock :: !(PersistentBlockPointer (ATIValues ati) bs),
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
    -- | Database handlers
    _db :: !(DatabaseHandlers (TS.BlockStatePointer bs)),
    -- |Context for the transaction log.
    _atiCtx :: !(ATIContext ati)
}
makeLenses ''SkovPersistentData

instance (bsp ~ TS.BlockStatePointer bs)
    => HasDatabaseHandlers bsp (SkovPersistentData ati bs) where
  dbHandlers = db

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovPersistentDataDefault
    :: (Serialize (TS.BlockStatePointer bs))
    => FilePath
    -> GenesisData
    -> bs
    -> (ATIValues ati, ATIContext ati)
    -> TS.BlockStatePointer bs -- ^How to serialize the block state reference for inclusion in the table.
    -> IO (SkovPersistentData ati bs)
initialSkovPersistentDataDefault dir = initialSkovPersistentData (defaultRuntimeParameters { rpTreeStateDir = dir })

initialSkovPersistentData
    :: (Serialize (TS.BlockStatePointer bs))
    => RuntimeParameters
    -> GenesisData
    -> bs
    -> (ATIValues ati, ATIContext ati)
    -> TS.BlockStatePointer bs -- ^How to serialize the block state reference for inclusion in the table.
    -> IO (SkovPersistentData ati bs)
initialSkovPersistentData rp gd genState ati serState = do
  gb <- makeGenesisPersistentBlockPointer gd genState (fst ati)
  let gbh = bpHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  initialDb <- initializeDatabase gb serState rp
  return SkovPersistentData {
            _blockTable = HM.singleton gbh (BlockFinalized 0),
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _lastFinalized = gb,
            _lastFinalizationRecord = gbfin,
            _branches = Seq.empty,
            _genesisData = gd,
            _genesisBlockPointer = gb,
            _focusBlock = gb,
            _pendingTransactions = emptyPendingTransactionTable,
            _transactionTable = emptyTransactionTable,
            _transactionTablePurgeCounter = 0,
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _db = initialDb,
            _atiCtx = snd ati
        }

--------------------------------------------------------------------------------

-- * Initialization functions

-- |Check the permissions in the required files.
checkExistingDatabase :: forall m. (MonadLogger m, MonadIO m) => RuntimeParameters -> m (FilePath, Bool)
checkExistingDatabase rp = do
  let blockStateFile = rpBlockStateFile rp <.> "dat"
      treeStateFile = rpTreeStateDir rp </> "data.mdb"
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
         return (blockStateFile, True)
     | bsPathEx -> do
         logExceptionAndThrowTS $ DatabaseInvariantViolation "Block state file exists, but tree state file does not."
     | tsPathEx -> do
         logExceptionAndThrowTS $ DatabaseInvariantViolation "Tree state file exists, but block state file does not."
     | otherwise ->
         return (blockStateFile, False)

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
--   * in the block state, an account which is listed cannot be loaded
--
-- TODO: We should probably use cursors instead of manually traversing the database.
-- however the LMDB.Simple bindings for cursors are essentially unusable, leading to
-- random segmentation faults and similar exceptions.
loadSkovPersistentData :: forall ati . CanExtend (ATIValues ati)
                       => RuntimeParameters
                       -> GenesisData
                       -> PBS.PersistentBlockStateContext
                       -> (ATIValues ati, ATIContext ati)
                       -> LogIO (SkovPersistentData ati PBS.PersistentBlockState)
loadSkovPersistentData rp _genesisData pbsc atiPair = do
  -- we open the environment first.
  -- It might be that the database is bigger than the default environment size.
  -- This seems to not be an issue while we only read from the database,
  -- and on insertions we resize the environment anyhow.
  -- But this behaviour of LMDB is poorly documented, so we might experience issues.
  _db <- either (logExceptionAndThrowTS . DatabaseOpeningError) return =<<
          liftIO (try $ databaseHandlers rp)

  -- Get the genesis block and check that its data matches the supplied genesis data.
  genStoredBlock <- maybe (logExceptionAndThrowTS GenesisBlockNotInDataBaseError) return =<<
          liftIO (getFirstBlock _db)
  _genesisBlockPointer <- liftIO $ makeBlockPointer genStoredBlock
  case _bpBlock _genesisBlockPointer of
    GenesisBlock gd' -> unless (_genesisData == gd') $ logExceptionAndThrowTS (GenesisBlockIncorrect (getHash _genesisBlockPointer))
    _ -> logExceptionAndThrowTS (DatabaseInvariantViolation "Block at height 0 is not a genesis block.")

  -- Populate the block table.
  _blockTable <- liftIO (loadBlocksFinalizationIndexes _db) >>= \case
      Left s -> logExceptionAndThrowTS $ DatabaseInvariantViolation s
      Right hm -> return $! HM.map BlockFinalized hm

  -- Get the last finalized block.
  (_lastFinalizationRecord, lfStoredBlock) <- liftIO (getLastBlock _db) >>= \case
      Left s -> logExceptionAndThrowTS $ DatabaseInvariantViolation s
      Right r -> return r
  _lastFinalized <- liftIO (makeBlockPointer lfStoredBlock)
  let lastState = _bpState _lastFinalized

  -- The final thing we need to establish is the transaction table invariants.
  -- This specifically means for each account we need to determine the next available nonce.
  -- For now we simply load all accounts, but after this table is also moved to
  -- some sort of a database we should not need to do that.

  let getTransactionTable :: PBS.PersistentBlockStateMonad PBS.PersistentBlockStateContext (ReaderT PBS.PersistentBlockStateContext LogIO) TransactionTable
      getTransactionTable = do
        accs <- getAccountList lastState
        foldM (\table addr ->
                 getAccount lastState addr >>= \case
                  Nothing -> logExceptionAndThrowTS (DatabaseInvariantViolation $ "Account " ++ show addr ++ " which is in the account list cannot be loaded.")
                  Just acc -> return (table & ttNonFinalizedTransactions . at addr ?~ emptyANFTWithNonce (acc ^. accountNonce)))
            emptyTransactionTable
            accs
  tt <- runReaderT (PBS.runPersistentBlockStateMonad getTransactionTable) pbsc

  return SkovPersistentData {
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _branches = Seq.empty,
            _focusBlock = _lastFinalized,
            _pendingTransactions = emptyPendingTransactionTable,
            _transactionTable = tt,
            _transactionTablePurgeCounter = 0,
            -- The best thing we can probably do is use the initial statistics,
            -- and make the meaning of those with respect to the last time
            -- consensus started.
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _atiCtx = snd atiPair,
            ..
        }

  where
    makeBlockPointer :: StoredBlock (TS.BlockStatePointer PBS.PersistentBlockState) -> IO (PersistentBlockPointer (ATIValues ati) PBS.PersistentBlockState)
    makeBlockPointer StoredBlock{..} = do
      bstate <- runReaderT (PBS.runPersistentBlockStateMonad (loadBlockState sbState)) pbsc
      makeBlockPointerFromPersistentBlock sbBlock bstate defaultValue sbInfo

-- |Close the database associated with a 'SkovPersistentData'.
-- The database should not be used after this.
closeSkovPersistentData :: SkovPersistentData ati bs -> IO ()
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
newtype PersistentTreeStateMonad ati bs m a = PersistentTreeStateMonad { runPersistentTreeStateMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, BlockStateTypes, MonadLogger, MonadError e,
            BlockStateQuery, AccountOperations, BakerQuery, BlockStateOperations, BlockStateStorage, BirkParametersOperations)

deriving instance (Monad m, MonadState (SkovPersistentData ati bs) m)
         => MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m)

instance (CanExtend (ATIValues ati),
          CanRecordFootprint (Footprint (ATIValues ati)))
         => ATITypes (PersistentTreeStateMonad ati bs m) where
  type ATIStorage (PersistentTreeStateMonad ati bs m) = ATIValues ati

instance (Monad m) => PerAccountDBOperations (PersistentTreeStateMonad () bs m)

instance (MonadIO m, MonadState (SkovPersistentData DiskDump bs) m) => PerAccountDBOperations (PersistentTreeStateMonad DiskDump bs m) where
  flushBlockSummaries bh ati sos = do
    PAAIConfig handle <- use logContext
    liftIO $ writeEntries handle bh ati sos

instance GlobalStateTypes (PersistentTreeStateMonad ati bs m) where
    type BlockPointerType (PersistentTreeStateMonad ati bs m) = PersistentBlockPointer (ATIValues ati) bs

instance HasLogContext PerAccountAffectIndex (SkovPersistentData DiskDump bs) where
  logContext = atiCtx

getWeakPointer :: (MonadLogger (PersistentTreeStateMonad ati bs m),
                  MonadIO (PersistentTreeStateMonad ati bs m),
                   BlockStateStorage (PersistentTreeStateMonad ati bs m),
                   BlockState (PersistentTreeStateMonad ati bs m) ~ bs,
                   CanExtend (ATIValues ati),
                   MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m))
               => Weak (PersistentBlockPointer (ATIValues ati) bs)
               -> BlockHash
               -> String
               -> PersistentTreeStateMonad ati bs m (PersistentBlockPointer (ATIValues ati) bs)
getWeakPointer weakPtr ptrHash name = do
        d <- liftIO $ deRefWeak weakPtr
        case d of
          Just v -> return v
          Nothing -> do
            lf <- use lastFinalized
            if ptrHash == getHash lf then
              return lf
            else do
              nb <- readBlock ptrHash
              case nb of
                Just sb -> constructBlock sb
                Nothing -> logErrorAndThrowTS ("Couldn't find " ++ name ++ " block in disk")

instance (MonadLogger (PersistentTreeStateMonad ati bs m),
          Monad (PersistentTreeStateMonad ati bs m),
          MonadIO (PersistentTreeStateMonad ati bs m),
          TS.BlockState m ~ bs,
          BlockStateStorage (PersistentTreeStateMonad ati bs m),
          MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m),
          CanExtend (ATIValues ati),
          CanRecordFootprint (Footprint (ATIValues ati)))
         => BlockPointerMonad (PersistentTreeStateMonad ati bs m) where
  blockState = return . _bpState
  bpParent block = case _bpBlock block of
      GenesisBlock{} -> return block
      NormalBlock bb -> getWeakPointer (_bpParent block) (blockPointer bb) "parent"
  bpLastFinalized block = getWeakPointer (_bpLastFinalized block) (_bpLastFinalizedHash (_bpInfo block)) "last finalized"
  bpTransactionAffectSummaries block = return (_bpATI block)

constructBlock :: (MonadIO m,
                   BlockStateStorage m,
                   TS.BlockState m ~ bs,
                   CanExtend (ATIStorage m))
               => StoredBlock (TS.BlockStatePointer bs) -> m (PersistentBlockPointer (ATIStorage m) bs)
constructBlock StoredBlock{..} = do
  bstate <- loadBlockState sbState
  makeBlockPointerFromPersistentBlock sbBlock bstate defaultValue sbInfo

instance (MonadLogger (PersistentTreeStateMonad ati bs m),
          MonadIO (PersistentTreeStateMonad ati bs m),
          BlockState (PersistentTreeStateMonad ati bs m) ~ bs,
          BlockStateStorage (PersistentTreeStateMonad ati bs m),
          PerAccountDBOperations (PersistentTreeStateMonad ati bs m),
          MonadState (SkovPersistentData ati bs) m)
         => TS.TreeStateMonad (PersistentTreeStateMonad ati bs m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = do
        return $! makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    getBlockStatus bh = do
      st <- use (blockTable . at' bh)
      case st of
        Just (BlockAlive bp) -> return $ Just $ TS.BlockAlive bp
        Just (BlockPending bp) -> return $ Just $ TS.BlockPending bp
        Just BlockDead -> return $ Just TS.BlockDead
        Just (BlockFinalized fidx) -> getFinalizedBlockAndFR fidx
        _ -> return Nothing
     where getFinalizedBlockAndFR fidx = do
            gb <- use genesisBlockPointer
            if bh == bpHash gb then
              return $ Just (TS.BlockFinalized gb (FinalizationRecord 0 (bpHash gb) emptyFinalizationProof 0))
            else do
              lf <- use lastFinalized
              if bh == bpHash lf then do
                lfr <- use lastFinalizationRecord
                return $ Just (TS.BlockFinalized lf lfr)
              else do
                b <- readBlock bh
                fr <- readFinalizationRecord fidx
                case (b, fr) of
                  (Just sb, Just finr) -> do
                    block <- constructBlock sb
                    return $ Just (TS.BlockFinalized block finr)
                  (Just _, Nothing) -> logErrorAndThrowTS $ "Lost finalization record that was stored" ++ show bh
                  (Nothing, Just _) -> logErrorAndThrowTS $ "Lost block that was stored as finalized" ++ show bh
                  _ -> logErrorAndThrowTS $ "Lost block and finalization record" ++ show bh
    makeLiveBlock block parent lastFin st ati arrTime energy = do
            blockP <- makePersistentBlockPointerFromPendingBlock block parent lastFin st ati arrTime energy
            blockTable . at' (getHash block) ?=! BlockAlive blockP
            return blockP
    markDead bh = blockTable . at' bh ?= BlockDead
    markFinalized bh fr = use (blockTable . at' bh) >>= \case
            Just (BlockAlive bp) -> do
              st <- saveBlockState (_bpState bp)
              writeBlock StoredBlock{
                  sbFinalizationIndex = finalizationIndex fr,
                  sbInfo = _bpInfo bp,
                  sbBlock = _bpBlock bp,
                  sbState = st
                }
              blockTable . at' bh ?=! BlockFinalized (finalizationIndex fr)
            _ -> return ()
    markPending pb = blockTable . at' (getHash pb) ?=! BlockPending pb
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
      writeFinalizationRecord finRec
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

    getAccountNonFinalized addr nnce =
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
    getCredential txHash =
      preuse (transactionTable . ttHashMap . ix txHash) >>= \case
        Just (WithMetadata{wmdData=CredentialDeployment{..},..}, _) -> return $ Just WithMetadata{wmdData=biCred,..}
        _ -> return Nothing

    getNonFinalizedChainUpdates uty sn =
      use (transactionTable . ttNonFinalizedChainUpdates . at' uty) >>= \case
        Nothing -> return []
        Just nfcus ->
          let (_, atsn, beyond) = Map.splitLookup sn (nfcus ^. nfcuMap)
          in return $ case atsn of
            Nothing -> Map.toAscList beyond
            Just s -> (sn, s) : Map.toAscList beyond

    addCommitTransaction bi@WithMetadata{..} slot = do
      let trHash = wmdHash
      tt <- use transactionTable
      -- check if the transaction is in the transaction table cache
      case tt ^? ttHashMap . ix trHash of
          Nothing -> do
            case wmdData of
              NormalTransaction tr -> do
                let sender = transactionSender tr
                    nonce = transactionNonce tr
                if (tt ^. ttNonFinalizedTransactions . at' sender . non emptyANFT . anftNextNonce) <= nonce then do
                  transactionTablePurgeCounter += 1
                  let wmdtr = WithMetadata{wmdData=tr,..}
                  transactionTable .= (tt & (ttNonFinalizedTransactions . at' sender . non emptyANFT . anftMap . at' nonce . non Set.empty %~ Set.insert wmdtr)
                                          & (ttHashMap . at' trHash ?~ (bi, Received slot)))
                  return (TS.Added bi)
                else return TS.ObsoleteNonce
              CredentialDeployment{..} -> do
                -- because we do not have nonce tracking for these transactions we need to check that
                -- this transction does not already exist in the on-disk storage.
                finalizedP <- memberTransactionTable trHash
                if finalizedP then
                  return TS.ObsoleteNonce
                else do
                  transactionTable . ttHashMap . at' trHash ?= (bi, Received slot)
                  return (TS.Added bi)
              ChainUpdate cu -> do
                let uty = updateType (uiPayload cu)
                    sn = updateSeqNumber (uiHeader cu)
                if (tt ^. ttNonFinalizedChainUpdates . at' uty . non emptyNFCU . nfcuNextSequenceNumber) <= sn then do
                  transactionTablePurgeCounter += 1
                  let wmdcu = WithMetadata{wmdData=cu,..}
                  transactionTable .= (tt
                      & (ttNonFinalizedChainUpdates . at' uty . non emptyNFCU . nfcuMap . at' sn . non Set.empty %~ Set.insert wmdcu)
                      & (ttHashMap . at' trHash ?~ (bi, Received slot)))
                  return (TS.Added bi)
                else return TS.ObsoleteNonce
          Just (bi', results) -> do
            -- if it is we update the maximum committed slot,
            -- unless the transaction is already finalized (this case is handled by updateSlot)
            -- In the current model this latter case should not happen; once a transaction is finalized
            -- it is written to disk (see finalizeTransactions below)
            when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at' trHash . mapped . _2 %= updateSlot slot
            return $ TS.Duplicate bi'

    finalizeTransactions bh slot txs = mapM finTrans txs >>= writeTransactionStatuses
        where
            finTrans WithMetadata{wmdData=NormalTransaction tr,..} = do
                let nonce = transactionNonce tr
                    sender = transactionSender tr
                anft <- use (transactionTable . ttNonFinalizedTransactions . at' sender . non emptyANFT)
                if anft ^. anftNextNonce == nonce
                then do
                    let nfn = anft ^. anftMap . at' nonce . non Set.empty
                        wmdtr = WithMetadata{wmdData=tr,..}
                    if Set.member wmdtr nfn
                    then do
                        -- Remove any other transactions with this nonce from the transaction table.
                        -- They can never be part of any other block after this point.
                        forM_ (Set.delete wmdtr nfn) $
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
            finTrans WithMetadata{wmdData=CredentialDeployment{..},..} = deleteAndFinalizeStatus wmdHash
            finTrans WithMetadata{wmdData=ChainUpdate cu,..} = do
                let sn = updateSeqNumber (uiHeader cu)
                    uty = updateType (uiPayload cu)
                nfcu <- use (transactionTable . ttNonFinalizedChainUpdates . at' uty . non emptyNFCU)
                unless (nfcu ^. nfcuNextSequenceNumber == sn) $ logErrorAndThrowTS $
                        "The recorded next sequence number for update type " ++ show uty ++ " (" ++ show (nfcu ^. nfcuNextSequenceNumber) ++ ") doesn't match the one that is going to be finalzied (" ++ show sn ++ ")"
                let nfsn = nfcu ^. nfcuMap . at' sn . non Set.empty
                    wmdcu = WithMetadata{wmdData=cu,..}
                unless (Set.member wmdcu nfsn) $ logErrorAndThrowTS $
                        "Tried to finalized a chain update that is not known to be in the set of non-finalized chain updates of type " ++ show uty
                -- Remove any other updates with the same sequence number, since they weren't finalized
                forM_ (Set.delete wmdcu nfsn) $
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
                  -- delete the transaction from the cache
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
                            sender = transactionSender tr
                        transactionTable
                          . ttNonFinalizedTransactions
                          . at' sender
                          . non emptyANFT
                          . anftMap
                          . at' nonce
                          . non Set.empty %= Set.delete WithMetadata{wmdData=tr,..}
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
