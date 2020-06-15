{-# LANGUAGE ConstraintKinds, BangPatterns, TypeFamilies, TemplateHaskell,
             NumericUnderscores, ScopedTypeVariables, DataKinds, RecordWildCards,
             MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving,
             LambdaCase, FlexibleContexts, DerivingStrategies, DerivingVia,
             StandaloneDeriving, UndecidableInstances, TypeApplications, MultiWayIf #-}
-- |This module provides a monad that is an instance of both `LMDBStoreMonad`, `LMDBQueryMonad`,
-- and `TreeStateMonad` effectively adding persistence to the tree state.
--
-- In this module we also implement the instances and functions that require a monadic context, such as the conversions.
module Concordium.GlobalState.Persistent.TreeState where

import Concordium.GlobalState.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlockPointer as PB
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.TransactionTable
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions as T
import Control.Exception hiding (handle)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.HashMap.Strict hiding (toList)
import qualified Data.HashMap.Strict as HM
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Typeable
import Data.Maybe
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import Data.Serialize as S (runGet, runGetPartial, Result(..), put, get, runPut, Put)
import qualified Data.Set as Set
import Database.LMDB.Simple as L
import Lens.Micro.Platform
import Concordium.Utils
import System.Mem.Weak
import System.Directory
import System.IO.Error
import System.FilePath
import Concordium.GlobalState.SQL.AccountTransactionIndex
import Data.Time.Clock
import Data.Foldable as Fold (foldl')

-- * SkovPersistentData definition

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
    _db :: !DatabaseHandlers,
    -- |Context for the transaction log.
    _atiCtx :: !(ATIContext ati)
}
makeLenses ''SkovPersistentData

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovPersistentDataDefault
    :: FilePath
    -> GenesisData
    -> bs
    -> (ATIValues ati, ATIContext ati)
    -> S.Put -- ^How to serialize the block state reference for inclusion in the table.
    -> IO (SkovPersistentData ati bs)
initialSkovPersistentDataDefault dir = initialSkovPersistentData (defaultRuntimeParameters { rpTreeStateDir = dir })

initialSkovPersistentData
    :: RuntimeParameters
    -> GenesisData
    -> bs
    -> (ATIValues ati, ATIContext ati)
    -> S.Put -- ^How to serialize the block state reference for inclusion in the table.
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

checkExistingDatabase :: RuntimeParameters -> IO (FilePath, Bool)
checkExistingDatabase rp = do
  let blockStateFile = rpBlockStateFile rp <.> "dat"
      treeStateFile = rpTreeStateDir rp </> "data.mdb"
  bsPathEx <- doesPathExist blockStateFile
  tsPathEx <- doesPathExist treeStateFile

  -- Check whether a path is a normal file that is readable and writable
  let checkRWFile path exc = do
        fileEx <- doesFileExist path
        unless fileEx $ throwIO BlockStatePathDir
        perms <- catchJust (guard . isPermissionError)
                           (getPermissions path)
                           (const (throwIO exc))
        unless (readable perms && writable perms) $ throwIO exc

  -- if both files exist we check whether they are both readable and writable.
  -- In case only one of them exists we raise an appropriate exception. We don't want to delete any data.
  if | bsPathEx && tsPathEx -> do
         -- check whether it is a normal file and whether we have the right permissions
         checkRWFile blockStateFile BlockStatePermissionError
         checkRWFile treeStateFile TreeStatePermissionError
         return (blockStateFile, True)
     | bsPathEx ->
         throwIO (DatabaseInvariantViolation "Block state file exists, but tree state file does not.")
     | tsPathEx ->
         throwIO (DatabaseInvariantViolation "Tree state file exists, but block state file does not.")
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
                       -> IO (SkovPersistentData ati PBS.PersistentBlockState)
loadSkovPersistentData rp gd pbsc atiPair = do
  -- we open the environment first.
  -- It might be that the database is bigger than the default environment size.
  -- This seems to not be an issue while we only read from the database,
  -- and on insertions we resize the environment anyhow.
  -- But this behaviour of LMDB is poorly documented, so we might experience issues.

  dbHandlers <- mapException DatabaseOpeningError $ databaseHandlers rp

  let env = dbHandlers ^. storeEnv -- database environment
      dbFH = dbHandlers  ^. finalizedByHeightStore -- finalized by height table
      dbB = dbHandlers ^. blockStore
      dbFinRecords = dbHandlers ^. finalizationRecordStore

  let failWith :: IO (Maybe a) -> InitException -> IO a
      failWith x exc =
        x >>= \case Nothing -> throwIO exc
                    Just v -> return v

      -- Lookup a finalized block at a given height.
      -- Raises an exception at internal database violation.
      lookupAtHeight :: BlockHeight -> IO (Maybe (BlockHash, ByteString))
      lookupAtHeight bHeight = do
        result <-
          transaction env $ do
            (L.get dbFH bHeight :: (L.Transaction ReadOnly (Maybe BlockHash))) >>= \case
              Nothing -> return Nothing
              Just bh -> Just . (bh, ) <$> L.get dbB bh
        -- the reason for doing it this way is that I am wary of throwing exceptions
        -- inside an LMDB transaction.
        case result of
          Nothing -> return Nothing
          Just (bh, Nothing) -> throwIO (DatabaseInvariantViolation $ "A referred to block does not exist: " ++ show bh)
          Just (bh, Just bytes) -> return (Just (bh, bytes))

      getBlockPointer :: ByteString -> IO (PersistentBlockPointer (ATIValues ati) PBS.PersistentBlockState, FinalizationIndex, PBS.PersistentBlockState)
      getBlockPointer bytes =
        case runGet getQuadruple bytes of
          Left _ -> throwIO (DatabaseInvariantViolation "Cannot deserialize block.")
          Right (finIndex, blockInfo, newBlock, state') -> do
            st <- runReaderT (PBS.runPersistentBlockStateMonad state') pbsc
            let ati = defaultValue :: ATIValues ati
            (, finIndex, st) <$> makeBlockPointerFromPersistentBlock newBlock st ati blockInfo
        where getQuadruple = do
                finIndex <- S.get
                blockInfo <- S.get
                newBlock <- getBlock (utcTimeToTransactionTime (_bpReceiveTime blockInfo))
                state' <- BS.getBlockState
                return (finIndex, blockInfo, newBlock, state')

      -- Look up the minimal data, without deserializing the rest of it.
      -- Due to the way LMDB works right now this still involves loading the entire block
      -- into memory.
      getMinimalBlockData :: ByteString -> IO FinalizationIndex
      getMinimalBlockData bytes =
        case runGetPartial S.get bytes of
          S.Fail err _ -> throwIO (DatabaseInvariantViolation $ "Cannot deserialize block: " ++ show err)
          S.Partial _ -> throwIO (DatabaseInvariantViolation $ "Cannot deserialize block. Partially successful.")
          S.Done finIndex _ -> return finIndex

      -- Given the genesis block pointer this function tries to load as many blocks as possible
      -- from disk. It returns the last blockpointer that was loaded.
      loadInSequence ::
        PersistentBlockPointer (ATIValues ati) PBS.PersistentBlockState -- Genesis block pointer
        -> ByteString -- Bytes of the genesis block as stored in the database.
        -> IO ((PersistentBlockPointer (ATIValues ati) PBS.PersistentBlockState, FinalizationIndex, PBS.PersistentBlockState),
               [(BlockHash, PersistentBlockStatus (ATIValues ati) PBS.PersistentBlockState)])
      loadInSequence gbp gBytes = do
        (lastBytes, blocks) <- go 1 gBytes [(getHash gbp, BlockFinalized 0)]
        (, blocks) <$> getBlockPointer lastBytes
        where go :: BlockHeight
                 -> ByteString
                 -> [(BlockHash, PersistentBlockStatus (ATIValues ati) PBS.PersistentBlockState)]
                 -> IO (ByteString, [(BlockHash, PersistentBlockStatus (ATIValues ati) PBS.PersistentBlockState)])
              go height lastBytes acc =
                lookupAtHeight height >>= \case
                  Nothing -> return (lastBytes, acc)
                  Just (bh, bytes) ->
                    getMinimalBlockData bytes >>=
                    \finIndex -> go (height + 1) bytes ((bh, BlockFinalized finIndex):acc)

  (gbHash, gbBytes) <- lookupAtHeight 0 `failWith` GenesisBlockNotInDataBaseError

  -- Check that this is really a genesis pointer with the same genesis data
  (gBlockPointer, _, _) <- getBlockPointer gbBytes

  unless (gbHash == getHash gBlockPointer) $ throwIO (DatabaseInvariantViolation $ "Genesis given hash and computed hash differ.")

  case _bpBlock gBlockPointer of
    GenesisBlock gd' -> unless (gd == gd') $ throwIO (GenesisBlockIncorrect (getHash gBlockPointer))
    _ -> throwIO (DatabaseInvariantViolation "Block at height 0 is not a genesis block.")

  -- We would ideally be able to simply query for the last finalization record, but the limitations
  -- of the LMDB bindings prevent us from doing that.
  ((lastPointer, lastBlockFinIndex, lastState), blocks) <- loadInSequence gBlockPointer gbBytes

  let getFinalizationRecord idx = L.readOnlyTransaction env (L.get dbFinRecords idx)
  lastFinRecord <- getFinalizationRecord lastBlockFinIndex `failWith` (DatabaseInvariantViolation "Finalization record for known index does not exist.")

  -- make sure the block pointed to by the last finalization record is indeed in the database
  _ <- readOnlyTransaction env (L.get dbB (finalizationBlockPointer lastFinRecord))
       `failWith` (DatabaseInvariantViolation "Finalization record points to a block which does not exist.")

  -- The final thing we need to establish is the transaction table invariants.
  -- This specifically means for each account we need to determine the next available nonce.
  -- For now we simply load all accounts, but after this table is also moved to
  -- some sort of a database we should not need to do that.

  let getTransactionTable :: PBS.PersistentBlockStateMonad PBS.PersistentBlockStateContext (ReaderT PBS.PersistentBlockStateContext IO) TransactionTable
      getTransactionTable = do
        accs <- BS.getAccountList lastState
        foldM (\table addr ->
                 BS.getAccount lastState addr >>= \case
                  Nothing -> liftIO (throwIO (DatabaseInvariantViolation $ "Account " ++ show addr ++ " which is in the account list cannot be loaded."))
                  Just acc -> return (table & ttNonFinalizedTransactions . at addr ?~ emptyANFTWithNonce (acc ^. accountNonce)))
            emptyTransactionTable
            accs
  tt <- runReaderT (PBS.runPersistentBlockStateMonad getTransactionTable) pbsc

  return SkovPersistentData {
            _blockTable = HM.fromList blocks,
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _lastFinalized = lastPointer,
            _lastFinalizationRecord = lastFinRecord,
            _branches = Seq.empty,
            _genesisData = gd,
            _genesisBlockPointer = gBlockPointer,
            _focusBlock = lastPointer,
            _pendingTransactions = emptyPendingTransactionTable,
            _transactionTable = tt,
            _transactionTablePurgeCounter = 0,
            -- The best thing we can probably do is use the initial statistics,
            -- and make the meaning of those with respect to the last time
            -- consensus started.
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _db = dbHandlers,
            _atiCtx = snd atiPair
        }

-- |Newtype wrapper that provides an implementation of the TreeStateMonad using a persistent tree state.
-- The underlying Monad must provide instances for:
--
-- * `BlockStateTypes`
-- * `BlockStateQuery`
-- * `BlockStateOperations`
-- * `BlockStateStorage`
-- * `MonadState (SkovPersistentData bs)`
-- * `PerAccountDBOperations`
--
-- This newtype establishes types for the @GlobalStateTypes@. The type variable @bs@ stands for the BlockState
-- type used in the implementation.
newtype PersistentTreeStateMonad ati bs m a = PersistentTreeStateMonad { runPersistentTreeStateMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, GS.BlockStateTypes,
            BS.BlockStateQuery, BS.BlockStateOperations, BS.BlockStateStorage, BS.BirkParametersOperations)

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

getWeakPointer :: (MonadIO (PersistentTreeStateMonad ati bs m),
                   BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
                   GS.BlockState (PersistentTreeStateMonad ati bs m) ~ bs,
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
              return $ fromMaybe (error ("Couldn't find " ++ name ++ " block in disk")) nb

instance (Monad (PersistentTreeStateMonad ati bs m),
          MonadIO (PersistentTreeStateMonad ati bs m),
          TS.BlockState m ~ bs,
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
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


-- |Construct a block from a serialized form.
-- The @ati@ is filled with a default value.
constructBlock :: (MonadIO m,
                   BS.BlockStateStorage m,
                   TS.BlockState m ~ bs,
                   CanExtend (ATIStorage m))
               => Maybe ByteString -> m (Maybe (PersistentBlockPointer (ATIStorage m) bs))
constructBlock Nothing = return Nothing
constructBlock (Just bytes) =
  case runGet getTriple bytes of
    Left _ -> return Nothing
    Right (blockInfo, newBlock, state') -> do
      st <- state'
      let ati = defaultValue
      Just <$> (makeBlockPointerFromPersistentBlock newBlock st ati blockInfo)
  where getTriple = do
          (_ :: FinalizationIndex) <- S.get -- TODO: This is ugly, but needed when loading from existing database.
          blockInfo <- S.get
          newBlock <- getBlock (utcTimeToTransactionTime (_bpReceiveTime blockInfo))
          state' <- BS.getBlockState
          return (blockInfo, newBlock, state')

instance (MonadIO (PersistentTreeStateMonad ati bs m),
          TS.BlockState m ~ bs,
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
          MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m))
         => LMDBStoreMonad (PersistentTreeStateMonad ati bs m) where

  writeBlock bp fr = do
    dbh <- use db
    bs <- BS.putBlockState (_bpState bp)
    let blockBS = runPut (S.put (finalizationIndex fr) <> S.put (_bpInfo bp) <> putBlock bp <> bs)
    dbh' <- putOrResize dbh (Block (getHash bp) (bpHeight bp) blockBS)
    db .=! dbh'

  writeFinalizationRecord fr = do
    dbh <- use db
    dbh' <- putOrResize dbh (Finalization (finalizationIndex fr) fr)
    db .=! dbh'

  writeTransactionStatus th t = do
    dbh <- use db
    dbh' <- putOrResize dbh (TxStatus th t)
    db .=! dbh'

  writeTransactionStatuses tss = do
    dbh <- use db
    dbh' <- putOrResize dbh (TxStatuses tss)
    db .=! dbh'


instance (MonadIO (PersistentTreeStateMonad ati bs m),
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
          TS.BlockState m ~ bs,
          CanExtend (ATIValues ati),
          MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m))
         => LMDBQueryMonad (PersistentTreeStateMonad ati bs m) where
  readBlock bh = do
    env <- use (db . storeEnv)
    dbB <- use (db . blockStore)
    bytes <- liftIO $ transaction env (L.get dbB bh :: L.Transaction ReadOnly (Maybe ByteString))
    constructBlock bytes

  readFinalizationRecord bh = do
    env <- use (db . storeEnv)
    dbF <- use (db . finalizationRecordStore)
    liftIO $ transaction env (L.get dbF bh :: L.Transaction ReadOnly (Maybe FinalizationRecord))

  readFinalizedBlockAtHeight bHeight = do
    env <- use (db . storeEnv)
    dbFH <- use (db . finalizedByHeightStore)
    mbh <- liftIO $ transaction env (L.get dbFH bHeight :: L.Transaction ReadOnly (Maybe BlockHash))
    case mbh of
      Nothing -> return Nothing
      Just bh -> readBlock bh

  readTransactionStatus th = do
    env <- use (db . storeEnv)
    dbT <- use (db . transactionStatusStore)
    liftIO $ transaction env (L.get dbT th :: L.Transaction ReadOnly (Maybe T.TransactionStatus))


instance (MonadIO (PersistentTreeStateMonad ati bs m),
          GS.BlockState (PersistentTreeStateMonad ati bs m) ~ bs,
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
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
                  (Just block, Just finr) -> return $ Just (TS.BlockFinalized block finr)
                  (Just _, Nothing) -> error $ "Lost finalization record that was stored" ++ show bh
                  (Nothing, Just _) -> error $ "Lost block that was stored as finalized" ++ show bh
                  _ -> error $ "Lost block and finalization record" ++ show bh
    makeLiveBlock block parent lastFin st ati arrTime energy = do
            blockP <- makePersistentBlockPointerFromPendingBlock block parent lastFin st ati arrTime energy
            blockTable . at' (getHash block) ?=! BlockAlive blockP
            return blockP
    markDead bh = blockTable . at' bh ?= BlockDead
    markFinalized bh fr = use (blockTable . at' bh) >>= \case
            Just (BlockAlive bp) -> do
              writeBlock bp fr
              blockTable . at' bh ?=! BlockFinalized (finalizationIndex fr)
            _ -> return ()
    markPending pb = blockTable . at' (getHash pb) ?=! BlockPending pb
    getGenesisBlockPointer = use genesisBlockPointer
    getGenesisData = use genesisData
    getLastFinalized = do
      lf <- use lastFinalized
      lfr <- use lastFinalizationRecord
      return (lf, lfr)
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
             readBlock (finalizationBlockPointer diskFinRec)
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
        readFinalizedBlockAtHeight bHeight

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
        Just (WithMetadata{wmdData=CredentialDeployment{..},..}, _) -> return $! Just WithMetadata{wmdData=biCred,..}
        _ -> return Nothing

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
                  let wmdtr = WithMetadata{wmdData=tr,..}
                  transactionTable .= (tt & (ttNonFinalizedTransactions . at' sender . non emptyANFT . anftMap . at' nonce . non Set.empty %~ Set.insert wmdtr)
                                          & (ttHashMap . at' trHash ?~ (bi, Received slot)))
                  transactionTablePurgeCounter %= (+ 1)
                  purgeCount <- use transactionTablePurgeCounter
                  RuntimeParameters{..} <- use runtimeParameters
                  when (purgeCount > rpInsertionsBeforeTransactionPurge) $ do
                    TS.purgeTransactionTable
                    transactionTablePurgeCounter .= 0
                  return (TS.Added bi)
                else return TS.ObsoleteNonce
              CredentialDeployment{..} -> do
                -- because we do not have nonce tracking for these transactions we need to check that
                -- this transction does not already exist in the on-disk storage.
                finalizedP <- memberTransactionTable trHash
                if finalizedP then
                  return $ TS.Duplicate bi
                else do
                  transactionTable . ttHashMap . at' trHash ?= (bi, Received slot)
                  return (TS.Added bi)
          Just (_, results) -> do
            -- if it is we update the maximum committed slot,
            -- unless the transaction is already finalized (this case is handled by updateSlot)
            -- In the current model this latter case should not happen; once a transaction is finalized
            -- it is written to disk (see finalizeTransactions below)
            when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at' trHash . mapped . _2 %= updateSlot slot
            return $ TS.Duplicate bi

    finalizeTransactions bh slot txs = mapM finTrans txs >>= writeTransactionStatuses
        where
            finTrans WithMetadata{wmdData=NormalTransaction tr,..} = do
                let nonce = transactionNonce tr
                    sender = transactionSender tr
                anft <- use (transactionTable . ttNonFinalizedTransactions . at' sender . non emptyANFT)
                assert (anft ^. anftNextNonce == nonce) $ do
                    let nfn = anft ^. anftMap . at' nonce . non Set.empty
                    let wmdtr = WithMetadata{wmdData=tr,..}
                    assert (Set.member wmdtr nfn) $ do
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
            finTrans WithMetadata{wmdData=CredentialDeployment{..},..} = deleteAndFinalizeStatus wmdHash

            deleteAndFinalizeStatus txHash = do
              status <- preuse (transactionTable . ttHashMap . ix txHash . _2)
              case status of
                Just (Committed{..}) -> do
                  -- delete the transaction from the cache
                  transactionTable . ttHashMap . at' txHash .= Nothing
                  -- and write the status to disk
                  return (txHash, Finalized{_tsSlot=slot,
                                            tsBlockHash=bh,
                                            tsFinResult=tsResults HM.! bh,
                                            -- the previous lookup is safe; finalized transaction must be on a block
                                            ..})
                _ -> error "Transaction should exist and be in committed state when finalized."

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
                if (lastFinSlot >= results ^. tsSlot) then do
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
         Nothing -> readTransactionStatus th

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .=! stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters

    -- | This function will remove transactions that are pending for a long time.
    -- First it goes over the account non finalized transactions and for each account it separates the transactions that
    -- will be purged to those that won't. For a transaction to be purged its arrival time plus the transactions keep-alive
    -- time must be lower that the current timestamp and its status must be `Received`.
    -- The `ttNonFinalizedTransactions` map is updated and afterwards the transactions are removed from the `ttHashMap`.
    -- The `PendingTransactionTable` is updated doing a rollback to the last seen nonce on the right side of each entry in
    -- `pttWithSender`.
    purgeTransactionTable = do
      lfs <- blockSlot <$> use lastFinalized
      tt <- use transactionTable
      pt <- use pendingTransactions
      RuntimeParameters{..} <- TS.getRuntimeParameters
      txHighestNonces <- removeTransactions tt rpTransactionsKeepAliveTime lfs
      pendingTransactions .= rollbackNonces txHighestNonces pt
     where
       removeTransactions TransactionTable{..} kat lastFinalizedSlot = do
         tm <- liftIO (utcTimeToTransactionTime <$> getCurrentTime)
         let removeTxs :: [(Nonce, Set.Set T.Transaction)] -> ([Set.Set T.Transaction], Map.Map Nonce (Set.Set T.Transaction), Nonce)
             removeTxs ncs = (\(x, y, h) -> (x, Map.fromList y, h)) $ removeTxs' ncs 0

             removeTxs' :: [(Nonce, Set.Set T.Transaction)] -> Nonce -> ([Set.Set T.Transaction], [(Nonce, Set.Set T.Transaction)], Nonce)
             removeTxs' [] h = ([], [], h)
             removeTxs' ((n, txs):ncs) hn =
               -- We can only remove transactions which are not committed to any blocks.
               -- Otherwise we would break many invariants.
               let toRemove t =
                     case _ttHashMap ^? ix (biHash t) . _2 of
                       -- we cannot remove a transaction that was received in a block that has not yet been purged
                       -- if its received slot is >= last finalized then the transaction will be in a live block
                       -- that might be processed at some point.
                       Just Received{..} -> _tsSlot <= lastFinalizedSlot
                       _ -> False
                   (toDrop, nn) =
                     Set.partition (\t -> biArrivalTime t + kat < tm && toRemove t) txs in -- split in old and still valid transactions
                 if Set.size nn == 0
                 -- if all are to be removed, remove all the next txs and return previous nonce
                 then (toDrop : fmap snd ncs, [], hn)
                 else let (nextToDrop, nextToKeep, nextHn) = removeTxs' ncs n in
                   -- else combine the transactions to drop and the transactions to keep with the ones from the next step
                   (toDrop : nextToDrop, (n, nn) : nextToKeep, nextHn)

             processANFT :: (AccountAddress, AccountNonFinalizedTransactions) -> ([Set.Set T.Transaction], (AccountAddress, AccountNonFinalizedTransactions), (AccountAddress, Nonce))
             processANFT (acc, AccountNonFinalizedTransactions{..}) =
               -- we need to get a list in ascending order
               -- in order to do only one pass over the nonces if we ever hit a nonce that gets emptied
               let (removed, kept, hn) = removeTxs (Map.toAscList _anftMap) in
                 (removed, (acc, AccountNonFinalizedTransactions{_anftMap = kept, ..}), (acc, hn))

             results = fmap processANFT (HM.toList $ _ttNonFinalizedTransactions)
             allDeletes = fmap (^. _1) results
             !newNFT = fromList $ fmap (^. _2) results
             highestNonces = fmap (^. _3) results
             -- remove all normal transactions that should be removed
             !newTMap = Fold.foldl' (Fold.foldl' (Fold.foldl' (\h tx -> (HM.delete (biHash tx) h)))) _ttHashMap allDeletes
             -- and finally remove all the credential deployments that are too old.
             !finalTT = HM.filter (\case
                                      (WithMetadata{wmdData=CredentialDeployment{},..}, Received{..}) ->
                                          wmdArrivalTime + kat >= tm && _tsSlot > lastFinalizedSlot
                                      _ -> True
                                  ) newTMap
         transactionTable .= TransactionTable{_ttHashMap = finalTT, _ttNonFinalizedTransactions = newNFT}
         return highestNonces

       rollbackNonces :: [(AccountAddress, Nonce)] -> PendingTransactionTable -> PendingTransactionTable
       rollbackNonces e PTT{..} = PTT {_pttWithSender =
                                       let !v = Fold.foldl' (\pt (acc, n) ->
                                                               update (\(n1, n2) ->
                                                                         if n2 > n && n >= n1 then Just (n1, n)
                                                                         else if n2 > n then Nothing
                                                                         else Just (n1, n2)) acc pt) _pttWithSender e in v,
                                       ..}
