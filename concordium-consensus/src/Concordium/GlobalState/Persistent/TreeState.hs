  {-# LANGUAGE ConstraintKinds, TypeFamilies, TemplateHaskell, NumericUnderscores, ScopedTypeVariables, DataKinds, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, DerivingStrategies, DerivingVia, StandaloneDeriving, UndecidableInstances #-}
-- |This module provides a monad that is an instance of both `LMDBStoreMonad` and `TreeStateMonad` effectively adding persistence to the tree state.
--
-- In this module we also implement the instances and functions that require a monadic context, such as the conversions.
module Concordium.GlobalState.Persistent.TreeState where

import Concordium.GlobalState.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
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
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.HashMap.Strict as HM hiding (toList)
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import Data.Serialize as S (runGet, put, get, runPut, runGet, Put)
import qualified Data.Set as Set
import Data.Time.Clock
import Database.LMDB.Simple as L
import Lens.Micro.Platform
import System.Mem.Weak
import Concordium.GlobalState.SQLiteATI
-- * SkovPersistentData definition

data PersistenBlockStatus ati bs =
    BlockAlive !(PersistentBlockPointer ati bs)
    | BlockDead
    | BlockFinalized !FinalizationIndex
    | BlockPending !PendingBlock
  deriving(Eq, Show)

-- |Skov data for the persistent tree state version that also holds the database handlers
data SkovPersistentData ati bs = SkovPersistentData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(HM.HashMap BlockHash (PersistenBlockStatus (ATIValues ati) bs)),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |Priority queue of blocks waiting for their last finalized block to be finalized, ordered by height of the last finalized block
    _blocksAwaitingLastFinalized :: !(MPQ.MinPQueue BlockHeight PendingBlock),
    -- |Pointer to the last finalized block
    _lastFinalized :: !(PersistentBlockPointer (ATIValues ati) bs),
    -- |Pointer to the last finalization record
    _lastFinalizationRecord :: !FinalizationRecord,
    -- |Pending finalization records by finalization index
    _finalizationPool :: !(Map.Map FinalizationIndex [FinalizationRecord]),
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
    -- |Consensus statistics
    _statistics :: !ConsensusStatistics,
    -- |Runtime parameters
    _runtimeParameters :: !RuntimeParameters,
    -- | Database handlers
    _db :: !(DatabaseHandlers bs),
    -- |Context for the transaction log.
    _atiCtx :: !(ATIContext ati)
}
makeLenses ''SkovPersistentData

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovPersistentDataDefault ::  FilePath -> GenesisData -> bs -> (ATIValues ati, ATIContext ati) -> S.Put -> IO (SkovPersistentData ati bs)
initialSkovPersistentDataDefault dir = initialSkovPersistentData (defaultRuntimeParameters { rpTreeStateDir = dir })

initialSkovPersistentData :: RuntimeParameters -> GenesisData -> bs -> (ATIValues ati, ATIContext ati) -> S.Put -> IO (SkovPersistentData ati bs)
initialSkovPersistentData rp gd genState ati serState = do
  gb <- makeGenesisPersistentBlockPointer gd genState (fst ati)
  let gbh = bpHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  initialDb <- initialDatabaseHandlers gb serState rp
  return SkovPersistentData {
            _blockTable = HM.singleton gbh (BlockFinalized 0),
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _blocksAwaitingLastFinalized = MPQ.empty,
            _lastFinalized = gb,
            _lastFinalizationRecord = gbfin,
            _finalizationPool = Map.empty,
            _branches = Seq.empty,
            _genesisData = gd,
            _genesisBlockPointer = gb,
            _focusBlock = gb,
            _pendingTransactions = emptyPendingTransactionTable,
            _transactionTable = emptyTransactionTable,
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _db = initialDb,
            _atiCtx = snd ati
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
            BS.BlockStateQuery, BS.BlockStateOperations, BS.BlockStateStorage)

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

instance (bs ~ GS.BlockState (PersistentTreeStateMonad ati bs m)) => GlobalStateTypes (PersistentTreeStateMonad ati bs m) where
    type PendingBlockType (PersistentTreeStateMonad ati bs m) = PendingBlock
    type BlockPointerType (PersistentTreeStateMonad ati bs m) = PersistentBlockPointer (ATIValues ati) bs

instance HasLogContext PerAccountAffectIndex (SkovPersistentData DiskDump bs) where
  logContext = atiCtx

getWeakPointer :: (bs ~ GS.BlockState (PersistentTreeStateMonad ati bs m),
                   MonadIO (PersistentTreeStateMonad ati bs m),
                   BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
                   CanExtend (ATIValues ati),
                   MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m))
               => PersistentBlockPointer (ATIValues ati) (TS.BlockState m)
               -> (PersistentBlockPointer (ATIValues ati) (TS.BlockState m) -> Weak (PersistentBlockPointer (ATIValues ati) (TS.BlockState m)))
               -> (BlockFields -> BlockHash)
               -> String
               -> PersistentTreeStateMonad ati (TS.BlockState m) m (PersistentBlockPointer (ATIValues ati) (TS.BlockState m))
getWeakPointer block field pointer name = do
      gb <- use genesisBlockPointer
      if gb == block then
        return gb
      else do
        d <- liftIO $ deRefWeak (field block)
        case d of
          Just v -> return v
          Nothing -> do
            nb <- maybe (return $ Just gb) (\f -> readBlock (pointer f)) (blockFields $ _bpBlock block)
            return $ fromMaybe (error ("Couldn't find " ++ name ++ " block in disk")) nb

instance (Monad (PersistentTreeStateMonad ati bs m),
          bs ~ GS.BlockState (PersistentTreeStateMonad ati bs m),
          MonadIO (PersistentTreeStateMonad ati bs m),
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
          MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m),
          CanExtend (ATIValues ati),
          CanRecordFootprint (Footprint (ATIValues ati)))
         => BlockPointerMonad (PersistentTreeStateMonad ati bs m) where
  blockState = return . _bpState
  bpParent block = getWeakPointer block _bpParent blockPointer "parent"
  bpLastFinalized block = getWeakPointer block _bpLastFinalized blockLastFinalized "last finalized"
  bpTransactionAffectSummaries block = return (_bpATI block)


-- |Construct a block from a serialized form.
-- The @ati@ is filled with a default value.
constructBlock :: (MonadIO m,
                   BS.BlockStateStorage m,
                   CanExtend (ATIStorage m))
               => Maybe ByteString -> BlockHash -> m (Maybe (PersistentBlockPointer (ATIStorage m) (TS.BlockState m)))
constructBlock Nothing _ = return Nothing
constructBlock (Just bytes) bh = do
  tm <- liftIO getCurrentTime
  case runGet (getTriple tm) bytes of
    Left err -> fail $ "Could not deserialize block: " ++ err ++ " with bytes " ++ show bytes
    Right (newBlock, state', height', txcount, txsize, txenergy) -> do
      st <- state'
      let ati = defaultValue
      Just <$> (makeBlockPointerFromPersistentBlock newBlock st ati bh height' txcount txsize txenergy)
  where getTriple tm = do
          newBlock <- getBlock (utcTimeToTransactionTime tm)
          state' <- BS.getBlockState
          height' <- S.get
          txcount <- S.get
          txsize <- S.get
          txenergy <- S.get
          return (newBlock, state', height', txcount, txsize, txenergy)

instance (MonadIO (PersistentTreeStateMonad ati bs m),
          bs ~ GS.BlockState (PersistentTreeStateMonad ati bs m),
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
          CanExtend (ATIValues ati),
          MonadState (SkovPersistentData ati bs) (PersistentTreeStateMonad ati bs m))
         => LMDBStoreMonad (PersistentTreeStateMonad ati bs m) where
  writeBlock bp = do
    dbh <- use db
    bs <- BS.putBlockState (_bpState bp)
    -- This value won't be created with `putBlock` because
    -- we don't want the transactions to be written in this block
    -- so instead we will use `blockBody`
    let blockBS = runPut (do
                             putBlock bp
                             bs
                             S.put (bpHeight bp)
                             S.put (bpTransactionCount bp)
                             S.put (bpTransactionsSize bp)
                             S.put (bpTransactionsEnergyCost bp))
    dbh' <- putOrResize dbh (Block (getHash bp, blockBS))
    db .= dbh'
  readBlock bh = do
    env <- use (db . storeEnv)
    dbB <- use (db . blockStore)
    bytes <- liftIO $ transaction env (L.get dbB bh :: L.Transaction ReadOnly (Maybe ByteString))
    constructBlock bytes bh
  readFinalizationRecord bh = do
    env <- use (db . storeEnv)
    dbF <- use (db . finalizationRecordStore)
    liftIO $ transaction env (L.get dbF bh :: L.Transaction ReadOnly (Maybe FinalizationRecord))
  writeFinalizationRecord fr = do
    dbh <- use db
    dbh' <- putOrResize dbh (Finalization (finalizationIndex fr, fr))
    db .= dbh'
  readTransactionStatus th = do
    env <- use (db . storeEnv)
    dbT <- use (db . transactionStatusStore)
    liftIO $ transaction env (L.get dbT th :: L.Transaction ReadOnly (Maybe T.TransactionStatus))
  writeTransactionStatus th t = do
    dbh <- use db
    dbh' <- putOrResize dbh (TxStatus (th, t))
    db .= dbh'

instance (MonadIO (PersistentTreeStateMonad ati bs m),
          BS.BlockStateStorage (PersistentTreeStateMonad ati bs m),
          GS.BlockState (PersistentTreeStateMonad ati bs m) ~ bs,
          PerAccountDBOperations (PersistentTreeStateMonad ati bs m),
          MonadState (SkovPersistentData ati bs) m)
         => TS.TreeStateMonad (PersistentTreeStateMonad ati bs m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = do
        return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    importPendingBlock blockBS rectime =
        case runGet (getBlock $ utcTimeToTransactionTime rectime) blockBS of
            Left err -> return $ Left $ "Block deserialization failed: " ++ err
            Right GenesisBlock {} -> return $ Left "Block deserialization failed: unexpected genesis block"
            Right (NormalBlock block0) -> return $ Right $  makePendingBlock block0 rectime
    getBlockStatus bh = do
      st <- use (blockTable . at bh)
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
            blockTable . at (getHash block) ?= BlockAlive blockP
            return blockP
    markDead bh = blockTable . at bh ?= BlockDead
    markFinalized bh fr = use (blockTable . at bh) >>= \case
            Just (BlockAlive bp) -> do
              writeBlock bp
              blockTable . at bh ?= BlockFinalized (finalizationIndex fr)
            _ -> return ()
    markPending pb = blockTable . at (getHash pb) ?= BlockPending pb
    getGenesisBlockPointer = use genesisBlockPointer
    getGenesisData = use genesisData
    getLastFinalized = do
      lf <- use lastFinalized
      lfr <- use lastFinalizationRecord
      return $! (lf, lfr)
    getNextFinalizationIndex = (+1) . finalizationIndex <$> use lastFinalizationRecord
    addFinalization newFinBlock finRec = do
      writeFinalizationRecord finRec
      lastFinalized .= newFinBlock
      lastFinalizationRecord .= finRec
    getFinalizationAtIndex finIndex = do
      lfr <- use lastFinalizationRecord
      if finIndex == finalizationIndex lfr then do
        b <- use lastFinalized
        return $ Just (lfr, b)
      else do
        dfr <- readFinalizationRecord finIndex
        case dfr of
          Just diskFinRec -> do
             diskb <- readBlock (finalizationBlockPointer diskFinRec)
             case diskb of
                Just diskBlock -> return $ Just (diskFinRec, diskBlock)
                _ -> return Nothing
          _ -> return Nothing
    getBranches = use branches
    putBranches brs = branches .= brs
    takePendingChildren bh = possiblyPendingTable . at bh . non [] <<.= []
    addPendingBlock pb = do
        let parent = blockPointer (bbFields (pbBlock pb))
        possiblyPendingTable . at parent . non [] %= (pb:)
        possiblyPendingQueue %= MPQ.insert (blockSlot (pbBlock pb)) (getHash pb, parent)
    takeNextPendingUntil slot = tnpu =<< use possiblyPendingQueue
        where
            tnpu ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, (pbh, parenth)), ppq') ->
                    if sl <= slot then do
                        (myPB, otherPBs) <- partition ((== pbh) . pbHash) <$> use (possiblyPendingTable . at parenth . non [])
                        case myPB of
                            [] -> tnpu ppq'
                            (realPB : _) -> do
                                possiblyPendingTable . at parenth . non [] .= otherPBs
                                possiblyPendingQueue .= ppq'
                                return (Just realPB)
                    else do
                        possiblyPendingQueue .= ppq
                        return Nothing
                Nothing -> do
                    possiblyPendingQueue .= ppq
                    return Nothing
    addAwaitingLastFinalized bh pb = blocksAwaitingLastFinalized %= MPQ.insert bh pb
    takeAwaitingLastFinalizedUntil bh =
            (MPQ.minViewWithKey <$> use blocksAwaitingLastFinalized) >>= \case
                Nothing -> return Nothing
                Just ((h, pb), balf') -> if h <= bh then do
                                            blocksAwaitingLastFinalized .= balf'
                                            return (Just pb)
                                        else return Nothing
    getFinalizationPoolAtIndex fi = use (finalizationPool . at fi . non [])
    putFinalizationPoolAtIndex fi frs = finalizationPool . at fi . non [] .= frs
    addFinalizationRecordToPool fr = finalizationPool . at (finalizationIndex fr) . non [] %= (fr :)
    getFocusBlock = use focusBlock
    putFocusBlock bb = focusBlock .= bb
    getPendingTransactions = use pendingTransactions
    putPendingTransactions pts = pendingTransactions .= pts

    getAccountNonFinalized addr nnce =
            use (transactionTable . ttNonFinalizedTransactions . at addr) >>= \case
                Nothing -> return []
                Just anfts ->
                    let (_, atnnce, beyond) = Map.splitLookup nnce (anfts ^. anftMap)
                    in return $ case atnnce of
                        Nothing -> Map.toAscList beyond
                        Just s -> (nnce, s) : Map.toAscList beyond

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
                if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                  let wmdtr = WithMetadata{wmdData=tr,..}
                  transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert wmdtr)
                                          & (ttHashMap . at trHash ?~ (bi, Received slot)))
                  return (TS.Added bi)
                else return TS.ObsoleteNonce
              CredentialDeployment{..} -> do
                -- because we do not have nonce tracking for these transactions we need to check that
                -- this transction does not already exist in the on-disk storage.
                finalizedP <- memberTransactionTable trHash
                if finalizedP then
                  return $ TS.Duplicate bi
                else do
                  transactionTable . ttHashMap . at trHash ?= (bi, Received slot)
                  return (TS.Added bi)
          Just (_, results) -> do
            -- if it is we update the maximum committed slot,
            -- unless the transaction is already finalized (this case is handled by updateSlot)
            -- In the current model this latter case should not happen; once a transaction is finalized
            -- it is written to disk (see finalizeTransactions below)
            when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at trHash . mapped . _2 %= updateSlot slot
            return $ TS.Duplicate bi

    finalizeTransactions bh slot = mapM_ finTrans
        where
            finTrans WithMetadata{wmdData=NormalTransaction tr,..} = do
                let nonce = transactionNonce tr
                    sender = transactionSender tr
                anft <- use (transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT)
                assert (anft ^. anftNextNonce == nonce) $ do
                    let nfn = anft ^. anftMap . at nonce . non Set.empty
                    let wmdtr = WithMetadata{wmdData=tr,..}
                    assert (Set.member wmdtr nfn) $ do
                        -- Remove any other transactions with this nonce from the transaction table.
                        -- They can never be part of any other block after this point.
                        forM_ (Set.delete wmdtr nfn) $
                          \deadTransaction -> transactionTable . ttHashMap . at (getHash deadTransaction) .= Nothing
                        -- Mark the status of the transaction as finalized, and remove the data from the in-memory table.
                        deleteAndFinalizeStatus wmdHash

                        -- Update the non-finalized transactions for the sender
                        transactionTable . ttNonFinalizedTransactions . at sender ?= (anft & (anftMap . at nonce .~ Nothing)
                                                                                           & (anftNextNonce .~ nonce + 1))
            finTrans WithMetadata{wmdData=CredentialDeployment{..},..} = deleteAndFinalizeStatus wmdHash

            deleteAndFinalizeStatus txHash = do
              status <- preuse (transactionTable . ttHashMap . ix txHash . _2)
              case status of
                Just (Committed{..}) -> do
                  -- delete the transaction from the cache
                  transactionTable . ttHashMap . at txHash .= Nothing
                  -- and write the status to disk
                  writeTransactionStatus txHash Finalized{_tsSlot=slot,
                                                          tsBlockHash=bh,
                                                          tsFinResult=tsResults HM.! bh,
                                                           -- the previous lookup is safe; finalized transaction must be on a block
                                                          ..}
                _ -> error "Transaction should exist and be in committed state when finalized."

    commitTransaction slot bh tr idx =
        -- add a transaction status. This only updates the cached version which is correct at the moment
        -- because transactions are only written to disk on finalization, at which point their
        -- statuses are no longer updated.
        transactionTable . ttHashMap . at (getHash tr) %= fmap (_2 %~ addResult bh slot idx)

    purgeTransaction WithMetadata{..} =
        use (transactionTable . ttHashMap . at wmdHash) >>= \case
            Nothing -> return True
            Just (_, results) -> do
                lastFinSlot <- blockSlot . _bpBlock . fst <$> TS.getLastFinalized
                if (lastFinSlot >= results ^. tsSlot) then do
                    -- remove from the table
                    transactionTable . ttHashMap . at wmdHash .= Nothing
                    -- if the transaction is from a sender also delete the relevant
                    -- entry in the account non finalized table
                    case wmdData of
                      NormalTransaction tr -> do
                        let nonce = transactionNonce tr
                            sender = transactionSender tr
                        transactionTable
                          . ttNonFinalizedTransactions
                          . at sender
                          . non emptyANFT
                          . anftMap
                          . at nonce
                          . non Set.empty %= Set.delete WithMetadata{wmdData=tr,..}
                      _ -> return () -- do nothing.
                    return True
                else return False

    markDeadTransaction bh tr =
      -- We only need to update the outcomes. The anf table nor the pending table need be updated
      -- here since a transaction should not be marked dead in a finalized block.
      transactionTable . ttHashMap . at (getHash tr) . mapped . _2 %= markDeadResult bh
    lookupTransaction th = do
       ts <- preuse (transactionTable . ttHashMap . ix th . _2)
       case ts of
         Just t -> return $ Just t
         Nothing -> readTransactionStatus th

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters
