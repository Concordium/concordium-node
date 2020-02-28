{-# LANGUAGE TypeFamilies, TemplateHaskell, NumericUnderscores, ScopedTypeVariables, DataKinds, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, DerivingStrategies, DerivingVia, StandaloneDeriving, UndecidableInstances #-}
-- |This module provides a monad that is an instance of both `LMDBStoreMonad` and `TreeStateMonad` effectively adding persistence to the tree state.
module Concordium.GlobalState.Persistent.TreeState (
  SkovPersistentData
  , initialSkovPersistentDataDefault
  , initialSkovPersistentData
  , PersistentTreeStateMonad (..)
  -- For testing purposes
  , PersistenBlockStatus(..)
  , db
  , genesisBlockPointer
  , blockTable
  , constructBlock
  ) where

import Concordium.GlobalState.Basic.Block as B
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.AccountTransactionIndex
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Control.Exception
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

data PersistenBlockStatus ati bs =
    BlockAlive !(PersistentBlockPointer ati bs)
    | BlockDead
    | BlockFinalized !FinalizationIndex
    | BlockPending !PendingBlock
  deriving(Eq, Show)

-- |Skov data for the persistent tree state version that also holds the database handlers
data SkovPersistentData ati bs = SkovPersistentData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(HM.HashMap BlockHash (PersistenBlockStatus ati bs)),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |Priority queue of blocks waiting for their last finalized block to be finalized, ordered by height of the last finalized block
    _blocksAwaitingLastFinalized :: !(MPQ.MinPQueue BlockHeight PendingBlock),
    -- |Pointer to the last finalized block
    _lastFinalized :: !(PersistentBlockPointer ati bs),
    -- |Pointer to the last finalization record
    _lastFinalizationRecord :: !FinalizationRecord,
    -- |Pending finalization records by finalization index
    _finalizationPool :: !(Map.Map FinalizationIndex [FinalizationRecord]),
    -- |Branches of the tree by height above the last finalized block
    _branches :: !(Seq.Seq [PersistentBlockPointer ati bs]),
    -- |Genesis data
    _genesisData :: !GenesisData,
    -- |Block pointer to genesis block
    _genesisBlockPointer :: !(PersistentBlockPointer ati bs),
    -- |Current focus block
    _focusBlock :: !(PersistentBlockPointer ati bs),
    -- |Pending transaction table
    _pendingTransactions :: !PendingTransactionTable,
    -- |Transaction table
    _transactionTable :: !TransactionTable,
    -- |Consensus statistics
    _statistics :: !ConsensusStatistics,
    -- |Runtime parameters
    _runtimeParameters :: !RuntimeParameters,
    -- | Database handlers
    _db :: DatabaseHandlers bs
}
makeLenses ''SkovPersistentData

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovPersistentDataDefault ::  FilePath -> GenesisData -> bs -> ati -> S.Put -> IO (SkovPersistentData ati bs)
initialSkovPersistentDataDefault dir = initialSkovPersistentData (defaultRuntimeParameters { rpTreeStateDir = dir })

initialSkovPersistentData :: RuntimeParameters -> GenesisData -> bs -> ati -> S.Put -> IO (SkovPersistentData ati bs)
initialSkovPersistentData rp gd genState ati serState = do
  gb <- makeGenesisBlockPointer gd genState ati
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
            _db = initialDb
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

deriving instance (PerAccountDBOperations m, ATIStorage m ~ ati) => PerAccountDBOperations (PersistentTreeStateMonad ati bs m)


instance (bs ~ GS.BlockState m, ati ~ ATIStorage m) => GS.GlobalStateTypes (PersistentTreeStateMonad ati bs m) where
    type PendingBlock (PersistentTreeStateMonad ati bs m) = PendingBlock
    type BlockPointer (PersistentTreeStateMonad ati bs m) = PersistentBlockPointer ati bs

-- |Construct a block from a serialized form.
-- The @ati@ is filled with a default value.
constructBlock :: forall m . (MonadIO m, BS.BlockStateStorage m, CanExtend (ATIStorage m))
               => Maybe ByteString -> m (Maybe (PersistentBlockPointer (ATIStorage m) (TS.BlockState m)))
constructBlock Nothing = return Nothing
constructBlock (Just bytes) = do
  tm <- liftIO getCurrentTime
  case runGet (getTriple tm) bytes of
    Left err -> fail $ "Could not deserialize block: " ++ err
    Right (newBlock, state', height') -> do
      st <- state'
      let ati = defaultValue
      liftIO $! Just <$> (makeBlockPointerFromBlock newBlock st ati height')
  where getTriple tm = do
          newBlock <- B.getBlock (utcTimeToTransactionTime tm)
          state' <- BS.getBlockState
          height' <- S.get
          return (newBlock, state', height')

instance (bs ~ GS.BlockState m,
          MonadIO m,
          BS.BlockStateStorage m,
          ATIStorage m ~ ati,
          CanExtend ati,
          MonadState (SkovPersistentData ati bs) m)
         => LMDBStoreMonad (PersistentTreeStateMonad ati bs m) where
  writeBlock bp = do
    dbh <- use db
    bs <- BS.putBlockState (_bpState bp)
    dbh' <- putOrResize dbh (Block (getHash bp, runPut (putBlock bp >> bs >> S.put (bpHeight bp))))
    db .= dbh'
  readBlock bh = do
    env <- use (db . storeEnv)
    dbB <- use (db . blockStore)
    bytes <- liftIO $ transaction env (L.get dbB bh :: L.Transaction ReadOnly (Maybe ByteString))
    constructBlock bytes
  readFinalizationRecord bh = do
    env <- use (db . storeEnv)
    dbF <- use (db . finalizationRecordStore)
    liftIO $ transaction env (L.get dbF bh :: L.Transaction ReadOnly (Maybe FinalizationRecord))
  writeFinalizationRecord fr = do
    dbh <- use db
    dbh' <- putOrResize dbh (Finalization (finalizationIndex fr, fr))
    db .= dbh'

getWeakPointer :: (MonadState (SkovPersistentData ati s) m,
                  LMDBStoreMonad m, TS.BlockPointer m ~ PersistentBlockPointer ati s) =>
                 PersistentBlockPointer ati s
               -> (PersistentBlockPointer ati s -> Weak (PersistentBlockPointer ati s))
               -> (BlockFields -> BlockHash)
               -> String
               -> m (PersistentBlockPointer ati s)
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

instance (bs ~ GS.BlockState m,
          BS.BlockStateStorage m,
          Monad m,
          MonadIO m,
          ATIStorage m ~ ati,
          CanExtend ati,
          MonadState (SkovPersistentData ati bs) m) => BlockPointerMonad (PersistentTreeStateMonad ati bs m) where
    blockState = return . _bpState
    bpParent block = getWeakPointer block _bpParent blockPointer "parent"
    bpLastFinalized block = getWeakPointer block _bpLastFinalized blockLastFinalized "last finalized"

instance (CanExtend ati, CanRecordFootprint (Footprint ati))
         => ATITypes (PersistentTreeStateMonad ati bs m) where
  type ATIStorage (PersistentTreeStateMonad ati bs m) = ati
  
instance (bs ~ GS.BlockState m,
          BS.BlockStateStorage m,
          Monad m,
          MonadIO m,
          ATIStorage m ~ ati,
          PerAccountDBOperations m,
          MonadState (SkovPersistentData ati bs) m
          )
          => TS.TreeStateMonad (PersistentTreeStateMonad ati bs m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    importPendingBlock blockBS rectime =
        case runGet (getBlock $ utcTimeToTransactionTime rectime) blockBS of
            Left err -> return $ Left $ "Block deserialization failed: " ++ err
            Right GenesisBlock {} -> return $ Left "Block deserialization failed: unexpected genesis block"
            Right (NormalBlock block0) -> return $ Right $ makePendingBlock block0 rectime
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
            blockP <- liftIO $ makeBlockPointerFromPendingBlock block parent lastFin st ati arrTime energy
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
    addCommitTransaction tr slot = do
            let trHash = getHash tr
            tt <- use transactionTable
            case tt ^. ttHashMap . at trHash of
                Nothing ->
                  if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                    transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                            & (ttHashMap . at (getHash tr) ?~ (tr, Received slot)))
                    return (TS.Added tr)
                  else return TS.ObsoleteNonce
                Just (tr', results) -> do
                  when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at trHash . mapped . _2 . tsSlot .=  slot
                  return $ TS.Duplicate tr'
        where
            sender = transactionSender tr
            nonce = transactionNonce tr
    finalizeTransactions bh slot = mapM_ finTrans
        where
            finTrans tr = do
                let nonce = transactionNonce tr
                    sender = transactionSender tr
                anft <- use (transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT)
                assert (anft ^. anftNextNonce == nonce) $ do
                    let nfn = anft ^. anftMap . at nonce . non Set.empty
                    assert (Set.member tr nfn) $ do
                        -- Remove any other transactions with this nonce from the transaction table.
                        -- They can never be part of any other block after this point.
                        forM_ (Set.delete tr nfn) $ \deadTransaction -> transactionTable . ttHashMap . at (getHash deadTransaction) .= Nothing
                        -- Mark the status of the transaction as finalized.
                        -- Singular here is safe due to the precondition (and assertion) that all transactions
                        -- which are part of live blocks are in the transaction table.
                        transactionTable . ttHashMap . singular (ix (getHash tr)) . _2 %=
                            \case Committed{..} -> Finalized{_tsSlot=slot,tsBlockHash=bh,tsFinResult=tsResults HM.! bh,..}
                                  _ -> error "Transaction should be in committed state when finalized."
                        -- Update the non-finalized transactions for the sender
                        transactionTable . ttNonFinalizedTransactions . at sender ?= (anft & (anftMap . at nonce .~ Nothing) & (anftNextNonce .~ nonce + 1))

    commitTransaction slot bh tr idx =
        transactionTable . ttHashMap . at (getHash tr) %= fmap (_2 %~ addResult bh slot idx)
    purgeTransaction tr =
        use (transactionTable . ttHashMap . at (getHash tr)) >>= \case
            Nothing -> return True
            Just (_, results) -> do
                lastFinSlot <- blockSlot . _bpBlock . fst <$> TS.getLastFinalized
                if (lastFinSlot >= results ^. tsSlot) then do
                    let nonce = transactionNonce tr
                        sender = transactionSender tr
                    transactionTable . ttHashMap . at (getHash tr) .= Nothing
                    transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %= Set.delete tr
                    return True
                else return False

    markDeadTransaction bh tr =
      -- We only need to update the outcomes. The anf table nor the pending table need be updated
      -- here since a transaction should not be marked dead in a finalized block.
      transactionTable . ttHashMap . at (getHash tr) . mapped . _2 %= markDeadResult bh

    lookupTransaction th = use (transactionTable . ttHashMap . at th)

    updateBlockTransactions trs pb = return $ pb {pbBlock = (pbBlock pb) {bbTransactions = BlockTransactions trs}}

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters
