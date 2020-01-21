{-# LANGUAGE TypeFamilies, TemplateHaskell, NumericUnderscores, ScopedTypeVariables, DataKinds, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, DerivingStrategies, DerivingVia, StandaloneDeriving, UndecidableInstances #-}
-- |This module provides a monad that is an instance of both `LMDBStoreMonad` and `TreeStateMonad` effectively adding persistence to the tree state.
module Concordium.GlobalState.Persistent.TreeState where

import Concordium.GlobalState.Basic.Block as B
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Statistics
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Control.Exception
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable
import Data.HashMap.Strict as HM hiding (toList)
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import Data.Serialize as S (runGet, put, get, runPut, runGet, runGetState, decode)
import qualified Data.Set as Set
import Data.Time.Clock
import Database.LMDB.Simple as L
import Lens.Micro.Platform
import System.Mem.Weak

data PersistenBlockStatus bs =
    BlockAlive !(PersistentBlockPointer bs)
    | BlockDead
    | BlockFinalized !FinalizationIndex
    | BlockPending !PendingBlock
  deriving(Eq)

-- |Skov data for the persistent tree state version that also holds the database handlers
data SkovPersistentData bs = SkovPersistentData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(HM.HashMap BlockHash (PersistenBlockStatus bs)),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |Priority queue of blocks waiting for their last finalized block to be finalized, ordered by height of the last finalized block
    _blocksAwaitingLastFinalized :: !(MPQ.MinPQueue BlockHeight PendingBlock),
    -- |List of finalization records with the blocks that they finalize, starting from genesis
    _finalizationList :: !(Seq.Seq (FinalizationRecord, PersistentBlockPointer bs)),
    -- |Pending finalization records by finalization index
    _finalizationPool :: !(Map.Map FinalizationIndex [FinalizationRecord]),
    -- |Branches of the tree by height above the last finalized block
    _branches :: !(Seq.Seq [PersistentBlockPointer bs]),
    -- |Genesis data
    _genesisData :: !GenesisData,
    -- |Block pointer to genesis block
    _genesisBlockPointer :: !(PersistentBlockPointer bs),
    -- |Current focus block
    _focusBlock :: !(PersistentBlockPointer bs),
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
initialSkovPersistentDataDefault :: GenesisData -> bs -> ByteString -> IO (SkovPersistentData bs)
initialSkovPersistentDataDefault = initialSkovPersistentData defaultRuntimeParameters

initialSkovPersistentData :: RuntimeParameters -> GenesisData -> bs -> ByteString -> IO (SkovPersistentData bs)
initialSkovPersistentData rp gd genState serState = do
  gb <- makeGenesisBlockPointer gd genState
  let gbh = _bpHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  initialDb <- initialDatabaseHandlers gb serState
  return SkovPersistentData {
            _blockTable = HM.singleton gbh (BlockFinalized 0),
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _blocksAwaitingLastFinalized = MPQ.empty,
            _finalizationList = Seq.singleton (gbfin, gb),
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
--
-- This newtype establishes types for the @GlobalStateTypes@. The type variable @bs@ stands for the BlockState
-- type used in the implementation.
newtype PersistentTreeStateMonad bs m a = PersistentTreeStateMonad { runPureTreeStateMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, GS.BlockStateTypes,
            BS.BlockStateQuery, BS.BlockStateOperations, BS.BlockStateStorage)
deriving instance (Monad m, MonadState (SkovPersistentData bs) m) => MonadState (SkovPersistentData bs) (PersistentTreeStateMonad bs m)

instance (bs ~ GS.BlockState m) => GS.GlobalStateTypes (PersistentTreeStateMonad bs m) where
    type PendingBlock (PersistentTreeStateMonad bs m) = PendingBlock
    type BlockPointer (PersistentTreeStateMonad bs m) = PersistentBlockPointer bs

instance (bs ~ GS.BlockState m, MonadIO m, BS.BlockStateStorage m, MonadState (SkovPersistentData bs) m) => LMDBStoreMonad (PersistentTreeStateMonad bs m) where
  writeBlock bp = do
    lim <- use (db . limits)
    env <- use (db . storeEnv)
    dbB <- use (db . blockStore)
    bs <- BS.putBlockState (_bpState bp)
    (l, e, d) <- putOrResize lim "blocks" env dbB (getHash bp) $ runPut (putBlock bp >> bs >> S.put (bpHeight bp))
    db . limits  .= l
    db . storeEnv .= e
    db . blockStore .= d
  readBlock bh = do
    env <- use (db . storeEnv)
    dbB <- use (db . blockStore)
    bytes <- liftIO $ transaction env (L.get dbB bh :: L.Transaction ReadOnly (Maybe ByteString))
    case bytes of
      Just b -> do
        tm <- liftIO getCurrentTime
        case runGetState (B.getBlock (utcTimeToTransactionTime tm)) b 0 of
          Right (newBlock, rest) ->
           case runGetState S.get rest 0 of
             Right (bs, rest') ->
              case runGet BS.getBlockState bs of
                Right state' -> do
                  st <- state'
                  case decode rest' of
                    Right height' -> liftIO $ Just <$> makeBlockPointerFromBlock newBlock st height'
                    Left _ -> return Nothing
                Left _ -> return Nothing
             Left _ -> return Nothing
          Left _ -> return Nothing
      Nothing -> return Nothing
  readFinalizationRecord bh = do
    env <- use (db . storeEnv)
    dbF <- use (db . finalizationRecordStore)
    liftIO $ transaction env (L.get dbF bh :: L.Transaction ReadOnly (Maybe FinalizationRecord))
  writeFinalizationRecord fr = do
    lim <- use (db . limits)
    env <- use (db . storeEnv)
    dbF <- use (db . finalizationRecordStore)
    (l, e, d) <- putOrResize lim "finalization" env dbF (finalizationIndex fr) fr
    db . limits .= l
    db . storeEnv .= e
    db . finalizationRecordStore .= d

instance (bs ~ GS.BlockState m, BS.BlockStateStorage m, Monad m, MonadIO m, MonadState (SkovPersistentData bs) m)
          => TS.TreeStateMonad (PersistentTreeStateMonad bs m) where
    blockState = return . _bpState
    bpParent b = do
      d <- liftIO $ deRefWeak (_bpParent b)
      case d of
        Just v -> return v
        Nothing -> do
          nb <- readBlock (_bpHash b)
          return $ fromMaybe (error "Couldn't find parent block in disk") nb
    bpLastFinalized b = do
      d <- liftIO $ deRefWeak (_bpParent b)
      case d of
        Just v -> return v
        Nothing -> do
          nb <- readBlock (_bpHash b)
          return $ fromMaybe (error "Couldn't find last finalized block in disk") nb
    makePendingBlock key slot parent bid pf n lastFin trs time = return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    importPendingBlock blockBS rectime =
        case runGet (getBlock $ utcTimeToTransactionTime rectime) blockBS of
            Left err -> return $ Left $ "Block deserialization failed: " ++ err
            Right GenesisBlock {} -> return $ Left "Block deserialization failed: unexpected genesis block"
            Right (NormalBlock block0) -> return $ Right $ makePendingBlock block0 rectime
    getBlockStatus bh = do
      st <- (^. at bh) <$> (use blockTable)
      case st of
        Just (BlockAlive bp) -> return $ Just $ TS.BlockAlive bp
        Just (BlockPending bp) -> return $ Just $ TS.BlockPending bp
        Just (BlockDead) -> return $ Just $ TS.BlockDead
        Just (BlockFinalized fidx) -> do
          b <- readBlock bh
          fr <- readFinalizationRecord fidx
          case (b, fr) of
            (Just block, Just finr) -> return $ Just (TS.BlockFinalized block finr)
            (Just _, Nothing) -> error $ "Lost finalization record that was stored" ++ show bh
            (Nothing, Just _) -> error $ "Lost block that was stored as finalized" ++ show bh
            _ -> error $ "Lost block and finalization record" ++ show bh
        _ -> return Nothing
    makeLiveBlock block parent lastFin st arrTime energy = do
            blockP <- liftIO $ makeBlockPointerFromPendingBlock block parent lastFin st arrTime energy
            blockTable . at (getHash block) ?= BlockAlive blockP
            return blockP
    markDead bh = blockTable . at bh ?= BlockDead
    markFinalized bh fr = use (blockTable . at bh) >>= \case
            Just (BlockAlive bp) -> do
              writeBlock bp
              writeFinalizationRecord fr
              blockTable . at bh ?= BlockFinalized (finalizationIndex fr)
            _ -> return ()
    markPending pb = blockTable . at (getHash pb) ?= BlockPending pb
    getGenesisBlockPointer = use genesisBlockPointer
    getGenesisData = use genesisData
    getLastFinalized = use finalizationList >>= \case
            _ Seq.:|> (finRec,lf) -> return (lf, finRec)
            _ -> error "empty finalization list"
    getNextFinalizationIndex = FinalizationIndex . fromIntegral . Seq.length <$> use finalizationList
    addFinalization newFinBlock finRec = finalizationList %= (Seq.:|> (finRec, newFinBlock))
    getFinalizationAtIndex finIndex = Seq.lookup (fromIntegral finIndex) <$> use finalizationList
    getFinalizationFromIndex finIndex = toList . Seq.drop (fromIntegral finIndex) <$> use finalizationList
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
            tt <- use transactionTable
            let trHash = getHash tr
            case tt ^. ttHashMap . at trHash of
                Nothing ->
                  if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                    transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                                   & (ttHashMap . at (getHash tr) ?~ (tr, slot)))
                    return (TS.Added tr)
                  else return TS.ObsoleteNonce
                Just (tr', slot') -> do
                                when (slot > slot') $ transactionTable .= (tt & ttHashMap . at trHash ?~ (tr', slot))
                                return $ TS.Duplicate tr'
        where
            sender = transactionSender tr
            nonce = transactionNonce tr
    finalizeTransactions = mapM_ finTrans
        where
            finTrans tr = do
                let nonce = transactionNonce tr
                    sender = transactionSender tr
                anft <- use (transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT)
                assert (anft ^. anftNextNonce == nonce) $ do
                    let nfn = anft ^. anftMap . at nonce . non Set.empty
                    assert (Set.member tr nfn) $ do
                        -- Remove any other transactions with this nonce from the transaction table
                        forM_ (Set.delete tr nfn) $ \deadTransaction -> transactionTable . ttHashMap . at (getHash deadTransaction) .= Nothing
                        -- Update the non-finalized transactions for the sender
                        transactionTable . ttNonFinalizedTransactions . at sender ?= (anft & (anftMap . at nonce .~ Nothing) & (anftNextNonce .~ nonce + 1))
    commitTransaction slot tr =
        transactionTable . ttHashMap . at (getHash tr) %= fmap (_2 %~ max slot)
    purgeTransaction tr =
        use (transactionTable . ttHashMap . at (getHash tr)) >>= \case
            Nothing -> return True
            Just (_, slot) -> do
                lastFinSlot <- blockSlot . _bpBlock . fst <$> TS.getLastFinalized
                if lastFinSlot >= slot then do
                    let nonce = transactionNonce tr
                        sender = transactionSender tr
                    transactionTable . ttHashMap . at (getHash tr) .= Nothing
                    transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %= Set.delete tr
                    return True
                else return False
    lookupTransaction th =
        use (transactionTable . ttHashMap . at th) >>= \case
            Nothing -> return Nothing
            Just (tr, _) -> do
                nn <- use (transactionTable . ttNonFinalizedTransactions . at (transactionSender tr) . non emptyANFT . anftNextNonce)
                return $ Just (tr, transactionNonce tr < nn)
    updateBlockTransactions trs pb = return $ pb {pbBlock = (pbBlock pb) {bbTransactions = BlockTransactions trs}}

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters
