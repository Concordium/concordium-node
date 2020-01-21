{-# LANGUAGE TypeFamilies, TemplateHaskell, NumericUnderscores, ScopedTypeVariables, DataKinds, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, DerivingStrategies, DerivingVia, StandaloneDeriving, UndecidableInstances #-}
module Concordium.GlobalState.Persistent.TreeState where

import Concordium.GlobalState.Basic.Block
import Concordium.GlobalState.Basic.BlockPointer
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.LMDB
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Control.Exception
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import Data.Serialize as S (runGet, put, get, runPut, runGet, runGetState, decode)
import qualified Data.Set as Set
import Lens.Micro.Platform
import Database.LMDB.Simple as L
import Concordium.GlobalState.Basic.Block as B
import Data.Time.Clock
import Concordium.Types.Transactions (utcTimeToTransactionTime)

-- |Skov data for the persistent tree state version that also holds the database handlers
data SkovPersistentData bs = SkovPersistentData {
    -- |TreeState structures
    _skov :: SkovData bs,
    -- | Database handlers
    _db :: DatabaseHandlers bs
}
makeLenses ''SkovPersistentData

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovPersistentDataDefault :: GenesisData -> bs -> ByteString -> IO (SkovPersistentData bs)
initialSkovPersistentDataDefault = initialSkovPersistentData defaultRuntimeParameters

initialSkovPersistentData :: RuntimeParameters -> GenesisData -> bs -> ByteString -> IO (SkovPersistentData bs)
initialSkovPersistentData rp gd genState serState = do
  initalSkov <- initialSkovData rp gd genState
  gb <- makeGenesisBlockPointer gd genState
  initialDb <- initialDatabaseHandlers gb serState
  return $ SkovPersistentData {
            _skov = initalSkov,
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
    type BlockPointer (PersistentTreeStateMonad bs m) = BasicBlockPointer bs

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
    bytes <- liftIO $ transaction env $ (L.get dbB bh :: L.Transaction ReadOnly (Maybe ByteString))
    case bytes of
      Just b -> do
        tm <- liftIO $ getCurrentTime
        case runGetState (B.getBlock (utcTimeToTransactionTime tm)) b 0 of
          Right (newBlock, rest) ->
           case runGetState S.get rest 0 of
             Right (bs, rest') -> do
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
    liftIO $ transaction env $ (L.get dbF bh :: L.Transaction ReadOnly (Maybe FinalizationRecord))
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
    makePendingBlock key slot parent bid pf n lastFin trs time = return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    importPendingBlock blockBS rectime =
        case runGet (getBlock $ utcTimeToTransactionTime rectime) blockBS of
            Left err -> return $ Left $ "Block deserialization failed: " ++ err
            Right (GenesisBlock {}) -> return $ Left $ "Block deserialization failed: unexpected genesis block"
            Right (NormalBlock block0) -> return $ Right $ makePendingBlock block0 rectime
    getBlockStatus bh = use (skov . blockTable . at bh)
    makeLiveBlock block parent lastFin st arrTime energy = do
            blockP <- liftIO $ makeBlockPointerFromPendingBlock block parent lastFin st arrTime energy
            skov . blockTable . at (getHash block) ?= TS.BlockAlive blockP
            return blockP
    markDead bh = skov . blockTable . at bh ?= TS.BlockDead
    markFinalized bh fr = use (skov . blockTable . at bh) >>= \case
            Just (TS.BlockAlive _) -> do
              skov . blockTable . at bh ?= TS.BlockFinalized (finalizationIndex fr)
            _ -> return ()
    markPending pb = skov . blockTable . at (getHash pb) ?= TS.BlockPending pb
    getGenesisBlockPointer = use (skov . genesisBlockPointer)
    getGenesisData = use (skov . genesisData)
    getLastFinalized = use (skov . finalizationList) >>= \case
            _ Seq.:|> (finRec,lf) -> return (lf, finRec)
            _ -> error "empty finalization list"
    getNextFinalizationIndex = FinalizationIndex . fromIntegral . Seq.length <$> use (skov . finalizationList)
    addFinalization newFinBlock finRec = do
      (skov . finalizationList) %= (Seq.:|> (finRec, newFinBlock))
    getFinalizationAtIndex finIndex = Seq.lookup (fromIntegral finIndex) <$> use (skov . finalizationList)
    getFinalizationFromIndex finIndex = toList . Seq.drop (fromIntegral finIndex) <$> use (skov . finalizationList)
    getBranches = use (skov . branches)
    putBranches brs = skov . branches .= brs
    takePendingChildren bh = skov . possiblyPendingTable . at bh . non [] <<.= []
    addPendingBlock pb = do
        let parent = blockPointer (bbFields (pbBlock pb))
        skov . possiblyPendingTable . at parent . non [] %= (pb:)
        skov . possiblyPendingQueue %= MPQ.insert (blockSlot (pbBlock pb)) (getHash pb, parent)
    takeNextPendingUntil slot = tnpu =<< use (skov . possiblyPendingQueue)
        where
            tnpu ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, (pbh, parenth)), ppq') ->
                    if sl <= slot then do
                        (myPB, otherPBs) <- partition ((== pbh) . pbHash) <$> use (skov . possiblyPendingTable . at parenth . non [])
                        case myPB of
                            [] -> tnpu ppq'
                            (realPB : _) -> do
                                skov . possiblyPendingTable . at parenth . non [] .= otherPBs
                                skov . possiblyPendingQueue .= ppq'
                                return (Just realPB)
                    else do
                        skov . possiblyPendingQueue .= ppq
                        return Nothing
                Nothing -> do
                    skov . possiblyPendingQueue .= ppq
                    return Nothing
    addAwaitingLastFinalized bh pb = skov . blocksAwaitingLastFinalized %= MPQ.insert bh pb
    takeAwaitingLastFinalizedUntil bh =
            (MPQ.minViewWithKey <$> use (skov . blocksAwaitingLastFinalized)) >>= \case
                Nothing -> return Nothing
                Just ((h, pb), balf') -> if (h <= bh) then do
                                            skov . blocksAwaitingLastFinalized .= balf'
                                            return (Just pb)
                                        else return Nothing
    getFinalizationPoolAtIndex fi = use (skov . finalizationPool . at fi . non [])
    putFinalizationPoolAtIndex fi frs = skov . finalizationPool . at fi . non [] .= frs
    addFinalizationRecordToPool fr = skov . finalizationPool . at (finalizationIndex fr) . non [] %= (fr :)
    getFocusBlock = use (skov . focusBlock)
    putFocusBlock bb = skov . focusBlock .= bb
    getPendingTransactions = use (skov . pendingTransactions)
    putPendingTransactions pts = skov . pendingTransactions .= pts
    getAccountNonFinalized addr nnce =
            use (skov . transactionTable . ttNonFinalizedTransactions . at addr) >>= \case
                Nothing -> return []
                Just anfts ->
                    let (_, atnnce, beyond) = Map.splitLookup nnce (anfts ^. anftMap)
                    in return $ case atnnce of
                        Nothing -> Map.toAscList beyond
                        Just s -> (nnce, s) : Map.toAscList beyond
    addCommitTransaction tr slot = do
            tt <- use (skov . transactionTable)
            let trHash = getHash tr
            case tt ^. ttHashMap . at trHash of
                Nothing ->
                  if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                    skov . transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                                   & (ttHashMap . at (getHash tr) ?~ (tr, slot)))
                    return (TS.Added tr)
                  else return TS.ObsoleteNonce
                Just (tr', slot') -> do
                                when (slot > slot') $ skov . transactionTable .= (tt & ttHashMap . at trHash ?~ (tr', slot))
                                return $ TS.Duplicate tr'
        where
            sender = transactionSender tr
            nonce = transactionNonce tr
    finalizeTransactions = mapM_ finTrans
        where
            finTrans tr = do
                let nonce = transactionNonce tr
                    sender = transactionSender tr
                anft <- use (skov . transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT)
                assert (anft ^. anftNextNonce == nonce) $ do
                    let nfn = anft ^. anftMap . at nonce . non Set.empty
                    assert (Set.member tr nfn) $ do
                        -- Remove any other transactions with this nonce from the transaction table
                        forM_ (Set.delete tr nfn) $ \deadTransaction -> skov . transactionTable . ttHashMap . at (getHash deadTransaction) .= Nothing
                        -- Update the non-finalized transactions for the sender
                        skov . transactionTable . ttNonFinalizedTransactions . at sender ?= (anft & (anftMap . at nonce .~ Nothing) & (anftNextNonce .~ nonce + 1))
    commitTransaction slot tr =
        skov . transactionTable . ttHashMap . at (getHash tr) %= fmap (_2 %~ max slot)
    purgeTransaction tr =
        use (skov . transactionTable . ttHashMap . at (getHash tr)) >>= \case
            Nothing -> return True
            Just (_, slot) -> do
                lastFinSlot <- blockSlot . _bpBlock . fst <$> TS.getLastFinalized
                if (lastFinSlot >= slot) then do
                    let nonce = transactionNonce tr
                        sender = transactionSender tr
                    skov . transactionTable . ttHashMap . at (getHash tr) .= Nothing
                    skov . transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %= Set.delete tr
                    return True
                else return False
    lookupTransaction th =
        use (skov . transactionTable . ttHashMap . at th) >>= \case
            Nothing -> return Nothing
            Just (tr, _) -> do
                nn <- use (skov . transactionTable . ttNonFinalizedTransactions . at (transactionSender tr) . non emptyANFT . anftNextNonce)
                return $ Just (tr, transactionNonce tr < nn)
    updateBlockTransactions trs pb = return $ pb {pbBlock = (pbBlock pb) {bbTransactions = BlockTransactions trs}}

    getConsensusStatistics = use (skov . statistics)
    putConsensusStatistics stats = skov . statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use (skov . runtimeParameters)
