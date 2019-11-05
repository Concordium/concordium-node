{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies, DerivingVia, StandaloneDeriving #-}
module Concordium.GlobalState.Basic.TreeState where

import Lens.Micro.Platform
import Data.List as List
import Data.Foldable
import Control.Monad.State
import Control.Exception
import Data.Serialize (runGet)

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Set as Set

import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Statistics (ConsensusStatistics, initialConsensusStatistics)
import Concordium.GlobalState.Transactions

import Concordium.GlobalState.Basic.Block
import Concordium.GlobalState.Basic.BlockPointer

data SkovData bs = SkovData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(HM.HashMap BlockHash (TS.BlockStatus (BasicBlockPointer bs) PendingBlock)),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |Priority queue of blocks waiting for their last finalized block to be finalized, ordered by height of the last finalized block
    _blocksAwaitingLastFinalized :: !(MPQ.MinPQueue BlockHeight PendingBlock),
    -- |List of finalization records with the blocks that they finalize, starting from genesis
    _finalizationList :: !(Seq.Seq (FinalizationRecord, BasicBlockPointer bs)),
    -- |Pending finalization records by finalization index
    _finalizationPool :: !(Map.Map FinalizationIndex [FinalizationRecord]),
    -- |Branches of the tree by height above the last finalized block
    _branches :: !(Seq.Seq [BasicBlockPointer bs]),
    -- |Genesis data
    _genesisData :: !GenesisData,
    -- |Block pointer to genesis block
    _genesisBlockPointer :: !(BasicBlockPointer bs),
    -- |Current focus block
    _focusBlock :: !(BasicBlockPointer bs),
    -- |Pending transaction table
    _pendingTransactions :: !PendingTransactionTable,
    -- |Transaction table
    _transactionTable :: !TransactionTable,
    -- |Consensus statistics
    _statistics :: !ConsensusStatistics,
    -- |Runtime parameters
    _runtimeParameters :: !RuntimeParameters
}
makeLenses ''SkovData

instance Show (SkovData bs) where
    show SkovData{..} = "Finalized: " ++ intercalate "," (take 6 . show . _bpHash . snd <$> toList _finalizationList) ++ "\n" ++
        "Branches: " ++ intercalate "," ( (('[':) . (++"]") . intercalate "," . map (take 6 . show . _bpHash)) <$> toList _branches)

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovDataDefault :: GenesisData -> bs -> (SkovData bs)
initialSkovDataDefault = initialSkovData defaultRuntimeParameters

initialSkovData :: RuntimeParameters -> GenesisData -> bs -> (SkovData bs)
initialSkovData rp gd genState = SkovData {
            _blockTable = HM.singleton gbh (TS.BlockFinalized gb gbfin),
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
            _runtimeParameters = rp
        }
    where
        gb = makeGenesisBlockPointer gd genState
        gbh = _bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

instance (bs ~ GS.BlockState m) => GS.GlobalStateTypes (GS.TreeStateM (SkovData bs) m) where
    type PendingBlock (GS.TreeStateM (SkovData bs) m) = PendingBlock
    type BlockPointer (GS.TreeStateM (SkovData bs) m) = BasicBlockPointer bs

instance (bs ~ GS.BlockState m, BS.BlockStateStorage m, Monad m, MonadState (SkovData bs) m) => TS.TreeStateMonad (GS.TreeStateM (SkovData bs) m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    importPendingBlock blockBS rectime =
        case runGet (getBlock $ utcTimeToTransactionTime rectime) blockBS of
            Left err -> return $ Left $ "Block deserialization failed: " ++ err
            Right (GenesisBlock {}) -> return $ Left $ "Block desrialization failed: unexpected genesis block"
            Right (NormalBlock block0) -> return $ Right $ makePendingBlock block0 rectime
    getBlockStatus bh = use (blockTable . at bh)
    makeLiveBlock block parent lastFin st arrTime energy = do
            let blockP = makeBasicBlockPointer block parent lastFin st arrTime energy
            blockTable . at (getHash block) ?= TS.BlockAlive blockP
            return blockP
    markDead bh = blockTable . at bh ?= TS.BlockDead
    markFinalized bh fr = use (blockTable . at bh) >>= \case
            Just (TS.BlockAlive bp) -> blockTable . at bh ?= TS.BlockFinalized bp fr
            _ -> return ()
    markPending pb = blockTable . at (getHash pb) ?= TS.BlockPending pb
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
                Just ((h, pb), balf') -> if (h <= bh) then do
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
                    -- transaction should be added, provided its signature checks out.
                    if verifyTransactionSignature tr then do
                      transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                              & (ttHashMap . at (getHash tr) ?~ (tr, slot)))
                      return (TS.Added tr)
                    else return TS.InvalidSignature
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
                if (lastFinSlot >= slot) then do
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
