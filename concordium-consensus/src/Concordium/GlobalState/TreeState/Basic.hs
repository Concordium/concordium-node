{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase #-}
module Concordium.GlobalState.TreeState.Basic where

import Lens.Micro.Platform
import Data.List
import Data.Foldable
import Control.Monad.State
import Control.Exception

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Set as Set

import Concordium.GlobalState.Types
import Concordium.GlobalState.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Block
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Transactions

data SkovData = SkovData {
    -- |Map of all received blocks by hash.
    _skovBlockTable :: HM.HashMap BlockHash BlockStatus,
    _skovPossiblyPendingTable :: HM.HashMap BlockHash [PendingBlock],
    _skovPossiblyPendingQueue :: MPQ.MinPQueue Slot (BlockHash, BlockHash),
    _skovBlocksAwaitingLastFinalized :: MPQ.MinPQueue BlockHeight PendingBlock,
    _skovFinalizationList :: Seq.Seq (FinalizationRecord, BlockPointer),
    _skovFinalizationPool :: Map.Map FinalizationIndex [FinalizationRecord],
    _skovBranches :: Seq.Seq [BlockPointer],
    _skovGenesisData :: GenesisData,
    _skovGenesisBlockPointer :: BlockPointer,
    _skovFocusBlock :: BlockPointer,
    _skovPendingTransactions :: PendingTransactionTable,
    _skovTransactionTable :: TransactionTable,
    _skovStatistics :: ConsensusStatistics
}
makeLenses ''SkovData

instance Show SkovData where
    show SkovData{..} = "Finalized: " ++ intercalate "," (take 6 . show . bpHash . snd <$> toList _skovFinalizationList) ++ "\n" ++
        "Branches: " ++ intercalate "," ( (('[':) . (++"]") . intercalate "," . map (take 6 . show . bpHash)) <$> toList _skovBranches)

class SkovLenses s where
    skov :: Lens' s SkovData
    blockTable :: Lens' s (HM.HashMap BlockHash BlockStatus)
    blockTable = skov . skovBlockTable
    possiblyPendingTable :: Lens' s (HM.HashMap BlockHash [PendingBlock])
    possiblyPendingTable = skov . skovPossiblyPendingTable
    possiblyPendingQueue :: Lens' s (MPQ.MinPQueue Slot (BlockHash, BlockHash))
    possiblyPendingQueue = skov . skovPossiblyPendingQueue
    blocksAwaitingLastFinalized :: Lens' s (MPQ.MinPQueue BlockHeight PendingBlock)
    blocksAwaitingLastFinalized = skov . skovBlocksAwaitingLastFinalized
    finalizationList :: Lens' s (Seq.Seq (FinalizationRecord, BlockPointer))
    finalizationList = skov . skovFinalizationList
    finalizationPool :: Lens' s (Map.Map FinalizationIndex [FinalizationRecord])
    finalizationPool = skov . skovFinalizationPool
    branches :: Lens' s (Seq.Seq [BlockPointer])
    branches = skov . skovBranches
    genesisData :: Lens' s GenesisData
    genesisData = skov . skovGenesisData
    genesisBlockPointer :: Lens' s BlockPointer
    genesisBlockPointer = skov . skovGenesisBlockPointer
    focusBlock :: Lens' s BlockPointer
    focusBlock = skov . skovFocusBlock
    pendingTransactions :: Lens' s PendingTransactionTable
    pendingTransactions = skov . skovPendingTransactions
    transactionTable :: Lens' s TransactionTable
    transactionTable = skov . skovTransactionTable
    statistics :: Lens' s ConsensusStatistics
    statistics = skov . skovStatistics

instance SkovLenses SkovData where
    skov = id

initialSkovData :: GenesisData -> BlockState -> SkovData
initialSkovData gd genState = SkovData {
            _skovBlockTable = HM.singleton gbh (BlockFinalized gb gbfin),
            _skovPossiblyPendingTable = HM.empty,
            _skovPossiblyPendingQueue = MPQ.empty,
            _skovBlocksAwaitingLastFinalized = MPQ.empty,
            _skovFinalizationList = Seq.singleton (gbfin, gb),
            _skovFinalizationPool = Map.empty,
            _skovBranches = Seq.empty,
            _skovGenesisData = gd,
            _skovGenesisBlockPointer = gb,
            _skovFocusBlock = gb,
            _skovPendingTransactions = emptyPendingTransactionTable,
            _skovTransactionTable = emptyTransactionTable,
            _skovStatistics = initialConsensusStatistics
        }
    where
        gb = makeGenesisBlockPointer gd genState
        gbh = bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

newtype SkovTreeState s m a = SkovTreeState {runSkovTreeState :: m a}
    deriving (Functor, Monad, Applicative, MonadState s)

instance (SkovLenses s, Monad m, MonadState s m) => TreeStateMonad (SkovTreeState s m) where
    getBlockStatus bh = use (blockTable . at bh)
    markDead bh = blockTable . at bh ?= BlockDead
    markFinalized bh fr = use (blockTable . at bh) >>= \case
            Just (BlockAlive bp) -> blockTable . at bh ?= BlockFinalized bp fr
            _ -> return ()
    getGenesisBlockPointer = use genesisBlockPointer
    getGenesisData = use genesisData
    getLastFinalized = use finalizationList >>= \case
            _ Seq.:|> (_,lf) -> return lf
            _ -> error "empty finalization list"
    getNextFinalizationIndex = FinalizationIndex . fromIntegral . Seq.length <$> use finalizationList
    addFinalization newFinBlock finRec = finalizationList %= (Seq.:|> (finRec, newFinBlock))
    getBranches = use branches
    putBranches brs = branches .= brs
    takePendingChildren bh = possiblyPendingTable . at bh . non [] <<.= []
    addPendingBlock pb = do
        let parent = blockPointer (pbBlock pb)
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
    addTransaction tr = do 
            tt <- use transactionTable
            case tt ^. ttHashMap . at (getHash tr) of
                Nothing -> if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                                transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                                        & (ttHashMap . at (getHash tr) ?~ (tr, 0)))
                            else return ()
                _ -> return ()
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
                lastFinSlot <- blockSlot . bpBlock <$> getLastFinalized
                if (lastFinSlot >= slot) then do
                    let nonce = transactionNonce tr
                        sender = transactionSender tr
                    transactionTable . ttHashMap . at (getHash tr) .= Nothing
                    transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %= Set.delete tr
                    return True
                else return False
    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats