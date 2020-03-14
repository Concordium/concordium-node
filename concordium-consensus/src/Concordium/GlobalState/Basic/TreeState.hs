{-# LANGUAGE TypeFamilies, TemplateHaskell, NumericUnderscores, ScopedTypeVariables, DataKinds, RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, DerivingStrategies, DerivingVia, StandaloneDeriving, UndecidableInstances #-}
module Concordium.GlobalState.Basic.TreeState where

import Lens.Micro.Platform
import Data.List as List
import Data.Foldable
import Control.Monad.State
import Control.Exception
import Data.Serialize (runGet)
import Data.Functor.Identity

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Set as Set

import Concordium.GlobalState.Types
import Concordium.GlobalState.Basic.BlockPointer
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Statistics (ConsensusStatistics, initialConsensusStatistics)
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.GlobalState.AccountTransactionIndex

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
    show SkovData{..} = "Finalized: " ++ intercalate "," (take 6 . show . bpHash . snd <$> toList _finalizationList) ++ "\n" ++
        "Branches: " ++ intercalate "," ( (('[':) . (++"]") . intercalate "," . map (take 6 . show . bpHash)) <$> toList _branches)

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovDataDefault :: GenesisData -> bs -> SkovData bs
initialSkovDataDefault = initialSkovData defaultRuntimeParameters

initialSkovData :: RuntimeParameters -> GenesisData -> bs -> SkovData bs
initialSkovData rp gd genState =
  SkovData {
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
  where gbh = bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
        gb = makeGenesisBasicBlockPointer gd genState

-- |Newtype wrapper that provides an implementation of the TreeStateMonad using a non-persistent tree state.
-- The underlying Monad must provide instances for:
--
-- * `BlockStateTypes`
-- * `BlockStateQuery`
-- * `BlockStateOperations`
-- * `BlockStateStorage`
-- * `MonadState (SkovData bs)`
--
-- This newtype establishes types for the @GlobalStateTypes@. The type variable @bs@ stands for the BlockState
-- type used in the implementation.
newtype PureTreeStateMonad bs m a = PureTreeStateMonad { runPureTreeStateMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, GS.BlockStateTypes,
            BS.BlockStateQuery, BS.BlockStateOperations, BS.BlockStateStorage)

deriving instance (Monad m, MonadState (SkovData bs) m) => MonadState (SkovData bs) (PureTreeStateMonad bs m)


instance (bs ~ GS.BlockState m) => GlobalStateTypes (PureTreeStateMonad bs m) where
    type PendingBlockType (PureTreeStateMonad bs m) = PendingBlock
    type BlockPointerType (PureTreeStateMonad bs m) = BasicBlockPointer bs

instance (bs ~ GS.BlockState m, Monad m, MonadState (SkovData bs) m) => BlockPointerMonad (PureTreeStateMonad bs m) where
    blockState = return . _bpState
    bpParent = return . runIdentity . _bpParent
    bpLastFinalized = return . runIdentity . _bpLastFinalized

instance ATITypes (PureTreeStateMonad bs m) where
  type ATIStorage (PureTreeStateMonad bs m) = ()

instance (Monad m) => PerAccountDBOperations (PureTreeStateMonad bs m) where
  -- default instance because ati = ()

instance (bs ~ GS.BlockState m, BS.BlockStateStorage m, Monad m, MonadIO m, MonadState (SkovData bs) m)
          => TS.TreeStateMonad (PureTreeStateMonad bs m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = do
        return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    importPendingBlock blockBS rectime =
        case runGet (getBlock (utcTimeToTransactionTime rectime)) blockBS of
            Left err -> return $ Left $ "Block deserialization failed: " ++ err
            Right (GenesisBlock {}) -> return $ Left $ "Block deserialization failed: unexpected genesis block"
            Right (NormalBlock block0) -> return $ Right $ makePendingBlock block0 rectime
    getBlockStatus bh = use (blockTable . at bh)
    makeLiveBlock block parent lastFin st () arrTime energy = do
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
                  when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at trHash . mapped . _2 %=  updateSlot slot
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
    lookupTransaction th =
       preuse (transactionTable . ttHashMap . ix th . _2)

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters
