{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE DerivingStrategies, DerivingVia #-}
module Concordium.GlobalState.Persistent.TreeState where
{-
import Lens.Micro.Platform
import Data.List as List
import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import Data.Hashable hiding (unhashed, hashed)
import Data.Time
import Data.Time.Clock.POSIX
import Data.IORef

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Set as Set

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Statistics (ConsensusStatistics, initialConsensusStatistics)
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.Rewards as Rewards

import Concordium.GlobalState.Basic.Block
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.BlobStore

data PersistentBlockPointer = PersistentBlockPointer {
    -- |Hash of the block
    _bpHash :: !BlockHash,
    -- |The block itself
    _bpBlock :: !Block,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: PersistentBlockPointer,
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: PersistentBlockPointer,
    -- |Height of the block in the tree
    _bpHeight :: !BlockHeight,
    -- |The handle for accessing the state (of accounts, contracts, etc.) after execution of the block.
    _bpState :: !PersistentBlockState,
    -- |Time at which the block was first received
    _bpReceiveTime :: UTCTime,
    -- |Time at which the block was first considered part of the tree (validated)
    _bpArriveTime :: UTCTime,
    -- |Number of transactions in a block
    _bpTransactionCount :: Int,
    -- |Energy cost of all transactions in the block.
    _bpTransactionsEnergyCost :: !Energy,
    -- |Size of the transaction data in bytes.
    _bpTransactionsSize :: !Int
}

instance Eq PersistentBlockPointer where
    bp1 == bp2 = _bpHash bp1 == _bpHash bp2

instance Ord PersistentBlockPointer where
    compare bp1 bp2 = compare (_bpHash bp1) (_bpHash bp2)

instance Hashable PersistentBlockPointer where
    hashWithSalt s = hashWithSalt s . _bpHash
    hash = hash . _bpHash

instance Show PersistentBlockPointer where
    show = show . _bpHash

instance HashableTo Hash.Hash PersistentBlockPointer where
    getHash = _bpHash

type instance BlockFieldType PersistentBlockPointer = BlockFields

instance BlockData PersistentBlockPointer where
    blockSlot = blockSlot . _bpBlock
    blockFields = blockFields . _bpBlock
    blockTransactions = blockTransactions . _bpBlock
    verifyBlockSignature key = verifyBlockSignature key . _bpBlock
    putBlock = putBlock . _bpBlock
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE verifyBlockSignature #-}
    {-# INLINE putBlock #-}

-- |Make a 'PersistentBlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBlockPointer ::
    PendingBlock        -- ^Pending block
    -> PersistentBlockPointer     -- ^Parent block pointer
    -> PersistentBlockPointer     -- ^Last finalized block pointer
    -> PersistentBlockState       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> Energy           -- ^Energy cost of all transactions in the block
    -> PersistentBlockPointer
makeBlockPointer pb _bpParent _bpLastFinalized _bpState _bpArriveTime _bpTransactionsEnergyCost
        = assert (getHash _bpParent == blockPointer bf) $
            assert (getHash _bpLastFinalized == blockLastFinalized bf) $
                PersistentBlockPointer {
                    _bpHash = getHash pb,
                    _bpBlock = NormalBlock (pbBlock pb),
                    _bpHeight = _bpHeight _bpParent + 1,
                    _bpReceiveTime = pbReceiveTime pb,
                    ..}
    where
        bf = bbFields $ pbBlock pb
        (_bpTransactionCount, _bpTransactionsSize) = List.foldl' (\(clen, csize) tx -> (clen + 1, trSize tx + csize)) (0, 0) (blockTransactions pb)


makeGenesisBlockPointer :: GenesisData -> PersistentBlockState -> PersistentBlockPointer
makeGenesisBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = PersistentBlockPointer {..}
        _bpBlock = makeGenesisBlock genData
        _bpHash = getHash _bpBlock
        _bpParent = theBlockPointer
        _bpLastFinalized = theBlockPointer
        _bpHeight = 0
        _bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
        _bpArriveTime = _bpReceiveTime
        _bpTransactionCount = 0
        _bpTransactionsEnergyCost = 0
        _bpTransactionsSize = 0


instance BS.BlockPointerData PersistentBlockPointer where
    type BlockState' PersistentBlockPointer = PersistentBlockState

    bpHash = _bpHash
    bpParent = _bpParent
    bpLastFinalized = _bpLastFinalized
    bpHeight = _bpHeight
    bpState = _bpState
    bpReceiveTime = _bpReceiveTime
    bpArriveTime = _bpArriveTime
    bpTransactionCount = _bpTransactionCount
    bpTransactionsEnergyCost = _bpTransactionsEnergyCost
    bpTransactionsSize = _bpTransactionsSize


data SkovData = SkovData {
    -- |Map of all received blocks by hash.
    _skovBlockTable :: !(HM.HashMap BlockHash (TS.BlockStatus PersistentBlockPointer PendingBlock)),
    _skovPossiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    _skovPossiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    _skovBlocksAwaitingLastFinalized :: !(MPQ.MinPQueue BlockHeight PendingBlock),
    _skovFinalizationList :: !(Seq.Seq (FinalizationRecord, PersistentBlockPointer)),
    _skovFinalizationPool :: !(Map.Map FinalizationIndex [FinalizationRecord]),
    _skovBranches :: !(Seq.Seq [PersistentBlockPointer]),
    _skovGenesisData :: !GenesisData,
    _skovGenesisBlockPointer :: !PersistentBlockPointer,
    _skovFocusBlock :: !PersistentBlockPointer,
    _skovPendingTransactions :: !PendingTransactionTable,
    _skovTransactionTable :: !TransactionTable,
    _skovStatistics :: !ConsensusStatistics,
    _skovRuntimeParameters :: !RuntimeParameters
}
makeLenses ''SkovData

instance Show SkovData where
    show SkovData{..} = "Finalized: " ++ intercalate "," (take 6 . show . _bpHash . snd <$> toList _skovFinalizationList) ++ "\n" ++
        "Branches: " ++ intercalate "," ( (('[':) . (++"]") . intercalate "," . map (take 6 . show . _bpHash)) <$> toList _skovBranches)

class SkovLenses s where
    skov :: Lens' s SkovData
    blockTable :: Lens' s (HM.HashMap BlockHash (TS.BlockStatus PersistentBlockPointer PendingBlock))
    blockTable = skov . skovBlockTable
    possiblyPendingTable :: Lens' s (HM.HashMap BlockHash [PendingBlock])
    possiblyPendingTable = skov . skovPossiblyPendingTable
    possiblyPendingQueue :: Lens' s (MPQ.MinPQueue Slot (BlockHash, BlockHash))
    possiblyPendingQueue = skov . skovPossiblyPendingQueue
    blocksAwaitingLastFinalized :: Lens' s (MPQ.MinPQueue BlockHeight PendingBlock)
    blocksAwaitingLastFinalized = skov . skovBlocksAwaitingLastFinalized
    finalizationList :: Lens' s (Seq.Seq (FinalizationRecord, PersistentBlockPointer))
    finalizationList = skov . skovFinalizationList
    finalizationPool :: Lens' s (Map.Map FinalizationIndex [FinalizationRecord])
    finalizationPool = skov . skovFinalizationPool
    branches :: Lens' s (Seq.Seq [PersistentBlockPointer])
    branches = skov . skovBranches
    genesisData :: Lens' s GenesisData
    genesisData = skov . skovGenesisData
    genesisBlockPointer :: Lens' s PersistentBlockPointer
    genesisBlockPointer = skov . skovGenesisBlockPointer
    focusBlock :: Lens' s PersistentBlockPointer
    focusBlock = skov . skovFocusBlock
    pendingTransactions :: Lens' s PendingTransactionTable
    pendingTransactions = skov . skovPendingTransactions
    transactionTable :: Lens' s TransactionTable
    transactionTable = skov . skovTransactionTable
    statistics :: Lens' s ConsensusStatistics
    statistics = skov . skovStatistics
    runtimeParameters :: Lens' s RuntimeParameters
    runtimeParameters = skov . skovRuntimeParameters

instance SkovLenses SkovData where
    skov = id

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovDataDefault :: GenesisData -> PersistentBlockState -> SkovData
initialSkovDataDefault = initialSkovData defaultRuntimeParameters

initialSkovData :: RuntimeParameters -> GenesisData -> PersistentBlockState -> SkovData
initialSkovData rp gd genState = SkovData {
            _skovBlockTable = HM.singleton gbh (TS.BlockFinalized gb gbfin),
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
            _skovStatistics = initialConsensusStatistics,
            _skovRuntimeParameters = rp
        }
    where
        gb = makeGenesisBlockPointer gd genState
        gbh = _bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

newtype SkovTreeState r s m a = SkovTreeState {runSkovTreeState :: m a}
    deriving (Functor, Monad, Applicative, MonadState s, MonadReader r, MonadIO)

type instance TS.PendingBlock (SkovTreeState r s m) = PendingBlock
type instance BS.BlockPointer (SkovTreeState r s m) = PersistentBlockPointer
type instance BS.UpdatableBlockState (SkovTreeState r s m) = PersistentBlockState

instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r) => BS.BlockStateQuery (SkovTreeState r s m) where
    getModule = doGetModule
    getAccount = doGetAccount
    getContractInstance = doGetInstance
    getModuleList = doGetModuleList
    getAccountList = doAccountList
    getContractInstanceList = doContractInstanceList
    getBlockBirkParameters = doGetBlockBirkParameters
    getRewardStatus = doGetRewardStatus
    getTransactionOutcome = doGetTransactionOutcome
    getSpecialOutcomes = doGetSpecialOutcomes
    {-# INLINE getModule #-}
    {-# INLINE getAccount #-}
    {-# INLINE getContractInstance #-}
    {-# INLINE getModuleList #-}
    {-# INLINE getAccountList #-}
    {-# INLINE getContractInstanceList #-}
    {-# INLINE getBlockBirkParameters #-}
    {-# INLINE getRewardStatus #-}
    {-# INLINE getTransactionOutcome #-}
    {-# INLINE getSpecialOutcomes #-}

instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r) => BS.BlockStateOperations (SkovTreeState r s m) where
    bsoGetModule = doGetModule
    bsoGetAccount = doGetAccount
    bsoGetInstance = doGetInstance
    bsoRegIdExists = doRegIdExists
    bsoPutNewAccount = doPutNewAccount
    bsoPutNewInstance = doPutNewInstance
    bsoPutNewModule = doPutNewModule
    bsoTryGetLinkedExpr = doTryGetLinkedExpr
    bsoPutLinkedExpr = doPutLinkedExpr
    bsoTryGetLinkedContract = doTryGetLinkedContract
    bsoPutLinkedContract = doPutLinkedContract
    bsoModifyAccount = doModifyAccount
    bsoModifyInstance = doModifyInstance
    bsoNotifyExecutionCost = doNotifyExecutionCost
    bsoNotifyIdentityIssuerCredential = doNotifyIdentityIssuerCredential
    bsoGetExecutionCost = doGetExecutionCost
    bsoGetBlockBirkParameters = doGetBlockBirkParameters
    bsoAddBaker = doAddBaker
    bsoUpdateBaker = doUpdateBaker
    bsoRemoveBaker = doRemoveBaker
    bsoSetInflation = doSetInflation
    bsoMint = doMint
    bsoDecrementCentralBankGTU = doDecrementCentralBankGTU
    bsoDelegateStake = doDelegateStake
    bsoGetIdentityProvider = doGetIdentityProvider
    bsoGetCryptoParams = doGetCryptoParams
    bsoSetTransactionOutcomes = doSetTransactionOutcomes
    bsoAddSpecialTransactionOutcome = doAddSpecialTransactionOutcome
    bsoUpdateSeedState = doUpdateSeedState
    {-# INLINE bsoGetModule #-}
    {-# INLINE bsoGetAccount #-}
    {-# INLINE bsoGetInstance #-}
    {-# INLINE bsoRegIdExists #-}
    {-# INLINE bsoPutNewAccount #-}
    {-# INLINE bsoPutNewInstance #-}
    {-# INLINE bsoPutNewModule #-}
    {-# INLINE bsoTryGetLinkedExpr #-}
    {-# INLINE bsoPutLinkedExpr #-}
    {-# INLINE bsoTryGetLinkedContract #-}
    {-# INLINE bsoPutLinkedContract #-}
    {-# INLINE bsoModifyAccount #-}
    {-# INLINE bsoModifyInstance #-}
    {-# INLINE bsoNotifyExecutionCost #-}
    {-# INLINE bsoNotifyIdentityIssuerCredential #-}
    {-# INLINE bsoGetExecutionCost #-}
    {-# INLINE bsoGetBlockBirkParameters #-}
    {-# INLINE bsoAddBaker #-}
    {-# INLINE bsoUpdateBaker #-}
    {-# INLINE bsoRemoveBaker #-}
    {-# INLINE bsoSetInflation #-}
    {-# INLINE bsoMint #-}
    {-# INLINE bsoDecrementCentralBankGTU #-}
    {-# INLINE bsoDelegateStake #-}
    {-# INLINE bsoGetIdentityProvider #-}
    {-# INLINE bsoGetCryptoParams #-}
    {-# INLINE bsoSetTransactionOutcomes #-}
    {-# INLINE bsoAddSpecialTransactionOutcome #-}
    {-# INLINE bsoUpdateSeedState #-}


instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r, SkovLenses s, Monad m, MonadState s m) => TS.TreeStateMonad (SkovTreeState r s m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs) time
    getBlockStatus bh = use (blockTable . at bh)
    makeLiveBlock block parent lastFin st arrTime energy = do
            let blockP = makeBlockPointer block parent lastFin st arrTime energy
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
    getFinalizationAtIndex finIndex = fmap fst . Seq.lookup (fromIntegral finIndex) <$> use finalizationList
    getFinalizationFromIndex finIndex = toList . fmap fst . Seq.drop (fromIntegral finIndex) <$> use finalizationList
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
                Nothing -> if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                                transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                                        & (ttHashMap . at (getHash tr) ?~ (tr, slot)))
                                return $ Just (tr, True)
                            else return Nothing
                Just (tr', slot') -> do
                                when (slot > slot') $ transactionTable .= (tt & ttHashMap . at trHash ?~ (tr', slot))
                                return $ Just (tr', False)
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

    {-# INLINE thawBlockState #-}
    thawBlockState pbs = do
            bsp <- loadPBS pbs
            liftIO $ newIORef $! BRMemory bsp {
                    bspBank = bspBank bsp & Rewards.executionCost .~ 0 & Rewards.identityIssuersRewards .~ HM.empty
                }

    {-# INLINE freezeBlockState #-}
    freezeBlockState pbs = do
        {-
        inner <- liftIO $ readIORef pbs
        inner' <- uncacheBuffered inner
        liftIO $ writeIORef pbs inner'
        -}
        return pbs

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState pbs = liftIO $ writeIORef pbs (error "Block state dropped")

    {-# INLINE purgeBlockState #-}
    purgeBlockState pbs = liftIO $ writeIORef pbs (error "Block state purged")

    {-# INLINE archiveBlockState #-}
    archiveBlockState pbs = do
        inner <- liftIO $ readIORef pbs
        inner' <- uncacheBuffered inner
        liftIO $ writeIORef pbs inner'

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters
-}