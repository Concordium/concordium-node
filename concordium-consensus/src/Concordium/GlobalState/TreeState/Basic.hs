{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase #-}
module Concordium.GlobalState.TreeState.Basic where

import Lens.Micro.Platform
import Data.List as List
import Data.Foldable
import Control.Monad.State
import Control.Exception

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Set as Set

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.Modules as Modules
import qualified Concordium.GlobalState.Account as Account
import qualified Concordium.GlobalState.Instances as Instances

import qualified Concordium.Crypto.SHA256 as Hash

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Hashable hiding (unhashed, hashed)


data BlockState = BlockState {
    _blockAccounts :: Account.Accounts,
    _blockInstances :: Instances.Instances,
    _blockModules :: Modules.Modules
}

makeLenses ''BlockState

emptyBlockState :: BlockState
emptyBlockState = BlockState {
  _blockAccounts = Account.emptyAccounts
  , _blockInstances = Instances.emptyInstances
  , _blockModules = Modules.emptyModules
  }


data BlockPointer = BlockPointer {
    -- |Hash of the block
    _bpHash :: !BlockHash,
    -- |The block itself
    _bpBlock :: !Block,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: BlockPointer,
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: BlockPointer,
    -- |Height of the block in the tree
    _bpHeight :: !BlockHeight,
    -- |The handle for accessing the state (of accounts, contracts, etc.) after execution of the block.
    _bpState :: !BlockState,
    -- |Time at which the block was first received
    _bpReceiveTime :: UTCTime,
    -- |Time at which the block was first considered part of the tree (validated)
    _bpArriveTime :: UTCTime,
    -- |Number of transactions in a block
    _bpTransactionCount :: Int
}

instance Eq BlockPointer where
    bp1 == bp2 = _bpHash bp1 == _bpHash bp2

instance Ord BlockPointer where
    compare bp1 bp2 = compare (_bpHash bp1) (_bpHash bp2)

instance Hashable BlockPointer where
    hashWithSalt s = hashWithSalt s . _bpHash
    hash = hash . _bpHash

instance Show BlockPointer where
    show = show . _bpHash

instance HashableTo Hash.Hash BlockPointer where
    getHash = _bpHash

instance BlockData BlockPointer where
    blockSlot = blockSlot . _bpBlock
    blockFields = blockFields . _bpBlock
    blockTransactions = blockTransactions . _bpBlock
    verifyBlockSignature key = verifyBlockSignature key . _bpBlock

-- |Make a 'BlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBlockPointer ::
    PendingBlock        -- ^Pending block
    -> BlockPointer     -- ^Parent block pointer
    -> BlockPointer     -- ^Last finalized block pointer
    -> BlockState       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> BlockPointer
makeBlockPointer pb _bpParent _bpLastFinalized _bpState _bpArriveTime
        = assert (getHash _bpParent == blockPointer bf) $
            assert (getHash _bpLastFinalized == blockLastFinalized bf) $
                BlockPointer {
                    _bpHash = getHash pb,
                    _bpBlock = NormalBlock (pbBlock pb),
                    _bpHeight = _bpHeight _bpParent + 1,
                    _bpReceiveTime = pbReceiveTime pb,
                    _bpTransactionCount = length (blockTransactions pb),
                    ..}
    where
        bf = bbFields $ pbBlock pb


makeGenesisBlockPointer :: GenesisData -> BlockState -> BlockPointer
makeGenesisBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = BlockPointer {..}
        _bpBlock = makeGenesisBlock genData
        _bpHash = getHash _bpBlock
        _bpParent = theBlockPointer
        _bpLastFinalized = theBlockPointer
        _bpHeight = 0
        _bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
        _bpArriveTime = _bpReceiveTime
        _bpTransactionCount = 0


data SkovData = SkovData {
    -- |Map of all received blocks by hash.
    _skovBlockTable :: HM.HashMap BlockHash (TS.BlockStatus BlockPointer),
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
    _skovStatistics :: TS.ConsensusStatistics
}
makeLenses ''SkovData

instance Show SkovData where
    show SkovData{..} = "Finalized: " ++ intercalate "," (take 6 . show . _bpHash . snd <$> toList _skovFinalizationList) ++ "\n" ++
        "Branches: " ++ intercalate "," ( (('[':) . (++"]") . intercalate "," . map (take 6 . show . _bpHash)) <$> toList _skovBranches)

class SkovLenses s where
    skov :: Lens' s SkovData
    blockTable :: Lens' s (HM.HashMap BlockHash (TS.BlockStatus BlockPointer))
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
    statistics :: Lens' s TS.ConsensusStatistics
    statistics = skov . skovStatistics

instance SkovLenses SkovData where
    skov = id

initialSkovData :: GenesisData -> BlockState -> SkovData
initialSkovData gd genState = SkovData {
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
            _skovStatistics = TS.initialConsensusStatistics
        }
    where
        gb = makeGenesisBlockPointer gd genState
        gbh = _bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

newtype SkovTreeState s m a = SkovTreeState {runSkovTreeState :: m a}
    deriving (Functor, Monad, Applicative, MonadState s)

instance TS.BlockPointerData BlockPointer where
    type BlockState' BlockPointer = BlockState

    bpHash = _bpHash
    bpBlock = _bpBlock
    bpParent = _bpParent
    bpLastFinalized = _bpLastFinalized
    bpHeight = _bpHeight
    bpState = _bpState
    bpReceiveTime = _bpReceiveTime
    bpArriveTime = _bpArriveTime
    bpTransactionCount = _bpTransactionCount

instance (Monad m, MonadState s m) => TS.BlockStateQuery (SkovTreeState s m) where

    {-# INLINE getModule #-}
    getModule bs mref = 
      let Modules.Modules{..} = bs ^. blockModules
      in return $ HM.lookup mref _modules

    {-# INLINE getContractInstance #-}
    getContractInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE getAccount #-}
    getAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE getModuleList #-}
    getModuleList bs = 
      let Modules.Modules{..} = bs ^. blockModules
      in return $ HM.keys _modules

    {-# INLINE getContractInstanceList #-}
    getContractInstanceList bs = return (bs ^.. blockInstances . Instances.foldInstances)

    {-# INLINE getAccountList #-}
    getAccountList bs =
      return $ Map.keys (Account.accountMap (bs ^. blockAccounts))
  

instance (Monad m, MonadState s m) => TS.BlockStateOperations (SkovTreeState s m) where
    type UpdatableBlockState (SkovTreeState s m) = BlockState

    {-# INLINE bsoGetModule #-}
    bsoGetModule bs mref = 
      let Modules.Modules{..} = bs ^. blockModules
      in return $ HM.lookup mref _modules

    {-# INLINE bsoGetInstance #-}
    bsoGetInstance bs caddr = return (Instances.getInstance caddr (bs ^. blockInstances))

    {-# INLINE bsoGetAccount #-}
    bsoGetAccount bs aaddr =
      return $ bs ^? blockAccounts . ix aaddr

    {-# INLINE bsoRegIdExists #-}
    bsoRegIdExists bs regid = return (Account.regIdExists regid (bs ^. blockAccounts))

    {-# INLINE bsoPutNewAccount #-}
    bsoPutNewAccount bs acc = return $
        if Account.exists addr accounts then
          (False, bs)
        else
          (True, bs & blockAccounts .~ Account.putAccount acc accounts)
        where accounts = bs ^. blockAccounts
              addr = acc ^. accountAddress

    bsoPutNewInstance bs mkInstance = return $
        let (caddr, instances') = Instances.createInstance mkInstance (bs ^. blockInstances)
        in (caddr, bs & blockInstances .~ instances')

    bsoPutNewModule bs mref iface viface = return $
        case Modules.putInterfaces mref iface viface (bs ^. blockModules) of
          Nothing -> (False, bs)
          Just mods' -> (True, bs & blockModules .~ mods')

    bsoModifyInstance bs caddr amount model = return $
        bs & blockInstances %~ Instances.updateInstanceAt caddr amount model

    bsoModifyAccount bs account = return $
        bs & blockAccounts %~ Account.putAccount account

instance (SkovLenses s, Monad m, MonadState s m) => TS.TreeStateMonad (SkovTreeState s m) where
    type BlockPointer (SkovTreeState s m) = BlockPointer
    getBlockStatus bh = use (blockTable . at bh)
    makeLiveBlock block parent lastFin st arrTime = do
            let blockP = makeBlockPointer block parent lastFin st arrTime
            blockTable . at (getHash block) ?= TS.BlockAlive blockP
            return blockP
    markDead bh = blockTable . at bh ?= TS.BlockDead
    markFinalized bh fr = use (blockTable . at bh) >>= \case
            Just (TS.BlockAlive bp) -> blockTable . at bh ?= TS.BlockFinalized bp fr
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
            case tt ^. ttHashMap . at (getHash tr) of
                Nothing -> if (tt ^. ttNonFinalizedTransactions . at sender . non emptyANFT . anftNextNonce) <= nonce then do
                                transactionTable .= (tt & (ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %~ Set.insert tr)
                                                        & (ttHashMap . at (getHash tr) ?~ (tr, slot)))
                                return True
                            else return False
                Just (_, slot') -> do
                                when (slot > slot') $ transactionTable .= (tt & ttHashMap . at (getHash tr) ?~ (tr, slot))
                                return False
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
                lastFinSlot <- blockSlot . _bpBlock <$> TS.getLastFinalized
                if (lastFinSlot >= slot) then do
                    let nonce = transactionNonce tr
                        sender = transactionSender tr
                    transactionTable . ttHashMap . at (getHash tr) .= Nothing
                    transactionTable . ttNonFinalizedTransactions . at sender . non emptyANFT . anftMap . at nonce . non Set.empty %= Set.delete tr
                    return True
                else return False

    {-# INLINE thawBlockState #-}
    thawBlockState = return

    {-# INLINE freezeBlockState #-}
    freezeBlockState = return

    {-# INLINE purgeBlockState #-}
    purgeBlockState _ = return ()

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats
