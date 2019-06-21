{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase #-}
module Concordium.GlobalState.Basic.TreeState where

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

import Concordium.ID.Types(cdi_regId)
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Statistics (ConsensusStatistics, initialConsensusStatistics)
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.Modules as Modules
import qualified Concordium.GlobalState.Account as Account
import qualified Concordium.GlobalState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards

import Concordium.GlobalState.Basic.BlockState

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
    _skovStatistics :: ConsensusStatistics
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
    statistics :: Lens' s ConsensusStatistics
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
            _skovStatistics = initialConsensusStatistics
        }
    where
        gb = makeGenesisBlockPointer gd genState
        gbh = _bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

newtype SkovTreeState s m a = SkovTreeState {runSkovTreeState :: m a}
    deriving (Functor, Monad, Applicative, MonadState s)

instance (Monad m, MonadState s m) => BS.BlockStateQuery (SkovTreeState s m) where

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
  
    {-# INLINE getBirkParameters #-}
    getBirkParameters = return . _blockBirkParameters

    {-# INLINE getRewardStatus #-}
    getRewardStatus = return . _blockBank

type instance BS.UpdatableBlockState (SkovTreeState s m) = BlockState

instance (Monad m, MonadState s m) => BS.BlockStateOperations (SkovTreeState s m) where

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

    bsoPutNewModule bs mref iface viface source = return $
        case Modules.putInterfaces mref iface viface source (bs ^. blockModules) of
          Nothing -> (False, bs)
          Just mods' -> (True, bs & blockModules .~ mods')

    bsoModifyInstance bs caddr amount model = return $
        bs & blockInstances %~ Instances.updateInstanceAt caddr amount model

    bsoModifyAccount bs accountUpdates = return $
        let account = bs ^. blockAccounts . singular (ix (accountUpdates ^. BS.auAddress))
            updatedAccount = BS.updateAccount accountUpdates account
        in case accountUpdates ^. BS.auCredential of
             Nothing -> bs & blockAccounts %~ Account.putAccount updatedAccount
             Just cdi ->
               bs & blockAccounts %~ Account.putAccount updatedAccount
                                   . Account.recordRegId (cdi_regId cdi)

    {-# INLINE bsoNotifyExecutionCost #-}
    bsoNotifyExecutionCost bs amnt =
      return . snd $ bs & blockBank . Rewards.executionCost <%~ (+ amnt)

    bsoNotifyIdentityIssuerCredential bs idk =
      return . snd $ bs & blockBank . Rewards.identityIssuersRewards . at idk . non 0 <%~ (+ 1)

    {-# INLINE bsoGetExecutionCost #-}
    bsoGetExecutionCost bs =
      return $ bs ^. blockBank . Rewards.executionCost 

    {-# INLINE bsoGetBirkParameters #-}
    bsoGetBirkParameters = return . _blockBirkParameters

    bsoAddBaker bs binfo = return $ 
        let bid = bs ^. blockBirkParameters . to nextBakerId
            bs' = bs & blockBirkParameters %~ (\bp -> bp { birkBakers = Map.insert bid binfo (birkBakers bp) })
            bs'' = bs' & blockBirkParameters %~ (\bp -> bp { nextBakerId = bid + 1 })
        in (bid, bs'')

    -- NB: The caller must ensure the baker exists. Otherwise this method is incorrect and will raise a runtime error.
    bsoUpdateBaker bs bupdate = return $
        let bid = bupdate ^. BS.buId
            oldbinfo = bs ^. blockBirkParameters . to birkBakers . singular (ix bid)
        in bs & blockBirkParameters %~ (\bp -> bp { birkBakers = Map.insert bid (BS.updateBaker bupdate oldbinfo) (birkBakers bp) })

    bsoRemoveBaker bs bid = return $ 
        case bs ^? blockBirkParameters . to birkBakers . ix bid of
          Nothing -> (False, bs)
          Just _ -> (True, bs & blockBirkParameters %~ (\bp -> bp { birkBakers = Map.delete bid (birkBakers bp) }))

    bsoSetInflation bs amnt = return $
        bs & blockBank . Rewards.mintedGTUPerSlot .~ amnt

    -- mint currency in the central bank, and also update the total gtu amount to maintain the invariant
    -- that the total gtu amount is indeed the total gtu amount
    bsoMint bs amount = return $
        let updated = bs & ((blockBank . Rewards.totalGTU) +~ amount) .
                           ((blockBank . Rewards.centralBankGTU) +~ amount)
        in (updated ^. blockBank . Rewards.centralBankGTU, updated)

    bsoDecrementCentralBankGTU bs amount = return $
        let updated = bs & ((blockBank . Rewards.centralBankGTU) -~ amount)
        in (updated ^. blockBank . Rewards.centralBankGTU, updated)


type instance BS.BlockPointer (SkovTreeState s m) = BlockPointer

instance (SkovLenses s, Monad m, MonadState s m) => TS.TreeStateMonad (SkovTreeState s m) where
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
    getFinalizationAtIndex finIndex = fmap fst . Seq.lookup (fromIntegral finIndex) <$> use finalizationList
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
    thawBlockState bs = return $ bs & (blockBank . Rewards.executionCost .~ 0) .
                                      (blockBank . Rewards.identityIssuersRewards .~ HM.empty)


    {-# INLINE freezeBlockState #-}
    freezeBlockState = return

    {-# INLINE purgeBlockState #-}
    purgeBlockState _ = return ()

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats
