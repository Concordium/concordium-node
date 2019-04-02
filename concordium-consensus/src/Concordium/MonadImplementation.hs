{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell, LambdaCase, RecordWildCards, ViewPatterns, ScopedTypeVariables #-}
module Concordium.MonadImplementation where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State hiding (gets)
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class
import Control.Monad.RWS
import Control.Exception(assert)
import Lens.Micro.Platform
import Data.Foldable
import Data.Maybe
import Data.List(intercalate)
import Data.Time
import Data.Time.Clock.POSIX

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ

import Concordium.Payload.Transaction
import Concordium.Payload.Monad
import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Kontrol.Monad
import Concordium.Birk.LeaderElection
import Concordium.Afgjort.Finalize
import Concordium.Logger

data BlockStatus =
    BlockAlive !BlockPointer
    | BlockDead
    | BlockFinalized !BlockPointer !FinalizationRecord
    deriving (Eq)

data PendingBlock = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !Block,
    pbReceiveTime :: !UTCTime
}
instance Eq PendingBlock where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

-- |Weight factor to use in computing exponentially-weighted moving averages.
emaWeight :: Double
emaWeight = 0.1

data SkovStatistics = SkovStatistics {
    _blocksReceivedCount :: Int,
    _blocksVerifiedCount :: Int,
    _blockLastReceived :: Maybe UTCTime,
    _blockReceiveLatencyEMA :: Double,
    _blockReceiveLatencyEMVar :: Double,
    _blockReceivePeriodEMA :: Maybe Double,
    _blockReceivePeriodEMVar :: Maybe Double,
    _blockLastArrive :: Maybe UTCTime,
    _blockArriveLatencyEMA :: Double,
    _blockArriveLatencyEMVar :: Double,
    _blockArrivePeriodEMA :: Maybe Double,
    _blockArrivePeriodEMVar :: Maybe Double,
    _transactionsPerBlockEMA :: Double,
    _transactionsPerBlockEMVar :: Double,
    _finalizationCount :: Int,
    _lastFinalizedTime :: Maybe UTCTime,
    _finalizationPeriodEMA :: Maybe Double,
    _finalizationPeriodEMVar :: Maybe Double
}
makeLenses ''SkovStatistics

instance Show SkovStatistics where
    show SkovStatistics{..} = intercalate "," $ [show (fromMaybe 0 (realToFrac . utcTimeToPOSIXSeconds <$> (_blockLastArrive)) :: Double), show _blockArriveLatencyEMA, show _blockArriveLatencyEMVar, show _blockArrivePeriodEMA, show _blockArrivePeriodEMVar] ++
                                                [show (fromMaybe 0 (realToFrac . utcTimeToPOSIXSeconds <$> (_blockLastReceived)) :: Double), show _blockReceiveLatencyEMA, show _blockReceiveLatencyEMVar, show _blockReceivePeriodEMA, show _blockReceivePeriodEMVar]


initialSkovStatistics :: SkovStatistics
initialSkovStatistics = SkovStatistics {
    _blocksReceivedCount = 0,
    _blocksVerifiedCount = 0,
    _blockLastReceived = Nothing,
    _blockReceiveLatencyEMA = 0,
    _blockReceiveLatencyEMVar = 0,
    _blockReceivePeriodEMA = Nothing,
    _blockReceivePeriodEMVar = Nothing,
    _blockLastArrive = Nothing,
    _blockArriveLatencyEMA = 0,
    _blockArriveLatencyEMVar = 0,
    _blockArrivePeriodEMA = Nothing,
    _blockArrivePeriodEMVar = Nothing,
    _transactionsPerBlockEMA = 0,
    _transactionsPerBlockEMVar = 0,
    _finalizationCount = 0,
    _lastFinalizedTime = Nothing,
    _finalizationPeriodEMA = Nothing,
    _finalizationPeriodEMVar = Nothing
}

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
    _skovTransactionsFinalized :: Map.Map TransactionNonce Transaction,
    _skovTransactionsPending :: Map.Map TransactionNonce Transaction,
    _skovStatistics :: SkovStatistics
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
    transactionsFinalized :: Lens' s (Map.Map TransactionNonce Transaction)
    transactionsFinalized = skov . skovTransactionsFinalized
    transactionsPending :: Lens' s (Map.Map TransactionNonce Transaction)
    transactionsPending = skov . skovTransactionsPending
    statistics :: Lens' s SkovStatistics
    statistics = skov . skovStatistics

instance SkovLenses SkovData where
    skov = id

initialSkovData :: GenesisData -> SkovData
initialSkovData gd = SkovData {
            _skovBlockTable = HM.singleton gbh (BlockFinalized gb gbfin),
            _skovPossiblyPendingTable = HM.empty,
            _skovPossiblyPendingQueue = MPQ.empty,
            _skovBlocksAwaitingLastFinalized = MPQ.empty,
            _skovFinalizationList = Seq.singleton (gbfin, gb),
            _skovFinalizationPool = Map.empty,
            _skovBranches = Seq.empty,
            _skovGenesisData = gd,
            _skovGenesisBlockPointer = gb,
            _skovTransactionsFinalized = Map.empty,
            _skovTransactionsPending = Map.empty,
            _skovStatistics = initialSkovStatistics
        }
    where
        gb = makeGenesisBlockPointer gd
        gbh = bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

lastFinalizationRecord :: SkovData -> FinalizationRecord
lastFinalizationRecord sd = case _skovFinalizationList sd of
    _ Seq.:|> (lf,_) -> lf
    _ -> error "empty _skovFinalizationList"

lastFinalized :: SkovData -> BlockPointer
lastFinalized sd = case _skovFinalizationList sd of
    _ Seq.:|> (_,lf) -> lf
    _ -> error "empty _skovFinalizationList"

lastFinalizedHeight :: SkovData -> BlockHeight
lastFinalizedHeight = bpHeight . lastFinalized

lastFinalizedSlot :: SkovData -> Slot
lastFinalizedSlot = blockSlot . bpBlock . lastFinalized

data SkovListeners m = SkovListeners {
    onBlock :: BlockPointer -> m (),
    onFinalize :: FinalizationRecord -> BlockPointer -> m ()
}

-- |Handle a block arriving that is dead.  That is, the block has never
-- been in the tree before, and now it never can be.  Any descendents of
-- this block that have previously arrived cannot have been added to the
-- tree, and we purge them recursively from '_skovPossiblyPendingTable'.
blockArriveDead :: (MonadState s m, SkovLenses s, LoggerMonad m) => BlockHash -> m ()
blockArriveDead cbp = do
    blockTable . at cbp ?= BlockDead
    logEvent Skov LLDebug $ "Block " ++ show cbp ++ " arrived dead"
    children <- fmap pbHash <$> use (possiblyPendingTable . at cbp . non [])
    forM_ children blockArriveDead

-- |Purge pending blocks with slot numbers predating the last finalized slot.
purgePending :: (MonadState s m, SkovLenses s, LoggerMonad m) => m ()
purgePending = do
        lfSlot <- use (skov . to lastFinalizedSlot)
        let purge ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, (cbp, parenth)), ppq') ->
                    if sl <= lfSlot then do
                        possiblyPendingTable . at parenth . non [] %= filter ((/= cbp) . pbHash)
                        blockStatus <- use (blockTable . at cbp)
                        when (isNothing blockStatus) $
                            blockArriveDead cbp
                        purge ppq'
                    else
                        return ppq
                Nothing -> return ppq
        ppq <- use possiblyPendingQueue
        ppq' <- purge ppq
        possiblyPendingQueue .= ppq'

processAwaitingLastFinalized :: (MonadState s m, SkovLenses s, SkovMonad m) => SkovListeners m -> m ()
processAwaitingLastFinalized sl = do
    lastFinHeight <- use (skov . to lastFinalizedHeight)
    (MPQ.minViewWithKey <$> use blocksAwaitingLastFinalized) >>= \case
        Nothing -> return ()
        Just ((h, pb), balf') -> when (h <= lastFinHeight) $ do
            blocksAwaitingLastFinalized .= balf'
            -- This block is awaiting its last final block to be finalized.
            -- At this point, it should be or it never will.
            addBlock sl pb
            processAwaitingLastFinalized sl

processFinalizationPool :: (MonadState s m, SkovLenses s, SkovMonad m) => SkovListeners m -> m ()
processFinalizationPool sl@SkovListeners{..} = do
    nextFinIx <- FinalizationIndex . fromIntegral . Seq.length <$> use finalizationList
    finPending <- use (finalizationPool . at nextFinIx)
    case finPending of
        Nothing -> return ()
        Just frs -> do
            logEvent Skov LLDebug $ "Processing " ++ show (length frs) ++ " finalization records at index " ++ show nextFinIx
            blockStatus <- use blockTable
            lastFinHeight <- use (skov . to lastFinalizedHeight)
            finParams <- getFinalizationParameters
            genHash <- bpHash <$> use genesisBlockPointer
            let
                finSessId = FinalizationSessionId genHash 0 -- FIXME: Don't hard-code this!
                goodFin finRec@FinalizationRecord{..} =
                    finalizationIndex == nextFinIx -- Should always be true
                    && verifyFinalProof finSessId (makeFinalizationCommittee finParams) finRec
                checkFin finRec lp = case blockStatus ^. at (finalizationBlockPointer finRec) of
                    -- If the block is not present, the finalization record is pending
                    Nothing -> (finRec :) <$> lp
                    -- If the block is alive and the finalization proof checks out,
                    -- we can use this for finalization
                    Just (BlockAlive bp) -> if goodFin finRec then Left (finRec, bp) else lp
                    -- Otherwise, the finalization record is dead because the block is
                    -- either dead or already finalized
                    Just _ -> lp
            case foldr checkFin (Right []) frs of
                -- We got a valid finalization proof, so progress finalization
                Left (finRec, newFinBlock) -> do
                    logEvent Skov LLInfo $ "Block " ++ show (bpHash newFinBlock) ++ " is finalized at height " ++ show (theBlockHeight $ bpHeight newFinBlock)
                    updateFinalizationStatistics
                    finalizationPool . at nextFinIx .= Nothing
                    finalizationList %= (Seq.:|> (finRec, newFinBlock))
                    oldBranches <- use branches
                    let pruneHeight = fromIntegral (bpHeight newFinBlock - lastFinHeight)
                    let
                        pruneTrunk _ Seq.Empty = return ()
                        pruneTrunk keeper (brs Seq.:|> l) = do
                            forM_ l $ \bp -> if bp == keeper then do
                                                blockTable . at (bpHash bp) ?= BlockFinalized bp finRec
                                                logEvent Skov LLDebug $ "Block " ++ show (bpHash bp) ++ " marked finalized"
                                                -- Update the transaction tables
                                                forM_ (toTransactions (blockData (bpBlock bp))) $ \trs -> do
                                                    transactionsPending %= \ptrs -> foldr (Map.delete . transactionNonce) ptrs trs
                                                    transactionsFinalized %= \ftrs -> foldr (\t -> Map.insert (transactionNonce t) t) ftrs trs
                                            else do
                                                blockTable . at (bpHash bp) ?= BlockDead
                                                logEvent Skov LLDebug $ "Block " ++ show (bpHash bp) ++ " marked dead"
                            pruneTrunk (bpParent keeper) brs
                    pruneTrunk newFinBlock (Seq.take pruneHeight oldBranches)
                    -- Prune the branches
                    let
                        pruneBranches _ Seq.Empty = return Seq.empty
                        pruneBranches parents (brs Seq.:<| rest) = do
                            survivors <- foldrM (\bp l ->
                                if bpParent bp `elem` parents then
                                    return (bp:l)
                                else do
                                    blockTable . at (bpHash bp) ?= BlockDead
                                    logEvent Skov LLDebug $ "Block " ++ show (bpHash bp) ++ " marked dead"
                                    return l)
                                [] brs
                            rest' <- pruneBranches survivors rest
                            return (survivors Seq.<| rest')
                    newBranches <- pruneBranches [newFinBlock] (Seq.drop pruneHeight oldBranches)
                    branches .= newBranches
                    -- purge pending blocks with slot numbers predating the last finalized slot
                    purgePending
                    onFinalize finRec newFinBlock
                    -- handle blocks in skovBlocksAwaitingLastFinalized
                    processAwaitingLastFinalized sl
                    processFinalizationPool sl
                Right frs' -> finalizationPool . at nextFinIx . non [] .= frs'
            

addBlock :: (MonadState s m, SkovLenses s, SkovMonad m) => SkovListeners m -> PendingBlock -> m ()
addBlock sl@SkovListeners{..} pb@(PendingBlock cbp _ _) = do
    res <- runMaybeT (tryAddBlock pb)
    case res of
        Nothing -> blockArriveDead cbp
        Just Nothing -> return () -- The block was not inserted
        Just (Just blockP) -> do
            onBlock blockP
            processFinalizationPool sl
            -- Handle any blocks that are waiting for this one
            mchildren <- possiblyPendingTable . at cbp <<.= Nothing
            forM_ mchildren $ \children ->
                forM_ children $ \childpb -> do
                    childStatus <- use (blockTable . at (pbHash childpb))
                    when (isNothing childStatus) $ addBlock sl childpb

tryAddBlock :: forall s m. (MonadState s m, SkovLenses s, SkovMonad m) => PendingBlock -> MaybeT m (Maybe BlockPointer)
tryAddBlock pb@(PendingBlock cbp block recTime) = do
        lfs <- use (skov . to lastFinalizedSlot)
        -- The block must be later than the last finalized block
        guard $ lfs < blockSlot block
        parentStatus <- use (blockTable . at (blockPointer block))
        case parentStatus of
            Nothing -> do
                possiblyPendingTable . at parent . non [] %= (pb:)
                possiblyPendingQueue %= MPQ.insert (blockSlot block) (cbp, blockPointer block)
                logEvent Skov LLDebug $ "Block " ++ show cbp ++ " is pending its parent (" ++ show parent ++ ")"
                return Nothing
            Just BlockDead -> mzero
            Just (BlockAlive parentP) -> tryAddLiveParent parentP `mplus` invalidBlock
            Just (BlockFinalized parentP _) -> tryAddLiveParent parentP `mplus` invalidBlock
    where
        parent = blockPointer block
        invalidBlock = do
            logEvent Skov LLWarning $ "Block is not valid: " ++ show cbp
            mzero
        tryAddLiveParent :: BlockPointer -> MaybeT m (Maybe BlockPointer)
        tryAddLiveParent parentP = do -- Alive or finalized
            let lf = blockLastFinalized block
            -- Check that the blockSlot is beyond the parent slot
            guard $ blockSlot (bpBlock parentP) < blockSlot block
            lfStatus <- use (blockTable . at lf)
            case lfStatus of
                -- If the block's last finalized block is live, but not finalized yet,
                -- add this block to the queue at the appropriate point
                Just (BlockAlive lfBlockP) -> do
                    blocksAwaitingLastFinalized %= MPQ.insert (bpHeight lfBlockP) pb
                    logEvent Skov LLDebug $ "Block " ++ show cbp ++ " is pending finalization of block " ++ show (bpHash lfBlockP) ++ " at height " ++ show (theBlockHeight $ bpHeight lfBlockP)
                    return Nothing
                -- If the block's last finalized block is finalized, we can proceed with validation.
                -- Together with the fact that the parent is alive, we know that the new node
                -- is a descendent of the finalized block.
                Just (BlockFinalized lfBlockP finRec) -> do
                    -- The last finalized pointer must be to the block that was actually finalized.
                    -- (Blocks can be implicitly finalized when a descendent is finalized.)
                    guard $ finalizationBlockPointer finRec == lf
                    -- We need to know that the slot numbers of the last finalized blocks are ordered.
                    -- If the parent block is the genesis block then its last finalized pointer is not valid,
                    -- and we skip the check.
                    genB <- use genesisBlockPointer
                    unless (parentP == genB) $ guard $
                        blockSlot (bpBlock lfBlockP) >= blockSlot (bpBlock (bpLastFinalized parentP))
                    bps@BirkParameters{..} <- getBirkParameters (blockSlot block)
                    BakerInfo{..} <- MaybeT $ pure $ birkBaker (blockBaker block) bps
                    -- Check the block proof
                    guard $ verifyProof
                                birkLeadershipElectionNonce
                                birkElectionDifficulty
                                (blockSlot block)
                                bakerElectionVerifyKey
                                bakerLotteryPower
                                (blockProof block)
                    -- The block nonce
                    guard $ verifyBlockNonce
                                birkLeadershipElectionNonce
                                (blockSlot block)
                                bakerElectionVerifyKey
                                (blockNonce block)
                    -- And the block signature
                    guard $ verifyBlockSignature bakerSignatureVerifyKey block
                    let height = bpHeight parentP + 1
                    ts <- MaybeT $ pure $ toTransactions (blockData block)
                    case executeBlockForState ts (makeChainMeta (blockSlot block) parentP lfBlockP) (bpState parentP) of
                        Left _ -> mzero -- FIXME: Report the errors somewhere, e.g., log to file.
                        Right gs -> do
                            curTime <- liftIO getCurrentTime
                            let blockP = BlockPointer {
                                bpHash = cbp,
                                bpBlock = block,
                                bpParent = parentP,
                                bpLastFinalized = lfBlockP,
                                bpHeight = height,
                                bpState = gs,
                                bpReceiveTime = recTime,
                                bpArriveTime = curTime,
                                bpTransactionCount = length ts
                            }
                            blockTable . at cbp ?= BlockAlive blockP
                            logEvent Skov LLInfo $ "Block " ++ show cbp ++ " arrived"
                            -- Update the statistics
                            updateArriveStatistics blockP
                            -- Add to the branches
                            finHght <- use (skov . to lastFinalizedHeight)
                            brs <- use branches
                            let branchLen = fromIntegral $ Seq.length brs
                            let insertIndex = height - finHght - 1
                            if insertIndex < branchLen then
                                branches . ix (fromIntegral insertIndex) %= (blockP:)
                            else
                                assert (insertIndex == branchLen)
                                    branches %= (Seq.|> [blockP])
                            return (Just blockP)
                -- If the block's last finalized block is dead, then the block arrives dead.
                -- If the block's last finalized block is pending then it can't be an ancestor,
                -- so the block is invalid and it arrives dead.
                _ -> mzero

-- | Called when a block is fully validated (arrives) to update the statistics.
updateArriveStatistics :: forall s m. (MonadState s m, SkovLenses s, SkovMonad m) => BlockPointer -> m ()
updateArriveStatistics BlockPointer{..} = do
        statistics . blocksVerifiedCount += 1
        updateLatency
        updatePeriod
        updateTransactionsPerBlock
        s <- use statistics
        logEvent Skov LLInfo $ "Arrive statistics:" ++
            " blocksVerifiedCount=" ++ show (s ^. blocksVerifiedCount) ++
            " blockLastArrive=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastArrive) ++
            " blockArriveLatencyEMA=" ++ show (s ^. blockArriveLatencyEMA) ++
            " blockArriveLatencyEMSD=" ++ show (sqrt $ s ^. blockArriveLatencyEMVar) ++
            " blockArrivePeriodEMA=" ++ show (s ^. blockArrivePeriodEMA) ++
            " blockArrivePeriodEMSD=" ++ show (sqrt <$> s ^. blockArrivePeriodEMVar) ++
            " transactionsPerBlockEMA=" ++ show (s ^. transactionsPerBlockEMA) ++
            " transactionsPerBlockEMSD=" ++ show (sqrt $ s ^. transactionsPerBlockEMVar)
    where
        curTime = bpArriveTime
        updateLatency = do
            slotTime <- getSlotTime (blockSlot bpBlock)
            oldEMA <- use $ statistics . blockArriveLatencyEMA
            let delta = realToFrac (diffUTCTime curTime slotTime) - oldEMA
            statistics . blockArriveLatencyEMA .= oldEMA + emaWeight * delta
            statistics . blockArriveLatencyEMVar %= \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta)
        updatePeriod = do
            oldLastArrive <- statistics . blockLastArrive <<.= Just curTime
            forM_ oldLastArrive $ \lastBTime -> do
                let blockTime = realToFrac (diffUTCTime curTime lastBTime)
                oldEMA <- fromMaybe blockTime <$> (use $ statistics . blockArrivePeriodEMA)
                let delta = blockTime - oldEMA
                statistics . blockArrivePeriodEMA ?= oldEMA + emaWeight * delta
                oldEMVar <- fromMaybe 0 <$> (use $ statistics . blockArrivePeriodEMVar)
                statistics . blockArrivePeriodEMVar ?= (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta)
        updateTransactionsPerBlock = do
            oldEMA <- use $ statistics . transactionsPerBlockEMA
            let delta = fromIntegral bpTransactionCount - oldEMA
            statistics . transactionsPerBlockEMA .= oldEMA + emaWeight * delta
            statistics . transactionsPerBlockEMVar %= \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta)

-- | Called when a block is received to update the statistics.
updateReceiveStatistics :: forall s m. (MonadState s m, SkovLenses s, SkovMonad m) => PendingBlock -> m ()
updateReceiveStatistics PendingBlock{..} = do
        statistics . blocksReceivedCount += 1
        updateLatency
        updatePeriod
        s <- use statistics
        logEvent Skov LLInfo $ "Receive statistics:" ++
            " blocksReceivedCount=" ++ show (s ^. blocksReceivedCount) ++
            " blockLastReceived=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastReceived) ++
            " blockReceiveLatencyEMA=" ++ show (s ^. blockReceiveLatencyEMA) ++
            " blockReceiveLatencyEMSD=" ++ show (sqrt $ s ^. blockReceiveLatencyEMVar) ++
            " blockReceivePeriodEMA=" ++ show (s ^. blockReceivePeriodEMA) ++
            " blockReceivePeriodEMSD=" ++ show (sqrt <$> s ^. blockReceivePeriodEMVar)
    where
        updateLatency = do
            slotTime <- getSlotTime (blockSlot pbBlock)
            oldEMA <- use $ statistics . blockReceiveLatencyEMA
            let delta = realToFrac (diffUTCTime pbReceiveTime slotTime) - oldEMA
            statistics . blockReceiveLatencyEMA .= oldEMA + emaWeight * delta
            statistics . blockReceiveLatencyEMVar %= \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta)
        updatePeriod = do
            oldLastReceived <- statistics . blockLastReceived <<.= Just pbReceiveTime
            forM_ oldLastReceived $ \lastBTime -> do
                let blockTime = realToFrac (diffUTCTime pbReceiveTime lastBTime)
                oldEMA <- fromMaybe blockTime <$> (use $ statistics . blockReceivePeriodEMA)
                let delta = blockTime - oldEMA
                statistics . blockReceivePeriodEMA ?= oldEMA + emaWeight * delta
                oldEMVar <- fromMaybe 0 <$> (use $ statistics . blockReceivePeriodEMVar)
                statistics . blockReceivePeriodEMVar ?= (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta)

-- | Called when a block has been finalized to update the statistics.        
updateFinalizationStatistics :: forall s m. (MonadState s m, SkovLenses s, SkovMonad m) => m ()
updateFinalizationStatistics = do
    statistics . finalizationCount += 1
    curTime <- liftIO getCurrentTime
    oldLastFinalized <- statistics . lastFinalizedTime <<.= Just curTime
    forM_ oldLastFinalized $ \lastFinTime -> do
        let finTime = realToFrac (diffUTCTime curTime lastFinTime)
        oldEMA <- fromMaybe finTime <$> (use $ statistics . finalizationPeriodEMA)
        let delta = finTime - oldEMA
        statistics . finalizationPeriodEMA ?= oldEMA + emaWeight * delta
        oldEMVar <- fromMaybe 0 <$> (use $ statistics . finalizationPeriodEMVar)
        statistics . finalizationPeriodEMVar ?= (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta)
    s <- use statistics
    logEvent Skov LLInfo $ "Finalization statistics:" ++
        " finalizationCount=" ++ show (s ^. finalizationCount) ++
        " lastFinalizedTime=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. lastFinalizedTime) ++
        " finalizationPeriodEMA=" ++ show (s ^. finalizationPeriodEMA) ++
        " finalizationPeriodEMSD=" ++ show (sqrt <$> s ^. finalizationPeriodEMVar)

-- Tree consists of finalization list + branches
-- When adding a block that is at height in the finalization list, check if it's already there; if not, it should be dead.
-- Height 0 is the genesis block

doResolveBlock :: (MonadState s m, SkovLenses s) => BlockHash -> m (Maybe BlockPointer)
{-# INLINE doResolveBlock #-}
doResolveBlock cbp = use (blockTable . at cbp) <&> \case
        Just (BlockAlive bp) -> Just bp
        Just (BlockFinalized bp _) -> Just bp
        _ -> Nothing

doStoreBlock :: (MonadState s m, SkovLenses s, SkovMonad m) => SkovListeners m -> Block -> m BlockHash
{-# INLINE doStoreBlock #-}
doStoreBlock sl block0 = do
    let cbp = hashBlock block0
    oldBlock <- use (blockTable . at cbp)
    when (isNothing oldBlock) $ do
        -- The block is new, so we have some work to do.
        logEvent Skov LLDebug $ "Received block " ++ show cbp
        curTime <- liftIO getCurrentTime
        let pb = (PendingBlock cbp block0 curTime)
        updateReceiveStatistics pb
        addBlock sl pb
    return cbp

doFinalizeBlock :: (MonadState s m, SkovLenses s, SkovMonad m) => SkovListeners m -> FinalizationRecord -> m ()
{-# INLINE doFinalizeBlock #-}
doFinalizeBlock sl finRec = do
    let thisFinIx = finalizationIndex finRec
    nextFinIx <- FinalizationIndex . fromIntegral . Seq.length <$> use finalizationList
    case compare thisFinIx nextFinIx of
        LT -> return () -- Already finalized at that index
        EQ -> do 
                finalizationPool . at thisFinIx . non [] %= (finRec:)
                processFinalizationPool sl
        GT -> finalizationPool . at thisFinIx . non [] %= (finRec:)

doIsFinalized :: (MonadState s m, SkovLenses s) => BlockHash -> m Bool
doIsFinalized bp = preuse (blockTable . ix bp) >>= \case
        Just (BlockFinalized _ _) -> return True
        _ -> return False

doGetCurrentHeight :: (MonadState s m, SkovLenses s) => m BlockHeight
doGetCurrentHeight = use $ skov . (to $ \s -> lastFinalizedHeight s + fromIntegral (Seq.length (_skovBranches s)))

doBranchesFromTop :: (MonadState s m, SkovLenses s) => m [[BlockPointer]]
doBranchesFromTop = revSeqToList <$> use branches
    where
        revSeqToList Seq.Empty = []
        revSeqToList (r Seq.:|> t) = t : revSeqToList r

doGetBlocksAtHeight :: (MonadState s m, SkovLenses s) => BlockHeight -> m [BlockPointer]
doGetBlocksAtHeight h = do
        lastFin <- use (skov . to lastFinalized)
        case compare h (bpHeight lastFin) of
            EQ -> return [lastFin]
            GT -> do
                brs <- use $ branches
                case brs Seq.!? (fromIntegral $ h - bpHeight lastFin - 1) of
                    Nothing -> return []
                    Just bs -> return bs
            LT -> return [findFrom (bpParent lastFin)] -- TODO: replace with more efficient search
    where
        findFrom bp
            | bpHeight bp == h = bp
            | otherwise = findFrom (bpParent bp)

noopSkovListeners :: Monad m => SkovListeners m
noopSkovListeners = SkovListeners {
    onBlock = \_ -> return (),
    onFinalize = \_ _ -> return ()
}

instance (MonadIO m, LoggerMonad m) => SkovMonad (StateT SkovData m) where
    {-# INLINE resolveBlock #-}
    resolveBlock = doResolveBlock
    storeBlock = doStoreBlock noopSkovListeners
    finalizeBlock = doFinalizeBlock noopSkovListeners
    isFinalized = doIsFinalized
    lastFinalizedBlock = use (to lastFinalized)
    getGenesisData = use skovGenesisData
    genesisBlock = use skovGenesisBlockPointer
    getCurrentHeight = doGetCurrentHeight
    branchesFromTop = doBranchesFromTop
    getBlocksAtHeight = doGetBlocksAtHeight

doAddPendingTransaction :: (MonadState s m, SkovLenses s) => Transaction -> m ()
doAddPendingTransaction tr@Transaction{..} = do
    isFin <- Map.member transactionNonce <$> use transactionsFinalized
    unless isFin $
        transactionsPending %= Map.insert transactionNonce tr
doGetPendingTransactionsAtBlock :: (MonadState s m, SkovLenses s) => BlockPointer -> m (Maybe (Map.Map TransactionNonce Transaction))
doGetPendingTransactionsAtBlock bp = do
    pts <- use transactionsPending
    doGetTransactionsAtBlock bp >>= \case
        Nothing -> return Nothing
        Just bts -> return $ Just $ Map.difference pts bts
doGetTransactionsAtBlock :: (MonadState s m, SkovLenses s) => BlockPointer -> m (Maybe (Map.Map TransactionNonce Transaction))
doGetTransactionsAtBlock bp = do
    lfp <- use (skov . to lastFinalized)
    genp <- use genesisBlockPointer
    let getTrans cbp
            | cbp == lfp = use transactionsFinalized
            | cbp == genp = return Map.empty
            | otherwise = do
                    bts <- MaybeT . pure $ toTransactions (blockData (bpBlock cbp))
                    parentTrans <- getTrans (bpParent cbp)
                    let upd tr@Transaction{..} s = if transactionNonce `Map.member` s then Nothing else Just (Map.insert transactionNonce tr s)
                    MaybeT $ pure $ foldrM upd parentTrans bts
    runMaybeT $ getTrans bp


instance (MonadIO m, LoggerMonad m) => PayloadMonad (StateT SkovData m) where
    addPendingTransaction = doAddPendingTransaction
    getPendingTransactionsAtBlock  = doGetPendingTransactionsAtBlock
    getTransactionsAtBlock = doGetTransactionsAtBlock

instance (MonadIO m, LoggerMonad m) => KontrolMonad (StateT SkovData m)

data SkovFinalizationState = SkovFinalizationState {
    _sfsSkov :: SkovData,
    _sfsFinalization :: FinalizationState
}
makeLenses ''SkovFinalizationState

instance SkovLenses SkovFinalizationState where
    skov = sfsSkov

instance FinalizationStateLenses SkovFinalizationState where
    finState = sfsFinalization

initialSkovFinalizationState :: FinalizationInstance -> GenesisData -> SkovFinalizationState
initialSkovFinalizationState finInst gen = SkovFinalizationState{..}
    where
        _sfsSkov = initialSkovData gen
        _sfsFinalization = initialFinalizationState finInst (bpHash (_skovGenesisBlockPointer _sfsSkov)) (makeFinalizationCommittee (genesisFinalizationParameters gen))

sfsSkovListeners :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => SkovListeners m
sfsSkovListeners = SkovListeners {
    onBlock = notifyBlockArrival,
    onFinalize = notifyBlockFinalized
}

instance (MonadIO m, LoggerMonad m) => SkovMonad (RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState m) where
    {-# INLINE resolveBlock #-}
    resolveBlock = doResolveBlock
    storeBlock = doStoreBlock sfsSkovListeners
    finalizeBlock = doFinalizeBlock sfsSkovListeners
    isFinalized bp = preuse (blockTable . ix bp) >>= \case
            Just (BlockFinalized _ _) -> return True
            _ -> return False
    lastFinalizedBlock = use (skov . to lastFinalized)
    getGenesisData = use genesisData
    genesisBlock = use genesisBlockPointer
    getCurrentHeight = doGetCurrentHeight
    branchesFromTop = doBranchesFromTop
    getBlocksAtHeight = doGetBlocksAtHeight

instance (MonadIO m, LoggerMonad m) => FinalizationMonad (RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState m) where
    broadcastFinalizationMessage = tell . Endo . (:) . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . Endo . (:) . BroadcastFinalizationRecord

instance (MonadIO m, LoggerMonad m) => PayloadMonad (RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState m) where
    addPendingTransaction = doAddPendingTransaction
    getPendingTransactionsAtBlock  = doGetPendingTransactionsAtBlock
    getTransactionsAtBlock = doGetTransactionsAtBlock

instance (MonadIO m, LoggerMonad m) => KontrolMonad (RWST FinalizationInstance (Endo [FinalizationOutputEvent]) SkovFinalizationState m)
