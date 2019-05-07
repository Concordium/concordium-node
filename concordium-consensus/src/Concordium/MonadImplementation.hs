{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell, LambdaCase, RecordWildCards, ViewPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, StandaloneDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Concordium.MonadImplementation where

import Control.Monad
import Control.Monad.Trans.State hiding (gets)
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class
import Control.Monad.RWS
import Lens.Micro.Platform
import Data.Foldable
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX

import GHC.Stack

import qualified Data.Sequence as Seq

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.TreeState
import qualified Concordium.GlobalState.TreeState.Basic as Basic
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Parameters

import Concordium.Scheduler.TreeStateEnvironment(executeFrom)


import Concordium.Skov.Monad
import Concordium.Kontrol.Monad
import Concordium.Birk.LeaderElection
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.TimeMonad

isAncestorOf :: BlockPointerData bp => bp -> bp -> Bool
isAncestorOf b1 b2 = case compare (bpHeight b1) (bpHeight b2) of
        GT -> False
        EQ -> b1 == b2
        LT -> isAncestorOf b1 (bpParent b2)

updateFocusBlockTo :: (TreeStateMonad m) => BlockPointer m -> m ()
updateFocusBlockTo newBB = do
        oldBB <- getFocusBlock
        pts <- getPendingTransactions
        putPendingTransactions (updatePTs oldBB newBB [] pts)
        putFocusBlock newBB
    where
        updatePTs oBB nBB forw pts = case compare (bpHeight oBB) (bpHeight nBB) of
                LT -> updatePTs oBB (bpParent nBB) (nBB : forw) pts
                EQ -> if oBB == nBB then
                            foldl (flip (forwardPTT . blockTransactions)) pts forw
                        else
                            updatePTs (bpParent oBB) (bpParent nBB) (nBB : forw) (reversePTT (blockTransactions oBB) pts)
                GT -> updatePTs (bpParent oBB) nBB forw (reversePTT (blockTransactions oBB) pts)

-- |Weight factor to use in computing exponentially-weighted moving averages.
emaWeight :: Double
emaWeight = 0.1


-- | Called when a block is fully validated (arrives) to update the statistics.
updateArriveStatistics :: forall m. (LoggerMonad m, TreeStateMonad m, SkovMonad m) => BlockPointer m -> m ()
updateArriveStatistics bp = do
        s0 <- getConsensusStatistics
        let s1 = s0 & blocksVerifiedCount +~ 1
        s2 <- updateLatency s1
        let s3 = updatePeriod s2
        let s = updateTransactionsPerBlock s3
        putConsensusStatistics s
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
        curTime = bpArriveTime bp
        updateLatency s = do
            slotTime <- getSlotTime (blockSlot bp)
            let 
                oldEMA = s ^. blockArriveLatencyEMA
                delta = realToFrac (diffUTCTime curTime slotTime) - oldEMA
            return $
                s & (blockArriveLatencyEMA .~ oldEMA + emaWeight * delta)
                  & (blockArriveLatencyEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        updatePeriod s = 
            case s ^. blockLastArrive of
                Nothing -> s & blockLastArrive ?~ curTime
                Just lastBTime -> 
                    let 
                        blockTime = realToFrac (diffUTCTime curTime lastBTime)
                        oldEMA = fromMaybe blockTime (s ^. blockArrivePeriodEMA)
                        delta = blockTime - oldEMA
                        oldEMVar = fromMaybe 0 (s ^. blockArrivePeriodEMVar)
                    in
                        s & (blockLastArrive ?~ curTime)
                            & (blockArrivePeriodEMA ?~ oldEMA + emaWeight * delta)
                            & (blockArrivePeriodEMVar ?~ (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        updateTransactionsPerBlock s =
            let
                oldEMA = s ^. transactionsPerBlockEMA
                delta = fromIntegral (bpTransactionCount bp) - oldEMA
            in
                s & (transactionsPerBlockEMA .~ oldEMA + emaWeight * delta)
                  & (transactionsPerBlockEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))

-- | Called when a block is received to update the statistics.
updateReceiveStatistics :: forall m. (TreeStateMonad m, SkovMonad m) => PendingBlock -> m ()
updateReceiveStatistics pb = do
        s0 <- getConsensusStatistics
        let s1 = s0 & blocksReceivedCount +~ 1
        s2 <- updateLatency s1
        let s = updatePeriod s2
        putConsensusStatistics s
        logEvent Skov LLInfo $ "Receive statistics:" ++
            " blocksReceivedCount=" ++ show (s ^. blocksReceivedCount) ++
            " blockLastReceived=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastReceived) ++
            " blockReceiveLatencyEMA=" ++ show (s ^. blockReceiveLatencyEMA) ++
            " blockReceiveLatencyEMSD=" ++ show (sqrt $ s ^. blockReceiveLatencyEMVar) ++
            " blockReceivePeriodEMA=" ++ show (s ^. blockReceivePeriodEMA) ++
            " blockReceivePeriodEMSD=" ++ show (sqrt <$> s ^. blockReceivePeriodEMVar)
    where
        updateLatency s = do
            slotTime <- getSlotTime (blockSlot pb)
            let
                oldEMA = s ^. blockReceiveLatencyEMA
                delta = realToFrac (diffUTCTime (pbReceiveTime pb) slotTime) - oldEMA
            return $
                s & (blockReceiveLatencyEMA .~ oldEMA + emaWeight * delta)
                  & (blockReceiveLatencyEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        updatePeriod s = 
            case s ^. blockLastReceived of
                Nothing -> s & blockLastReceived ?~ pbReceiveTime pb
                Just lastBTime ->
                    let
                        blockTime = realToFrac (diffUTCTime (pbReceiveTime pb) lastBTime)
                        oldEMA = fromMaybe blockTime (s ^. blockReceivePeriodEMA)
                        delta = blockTime - oldEMA
                        oldEMVar = fromMaybe 0 (s ^. blockReceivePeriodEMVar)
                    in
                        s & (blockLastReceived ?~ pbReceiveTime pb)
                          & (blockReceivePeriodEMA ?~ oldEMA + emaWeight * delta)
                          & (blockReceivePeriodEMVar ?~ (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))

-- | Called when a block has been finalized to update the statistics.        
updateFinalizationStatistics :: forall m. (TreeStateMonad m, SkovMonad m) => m ()
updateFinalizationStatistics = do
        s0 <- getConsensusStatistics
        let s1 = s0 & finalizationCount +~ 1
        curTime <- currentTime
        let s = case (s1 ^. lastFinalizedTime) of
                Nothing -> s1 & lastFinalizedTime ?~ curTime
                Just lastFinTime ->
                    let
                        finTime = realToFrac (diffUTCTime curTime lastFinTime)
                        oldEMA = fromMaybe finTime (s1 ^. finalizationPeriodEMA)
                        delta = finTime - oldEMA
                        oldEMVar = fromMaybe 0 (s1 ^. finalizationPeriodEMVar)
                    in
                        s1 & (lastFinalizedTime ?~ curTime)
                           & (finalizationPeriodEMA ?~ oldEMA + emaWeight * delta)
                           & (finalizationPeriodEMVar ?~ (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        putConsensusStatistics s
        logEvent Skov LLInfo $ "Finalization statistics:" ++
            " finalizationCount=" ++ show (s ^. finalizationCount) ++
            " lastFinalizedTime=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. lastFinalizedTime) ++
            " finalizationPeriodEMA=" ++ show (s ^. finalizationPeriodEMA) ++
            " finalizationPeriodEMSD=" ++ show (sqrt <$> s ^. finalizationPeriodEMVar)

data SkovListeners m = SkovListeners {
    onBlock :: BlockPointer m -> m (),
    onFinalize :: FinalizationRecord -> BlockPointer m -> m ()
}

-- |Handle a block arriving that is dead.  That is, the block has never
-- been in the tree before, and now it never can be.  Any descendents of
-- this block that have previously arrived cannot have been added to the
-- tree, and we purge them recursively from '_skovPossiblyPendingTable'.
blockArriveDead :: (HasCallStack, TreeStateMonad m, LoggerMonad m) => BlockHash -> m ()
blockArriveDead cbp = do
        markDead cbp
        logEvent Skov LLDebug $ "Block " ++ show cbp ++ " arrived dead"
        children <- map getHash <$> takePendingChildren cbp
        forM_ children blockArriveDead

-- |Purge pending blocks with slot numbers predating the last finalized slot.
purgePending :: (HasCallStack, TreeStateMonad m, LoggerMonad m) => m ()
purgePending = do
        lfSlot <- getLastFinalizedSlot
        let purgeLoop = takeNextPendingUntil lfSlot >>= \case
                            Nothing -> return ()
                            Just (getHash -> pb) -> do
                                pbStatus <- getBlockStatus pb
                                when (isNothing pbStatus) $ blockArriveDead pb
                                purgeLoop
        purgeLoop

processAwaitingLastFinalized :: (HasCallStack, TreeStateMonad m, SkovMonad m, MonadWriter (Endo [SkovFinalizationEvent]) m) => SkovListeners m -> m ()
processAwaitingLastFinalized sl = do
        lastFinHeight <- getLastFinalizedHeight
        takeAwaitingLastFinalizedUntil lastFinHeight >>= \case
            Nothing -> return ()
            Just pb -> do
                -- This block is awaiting its last final block to be finalized.
                -- At this point, it should be or it never will.
                addBlock sl pb
                processAwaitingLastFinalized sl

processFinalizationPool :: forall m. (HasCallStack, TreeStateMonad m, SkovMonad m, MonadWriter (Endo [SkovFinalizationEvent]) m) => SkovListeners m -> m ()
processFinalizationPool sl@SkovListeners{..} = do
        nextFinIx <- getNextFinalizationIndex
        frs <- getFinalizationPoolAtIndex nextFinIx
        unless (null frs) $ do
            logEvent Skov LLTrace $ "Processing " ++ show (length frs) ++ " finalization records at index " ++ show nextFinIx
            lastFinHeight <- getLastFinalizedHeight
            finParams <- getFinalizationParameters
            genHash <- getHash <$> getGenesisBlockPointer
            let
                finSessId = FinalizationSessionId genHash 0 -- FIXME: Don't hard-code this!
                goodFin finRec@FinalizationRecord{..} =
                    finalizationIndex == nextFinIx -- Should always be true
                    && verifyFinalProof finSessId (makeFinalizationCommittee finParams) finRec
                checkFin finRec lp = getBlockStatus (finalizationBlockPointer finRec) >>= return . \case
                    -- If the block is not present, the finalization record is pending
                    Nothing -> (finRec :) <$> lp
                    -- If the block is alive and the finalization proof checks out,
                    -- we can use this for finalization
                    Just (BlockAlive bp) -> if goodFin finRec then Left (finRec, bp) else lp
                    -- Otherwise, the finalization record is dead because the block is
                    -- either dead or already finalized
                    Just _ -> lp
            foldrM checkFin (Right []) frs >>= \case
                -- We got a valid finalization proof, so progress finalization
                Left (finRec, newFinBlock) -> do
                    logEvent Skov LLInfo $ "Block " ++ show (bpHash newFinBlock) ++ " is finalized at height " ++ show (theBlockHeight $ bpHeight newFinBlock)
                    updateFinalizationStatistics
                    -- Check if the focus block is a descendent of the block we are finalizing
                    focusBlockSurvives <- (isAncestorOf newFinBlock) <$> getFocusBlock
                    -- If not, update the focus to the new finalized block.
                    -- This is to ensure that the focus block is always a live (or finalized) block.
                    unless focusBlockSurvives $ updateFocusBlockTo newFinBlock
                    putFinalizationPoolAtIndex nextFinIx []
                    addFinalization newFinBlock finRec
                    oldBranches <- getBranches
                    let pruneHeight = fromIntegral (bpHeight newFinBlock - lastFinHeight)
                    let
                        pruneTrunk :: BlockPointer m -> Branches m -> m ()
                        pruneTrunk _ Seq.Empty = return ()
                        pruneTrunk keeper (brs Seq.:|> l) = do
                            forM_ l $ \bp -> if bp == keeper then do
                                                markFinalized (getHash bp) finRec
                                                logEvent Skov LLDebug $ "Block " ++ show bp ++ " marked finalized"
                                            else do
                                                markDead (getHash bp)
                                                logEvent Skov LLDebug $ "Block " ++ show bp ++ " marked dead"
                            pruneTrunk (bpParent keeper) brs
                            finalizeTransactions (blockTransactions keeper)
                    pruneTrunk newFinBlock (Seq.take pruneHeight oldBranches)
                    -- Prune the branches
                    let
                        pruneBranches _ Seq.Empty = return Seq.empty
                        pruneBranches parents (brs Seq.:<| rest) = do
                            survivors <- foldrM (\bp l ->
                                if bpParent bp `elem` parents then
                                    return (bp:l)
                                else do
                                    markDead (bpHash bp)
                                    logEvent Skov LLDebug $ "Block " ++ show (bpHash bp) ++ " marked dead"
                                    return l)
                                [] brs
                            rest' <- pruneBranches survivors rest
                            return (survivors Seq.<| rest')
                    newBranches <- pruneBranches [newFinBlock] (Seq.drop pruneHeight oldBranches)
                    putBranches newBranches
                    -- purge pending blocks with slot numbers predating the last finalized slot
                    purgePending
                    onFinalize finRec newFinBlock
                    -- handle blocks in skovBlocksAwaitingLastFinalized
                    processAwaitingLastFinalized sl
                    processFinalizationPool sl
                Right frs' -> putFinalizationPoolAtIndex nextFinIx frs'

addBlock :: (HasCallStack, TreeStateMonad m, SkovMonad m, MonadWriter (Endo [SkovFinalizationEvent]) m) => SkovListeners m -> PendingBlock -> m ()
addBlock sl@SkovListeners{..} pb = do
    let cbp = getHash pb
    res <- runMaybeT (tryAddBlock sl pb)
    case res of
        Nothing -> blockArriveDead cbp
        Just Nothing -> return () -- The block was not inserted
        Just (Just blockP) -> do
            onBlock blockP
            processFinalizationPool sl
            -- Handle any blocks that are waiting for this one
            children <- takePendingChildren cbp
            forM_ children $ \childpb -> do
                childStatus <- getBlockStatus (pbHash childpb)
                when (isNothing childStatus) $ addBlock sl childpb

-- |Try to add a block to the tree.  There are three possible outcomes:
--
-- 1. The operation fails, in which case the block was determined to be invalid in the current tree, and subsequently
--    should be marked dead.
-- 2. The operation succeeds returning @Nothing@, in which case more information is required to determine validity
--    of the block, and it has been added to the appropriate wait pool.
-- 3. The operation succeeds returning @Just bp@, in which the block is added, marked alive and represented by
--    block pointer bp.
tryAddBlock :: forall m. (HasCallStack, TreeStateMonad m, SkovMonad m, MonadWriter (Endo [SkovFinalizationEvent]) m) => SkovListeners m -> PendingBlock -> MaybeT m (Maybe (BlockPointer m))
tryAddBlock SkovListeners{..} block = do
        lfs <- getLastFinalizedSlot
        -- The block must be later than the last finalized block
        guard $ lfs < blockSlot block
        parentStatus <- getBlockStatus parent
        case parentStatus of
            Nothing -> do
                addPendingBlock block
                notifyMissingBlock parent
                logEvent Skov LLDebug $ "Block " ++ show block ++ " is pending its parent (" ++ show parent ++ ")"
                -- Check also if the block's last finalized block has been finalized
                getBlockStatus (blockLastFinalized bf) >>= \case
                    Just (BlockFinalized _ _) -> return ()
                    _ -> notifyMissingFinalization (Left $ blockLastFinalized bf)
                return Nothing
            Just BlockDead -> mzero
            Just (BlockAlive parentP) -> tryAddLiveParent parentP `mplus` invalidBlock
            Just (BlockFinalized parentP _) -> do
                lfb <- getLastFinalized
                -- If the parent is finalized, it had better be the last finalized, or else the block is already dead
                guard (parentP == lfb)
                tryAddLiveParent parentP `mplus` invalidBlock
    where
        bf = bbFields (pbBlock block)
        parent = blockPointer bf
        invalidBlock = do
            logEvent Skov LLWarning $ "Block is not valid: " ++ show block
            mzero
        tryAddLiveParent :: BlockPointer m -> MaybeT m (Maybe (BlockPointer m))
        tryAddLiveParent parentP = do -- Alive or finalized
            let lf = blockLastFinalized bf
            -- Check that the blockSlot is beyond the parent slot
            guard $ blockSlot (bpBlock parentP) < blockSlot block
            lfStatus <- getBlockStatus lf
            case lfStatus of
                -- If the block's last finalized block is live, but not finalized yet,
                -- add this block to the queue at the appropriate point
                Just (BlockAlive lfBlockP) -> do
                    addAwaitingLastFinalized (bpHeight lfBlockP) block
                    notifyMissingFinalization (Left lf)
                    logEvent Skov LLDebug $ "Block " ++ show block ++ " is pending finalization of block " ++ show (bpHash lfBlockP) ++ " at height " ++ show (theBlockHeight $ bpHeight lfBlockP)
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
                    genB <- getGenesisBlockPointer
                    unless (parentP == genB) $ guard $ -- TODO: Should be possible to remove this test, since bpLastFinalized of the genesis block should be the genesis block itself
                        blockSlot lfBlockP >= blockSlot (bpLastFinalized parentP)
                    bps@BirkParameters{..} <- getBirkParameters (blockSlot block)
                    BakerInfo{..} <- MaybeT $ pure $ birkBaker (blockBaker bf) bps
                    -- Check the block proof
                    guard $ verifyProof
                                birkLeadershipElectionNonce
                                birkElectionDifficulty
                                (blockSlot block)
                                bakerElectionVerifyKey
                                bakerLotteryPower
                                (blockProof bf)
                    -- The block nonce
                    guard $ verifyBlockNonce
                                birkLeadershipElectionNonce
                                (blockSlot block)
                                bakerElectionVerifyKey
                                (blockNonce bf)
                    -- And the block signature
                    guard $ verifyBlockSignature bakerSignatureVerifyKey block
                    let ts = blockTransactions block
                    executeFrom (blockSlot block) parentP lfBlockP ts >>= \case
                        Left err -> do
                            logEvent Skov LLWarning ("Block execution failure: " ++ show err)
                            mzero
                        Right gs -> Just <$> blockArrive block parentP lfBlockP gs
                -- If the block's last finalized block is dead, then the block arrives dead.
                -- If the block's last finalized block is pending then it can't be an ancestor,
                -- so the block is invalid and it arrives dead.
                _ -> mzero

blockArrive :: (HasCallStack, TreeStateMonad m, SkovMonad m) 
        => PendingBlock     -- ^Block to add
        -> BlockPointer m     -- ^Parent pointer
        -> BlockPointer m    -- ^Last finalized pointer
        -> BlockState m      -- ^State
        -> m (BlockPointer m)
blockArrive block parentP lfBlockP gs = do
        let height = bpHeight parentP + 1
        curTime <- currentTime
        blockP <- makeLiveBlock block parentP lfBlockP gs curTime
        logEvent Skov LLInfo $ "Block " ++ show (getHash block :: BlockHash) ++ " arrived"
        -- Update the statistics
        updateArriveStatistics blockP
        -- Add to the branches
        finHght <- getLastFinalizedHeight
        brs <- getBranches
        let branchLen = fromIntegral $ Seq.length brs
        let insertIndex = height - finHght - 1
        if insertIndex < branchLen then
            putBranches $ brs & ix (fromIntegral insertIndex) %~ (blockP:)
        else
            if (insertIndex == branchLen) then
                putBranches $ brs Seq.|> [blockP]
            else do
                -- This should not be possible, since the parent block should either be
                -- the last finalized block (in which case insertIndex == 0)
                -- or the child of a live block (in which case insertIndex <= branchLen)
                let errMsg = "Attempted to add block at invalid height (" ++ show (theBlockHeight height) ++ ") while last finalized height is " ++ show (theBlockHeight finHght)
                logEvent Skov LLError errMsg
                error errMsg
        return blockP

doResolveBlock :: (TreeStateMonad m) => BlockHash -> m (Maybe (BlockPointer m))
{-# INLINE doResolveBlock #-}
doResolveBlock cbp = getBlockStatus cbp <&> \case
        Just (BlockAlive bp) -> Just bp
        Just (BlockFinalized bp _) -> Just bp
        _ -> Nothing

doStoreBlock :: (TreeStateMonad m, SkovMonad m, MonadWriter (Endo [SkovFinalizationEvent]) m) => SkovListeners m -> BakedBlock -> m BlockHash
{-# INLINE doStoreBlock #-}
doStoreBlock sl block0 = do
    curTime <- currentTime
    let pb = makePendingBlock block0 curTime
    let cbp = getHash pb
    oldBlock <- getBlockStatus cbp
    when (isNothing oldBlock) $ do
        -- The block is new, so we have some work to do.
        logEvent Skov LLDebug $ "Received block " ++ show cbp
        updateReceiveStatistics pb
        forM_ (blockTransactions pb) $ \tr -> doReceiveTransaction tr (blockSlot pb)
        addBlock sl pb
    return cbp

doStoreBakedBlock :: (TreeStateMonad m, SkovMonad m) => SkovListeners m 
        -> PendingBlock     -- ^Block to add
        -> BlockPointer m    -- ^Parent pointer
        -> BlockPointer m     -- ^Last finalized pointer
        -> BlockState m      -- ^State
        -> m (BlockPointer m)
doStoreBakedBlock SkovListeners{..} pb parent lastFin st = do
        bp <- blockArrive pb parent lastFin st
        onBlock bp
        return bp

doFinalizeBlock :: (TreeStateMonad m, SkovMonad m, MonadWriter (Endo [SkovFinalizationEvent]) m) => SkovListeners m -> FinalizationRecord -> m ()
{-# INLINE doFinalizeBlock #-}
doFinalizeBlock sl finRec = do
    let thisFinIx = finalizationIndex finRec
    nextFinIx <- getNextFinalizationIndex
    case compare thisFinIx nextFinIx of
        LT -> return () -- Already finalized at that index
        EQ -> do 
                addFinalizationRecordToPool finRec
                processFinalizationPool sl
        GT -> do
                logEvent Skov LLDebug $ "Requesting finalization at index " ++ show (thisFinIx - 1) ++ "."
                notifyMissingFinalization (Right $ thisFinIx - 1)
                addFinalizationRecordToPool finRec

doIsFinalized :: (TreeStateMonad m) => BlockHash -> m Bool
doIsFinalized bp = getBlockStatus bp >>= \case
        Just (BlockFinalized _ _) -> return True
        _ -> return False

doGetCurrentHeight :: (TreeStateMonad m) => m BlockHeight
doGetCurrentHeight = do
        lfHeight <- getLastFinalizedHeight
        branchLen <- fromIntegral . Seq.length <$> getBranches
        return $ lfHeight + branchLen

doBranchesFromTop :: (TreeStateMonad m) => m [[BlockPointer m]]
doBranchesFromTop = revSeqToList <$> getBranches
    where
        revSeqToList Seq.Empty = []
        revSeqToList (r Seq.:|> t) = t : revSeqToList r

doGetBlocksAtHeight :: (TreeStateMonad m) => BlockHeight -> m [BlockPointer m]
doGetBlocksAtHeight h = do
        lastFin <- getLastFinalized
        case compare h (bpHeight lastFin) of
            EQ -> return [lastFin]
            GT -> do
                brs <- getBranches
                case brs Seq.!? (fromIntegral $ h - bpHeight lastFin - 1) of
                    Nothing -> return []
                    Just bs -> return bs
            LT -> return [findFrom (bpParent lastFin)] -- TODO: replace with more efficient search
    where
        findFrom bp
            | bpHeight bp == h = bp
            | otherwise = findFrom (bpParent bp)


doReceiveTransaction :: (TreeStateMonad m) => Transaction -> Slot -> m ()
doReceiveTransaction tr slot = do
        added <- addCommitTransaction tr slot
        when added $ do
            ptrs <- getPendingTransactions
            focus <- getFocusBlock
            macct <- getAccount (bpState focus) (transactionSender tr)
            let nextNonce = maybe minNonce _accountNonce macct
            putPendingTransactions $ extendPendingTransactionTable nextNonce tr ptrs

-- NOTE: Deriving via needs undecidable instances. If we could specify type instances concretely
-- in this mechanism those would not be needed, but unfortunately this is not possible.
newtype SimpleSkovMonad s m a = SimpleSkovMonad {runSimpleSkovMonad :: StateT s m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState s)
    deriving BlockStateQuery via (Basic.SkovTreeState s (StateT s m))
    deriving BlockStateOperations via (Basic.SkovTreeState s (StateT s m))
    deriving TreeStateMonad via (Basic.SkovTreeState s (StateT s m))

instance (Monad m, Basic.SkovLenses s) => SkovQueryMonad (SimpleSkovMonad s m) where
    {-# INLINE resolveBlock #-}
    resolveBlock = doResolveBlock
    isFinalized = doIsFinalized
    lastFinalizedBlock = getLastFinalized
    getGenesisData = Concordium.GlobalState.TreeState.getGenesisData
    genesisBlock = getGenesisBlockPointer
    getCurrentHeight = doGetCurrentHeight
    branchesFromTop = doBranchesFromTop
    getBlocksAtHeight = doGetBlocksAtHeight

newtype FinalizationSkovMonad r w s m a = FinalizationSkovMonad {runFinalizationSkovMonad :: RWST r w s m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState s, MonadReader r, MonadWriter w, MonadIO)
    deriving BlockStateQuery via (Basic.SkovTreeState s (RWST r w s m))
    deriving BlockStateOperations via (Basic.SkovTreeState s (RWST r w s m))
    deriving TreeStateMonad via (Basic.SkovTreeState s (RWST r w s m))

sfsSkovListeners :: (MonadState s m, FinalizationStateLenses s, MonadReader FinalizationInstance m, FinalizationMonad m) => SkovListeners m
sfsSkovListeners = SkovListeners {
    onBlock = notifyBlockArrival,
    onFinalize = notifyBlockFinalized
}

data SkovFinalizationEvent
    = SkovFinalization FinalizationOutputEvent
    | SkovMissingBlock BlockHash
    | SkovMissingFinalization (Either BlockHash FinalizationIndex)

notifyMissingBlock :: (MonadWriter (Endo [SkovFinalizationEvent]) m) => BlockHash -> m ()
notifyMissingBlock = tell . Endo . (:) . SkovMissingBlock

notifyMissingFinalization :: (MonadWriter (Endo [SkovFinalizationEvent]) m) => Either BlockHash FinalizationIndex -> m ()
notifyMissingFinalization = tell . Endo . (:) . SkovMissingFinalization


instance (TimeMonad m, LoggerMonad m, Basic.SkovLenses s, FinalizationStateLenses s) => FinalizationMonad (FinalizationSkovMonad FinalizationInstance (Endo [SkovFinalizationEvent]) s m) where
    broadcastFinalizationMessage = tell . Endo . (:) . SkovFinalization . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . Endo . (:) . SkovFinalization . BroadcastFinalizationRecord
    requestMissingFinalization = notifyMissingFinalization . Right
    requestMissingBlock = notifyMissingBlock

instance (Monad m, Basic.SkovLenses s, Monoid w) => SkovQueryMonad (FinalizationSkovMonad FinalizationInstance w s m) where
    {-# INLINE resolveBlock #-}
    resolveBlock = doResolveBlock
    isFinalized = doIsFinalized
    lastFinalizedBlock = getLastFinalized
    getGenesisData = Concordium.GlobalState.TreeState.getGenesisData
    genesisBlock = getGenesisBlockPointer
    getCurrentHeight = doGetCurrentHeight
    branchesFromTop = doBranchesFromTop
    getBlocksAtHeight = doGetBlocksAtHeight


instance (TimeMonad m, LoggerMonad m, Basic.SkovLenses s, FinalizationStateLenses s) => SkovMonad (FinalizationSkovMonad FinalizationInstance (Endo [SkovFinalizationEvent]) s m) where
    storeBlock = doStoreBlock sfsSkovListeners
    storeBakedBlock = doStoreBakedBlock sfsSkovListeners
    receiveTransaction tr = doReceiveTransaction tr 0
    finalizeBlock = doFinalizeBlock sfsSkovListeners

instance (TimeMonad m, LoggerMonad m, Basic.SkovLenses s, FinalizationStateLenses s) => KontrolMonad (FinalizationSkovMonad FinalizationInstance (Endo [SkovFinalizationEvent]) s m)

data SkovFinalizationState = SkovFinalizationState {
    _sfsSkov :: Basic.SkovData,
    _sfsFinalization :: FinalizationState
}
makeLenses ''SkovFinalizationState

instance Basic.SkovLenses SkovFinalizationState where
    skov = sfsSkov

instance FinalizationStateLenses SkovFinalizationState where
    finState = sfsFinalization

type FSM m = FinalizationSkovMonad FinalizationInstance (Endo [SkovFinalizationEvent]) SkovFinalizationState m

initialSkovFinalizationState :: FinalizationInstance -> GenesisData -> BlockState (FSM m) -> SkovFinalizationState
initialSkovFinalizationState finInst gen initBS = SkovFinalizationState{..}
    where
        _sfsSkov = Basic.initialSkovData gen initBS
        _sfsFinalization = initialFinalizationState finInst (bpHash (Basic._skovGenesisBlockPointer _sfsSkov)) (makeFinalizationCommittee (genesisFinalizationParameters gen))


execFSM :: (Monad m) => FSM m a -> FinalizationInstance -> GenesisData -> BlockState (FSM m) -> m a
execFSM (FinalizationSkovMonad a) fi gd bs0 = fst <$> evalRWST a fi (initialSkovFinalizationState fi gd bs0)

runFSM :: FSM m a -> FinalizationInstance -> SkovFinalizationState -> m (a, SkovFinalizationState, Endo [SkovFinalizationEvent])
runFSM (FinalizationSkovMonad a) fi fs = runRWST a fi fs


evalSSM :: (Monad m) => SimpleSkovMonad s m a -> s -> m a
evalSSM (SimpleSkovMonad a) st = evalStateT a st
