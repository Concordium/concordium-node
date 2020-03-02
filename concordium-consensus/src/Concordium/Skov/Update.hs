{-# LANGUAGE
    ScopedTypeVariables,
    ViewPatterns #-}
module Concordium.Skov.Update where

import Control.Monad
import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Foldable
import Data.Maybe

import GHC.Stack

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.Types.Transactions
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers

import Concordium.Scheduler.TreeStateEnvironment(executeFrom)


import Concordium.Kontrol
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.UpdateLeaderElectionParameters
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Skov.Statistics

-- |Determine if one block is an ancestor of another.
-- A block is considered to be an ancestor of itself.
isAncestorOf :: BlockPointerData bp => bp -> bp -> Bool
isAncestorOf b1 b2 = case compare (bpHeight b1) (bpHeight b2) of
        GT -> False
        EQ -> b1 == b2
        LT -> isAncestorOf b1 (bpParent b2)

-- |Update the focus block, together with the pending transaction table.
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

-- |A monad implementing 'OnSkov' provides functions for responding to
-- a block being added to the tree, and a finalization record being verified.
-- It also provides a function for logging transfers at finalization time.
class OnSkov m where
    -- |Called when a block arrives.
    onBlock :: BlockPointer m -> m ()
    -- |Called when a finalization record is validated.  This is
    -- only called for the block that is explicitly finalized (i.e.
    -- once per finalization record).
    onFinalize :: FinalizationRecord -> BlockPointer m -> m ()
    -- |Called when a block or finalization record that was previously
    -- pending becomes live.
    onPendingLive :: m ()
    -- |A function to log transfers at finalization time. Since it is
    -- potentially expensive to even keep track of events we make it an
    -- explicitly optional value to short-circuit evaluation.
    logTransfer :: m (Maybe (BlockHash -> Slot -> TransferReason -> m ()))

-- |Log transfers in the given block using the 'logTransfer' method of the
-- 'OnSkov' class.
logTransfers :: (TreeStateMonad m, OnSkov m, LoggerMonad m) => BlockPointer m -> m ()
logTransfers bp = logTransfer >>= \case
  Nothing -> return ()
  Just logger -> do
    state <- blockState bp
    case blockFields bp of
      Nothing -> return ()  -- don't do anything for the genesis block
      Just fields -> do
        forM_ (blockTransactions bp) $ \tx ->
          getTransactionOutcome state (trHash tx) >>= \case
            Nothing ->
              logEvent Skov LLDebug $ "Could not retrieve transaction outcome in block " ++
                                      show (bpHash bp) ++
                                      " for transaction " ++
                                      show (trHash tx)
            Just outcome ->
              mapM_ (logger (bpHash bp) (blockSlot bp)) (resultToReasons fields tx outcome)
        special <- getSpecialOutcomes state
        mapM_ (logger (bpHash bp) (blockSlot bp) . specialToReason fields) special


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
                                let
                                    isPending Nothing = True
                                    isPending (Just BlockPending{}) = True
                                    isPending _ = False
                                when (isPending pbStatus) $ blockArriveDead pb
                                purgeLoop
        purgeLoop

doTrustedFinalize :: (TreeStateMonad m, SkovMonad m, OnSkov m) => FinalizationRecord -> m (Either UpdateResult (BlockPointer m))
doTrustedFinalize finRec =
    getBlockStatus (finalizationBlockPointer finRec) >>= \case
        Just (BlockAlive bp) -> Right bp <$ processFinalization bp finRec
        Just BlockDead -> return $ Left ResultInvalid
        Just BlockFinalized{} -> return $ Left ResultInvalid
        Just BlockPending{} -> return $ Left ResultUnverifiable
        Nothing -> return $ Left ResultInvalid

-- |Process the finalization of a block.  The following are assumed:
--
-- * The block is either live or finalized.
-- * The finalization record is valid and finalizes the given block.
processFinalization :: forall m. (TreeStateMonad m, SkovMonad m, OnSkov m) => BlockPointer m -> FinalizationRecord -> m ()
processFinalization newFinBlock finRec@FinalizationRecord{..} = do
    nextFinIx <- getNextFinalizationIndex
    when (nextFinIx == finalizationIndex) $ do
        -- We actually have a new block to finalize.
        logEvent Skov LLInfo $ "Block " ++ show (bpHash newFinBlock) ++ " is finalized at height " ++ show (theBlockHeight $ bpHeight newFinBlock) ++ " with finalization delta=" ++ show finalizationDelay
        updateFinalizationStatistics
        -- Check if the focus block is a descendent of the block we are finalizing
        focusBlockSurvives <- isAncestorOf newFinBlock <$> getFocusBlock
        -- If not, update the focus to the new finalized block.
        -- This is to ensure that the focus block is always a live (or finalized) block.
        unless focusBlockSurvives $ updateFocusBlockTo newFinBlock
        -- Archive the states of blocks up to but not including the new finalized block
        lastFinHeight <- getLastFinalizedHeight
        let doArchive b = case compare (bpHeight b) lastFinHeight of
                LT -> return ()
                EQ -> archiveBlockState =<< blockState b
                GT -> doArchive (bpParent b) >> blockState b >>= archiveBlockState
        doArchive (bpParent newFinBlock)
        -- Add the finalization to the finalization list
        -- TODO: The way this is stored will probably change.
        addFinalization newFinBlock finRec
        -- Prune the branches, which consist of all the non-finalized blocks
        -- grouped by block height.
        oldBranches <- getBranches
        -- 'pruneHeight' is the number of blocks that are being finalized
        -- as a result of the finalization record.
        let pruneHeight = fromIntegral (bpHeight newFinBlock - lastFinHeight)
        -- First, prune the trunk: the section of the branches beyond the
        -- last finalized block up to and including the new finalized block.
        -- We proceed backwards from the new finalized block, marking it and
        -- its ancestors as finalized, while other blocks at the same height
        -- are marked dead.
        let
            pruneTrunk :: BlockPointer m -> Branches m -> m ()
            pruneTrunk _ Seq.Empty = return ()
            pruneTrunk keeper (brs Seq.:|> l) = do
                forM_ l $ \bp -> if bp == keeper then do
                                    markFinalized (getHash bp) finRec
                                    logEvent Skov LLDebug $ "Block " ++ show bp ++ " marked finalized"
                                else do
                                    markDead (getHash bp)
                                    -- Purge the state of a dead block.
                                    purgeBlockState =<< blockState bp
                                    logEvent Skov LLDebug $ "Block " ++ show bp ++ " marked dead"
                pruneTrunk (bpParent keeper) brs
                -- Finalize the transactions of the survivng block.
                -- (This is handled in order of finalization.)
                finalizeTransactions (blockTransactions keeper)
                logTransfers keeper
        pruneTrunk newFinBlock (Seq.take pruneHeight oldBranches)
        -- Prune the branches: mark dead any block that doesn't descend from
        -- the newly-finalized block.
        let
            pruneBranches _ Seq.Empty = return Seq.empty
            pruneBranches parents (brs Seq.:<| rest) = do
                survivors <- foldrM (\bp l ->
                    if bpParent bp `elem` parents then
                        return (bp:l)
                    else do
                        markDead (bpHash bp)
                        purgeBlockState =<< blockState bp
                        logEvent Skov LLDebug $ "Block " ++ show (bpHash bp) ++ " marked dead"
                        return l)
                    [] brs
                rest' <- pruneBranches survivors rest
                return (survivors Seq.<| rest')
        unTrimmedBranches <- pruneBranches [newFinBlock] (Seq.drop pruneHeight oldBranches)
        -- This removes empty lists at the end of branches which can result in finalizing on a
        -- block not in the current best local branch
        let 
            trimBranches Seq.Empty = return Seq.Empty
            trimBranches prunedbrs@(xs Seq.:|> x) =
                case x of
                    [] -> trimBranches xs
                    _ -> return prunedbrs
        newBranches <- trimBranches unTrimmedBranches
        putBranches newBranches
        -- purge pending blocks with slot numbers predating the last finalized slot
        purgePending
        onFinalize finRec newFinBlock

-- |Try to add a block to the tree.  There are three possible outcomes:
--
-- 1. The block is determined to be invalid in the current tree.
--    In this case, the block is marked dead.
-- 2. The block is pending the arrival of its parent block, or
--    the finalization of its last finalized block.  In this case
--    it is added to the appropriate pending queue.  'addBlock'
--    should be called again when the pending criterion is fulfilled.
-- 3. The block is determined to be valid and added to the tree.
addBlock :: forall m. (HasCallStack, TreeStateMonad m, SkovMonad m, FinalizationMonad m, OnSkov m) => PendingBlock m -> m UpdateResult
addBlock block = do
        lfs <- getLastFinalizedSlot
        -- The block must be later than the last finalized block
        if lfs >= blockSlot block then deadBlock else do
            parentStatus <- getBlockStatus parent
            case parentStatus of
                Nothing -> do
                    addPendingBlock block
                    markPending block
                    logEvent Skov LLDebug $ "Block " ++ show block ++ " is pending its parent (" ++ show parent ++ ")"
                    return ResultPendingBlock
                Just (BlockPending _) -> do
                    addPendingBlock block
                    markPending block
                    logEvent Skov LLDebug $ "Block " ++ show block ++ " is pending, since its parent is pending"
                    return ResultPendingBlock
                Just BlockDead -> deadBlock
                Just (BlockAlive parentP) -> tryAddLiveParent parentP
                Just (BlockFinalized parentP _) -> do
                    (lfb, _) <- getLastFinalized
                    -- If the parent is finalized, it had better be the last finalized, or else the block is already dead
                    if parentP /= lfb then deadBlock else tryAddLiveParent parentP
    where
        deadBlock :: m UpdateResult
        deadBlock = do
            blockArriveDead $ getHash block
            return ResultStale
        invalidBlock :: m UpdateResult
        invalidBlock = do
            logEvent Skov LLWarning $ "Block is not valid: " ++ show block
            blockArriveDead $ getHash block
            return ResultInvalid
        parent = blockPointer block
        check q a = if q then a else invalidBlock
        tryAddLiveParent :: BlockPointer m -> m UpdateResult
        tryAddLiveParent parentP = -- The parent block must be Alive or Finalized here.
            -- Determine if the block's finalized data is valid and if so what
            -- its last finalized block pointer should be.
            case blockFinalizationData block of
                -- If the block contains no finalization data, it is trivially valid and
                -- inherits the last finalized pointer from the parent.
                NoFinalizationData -> tryAddParentLastFin parentP (bpLastFinalized parentP)
                -- If the block contains a finalization record...
                BlockFinalizationData finRec@FinalizationRecord{..} -> do
                    -- send it to for finalization processing
                    finOK <- finalizationReceiveRecord True finRec >>= \case
                        ResultSuccess ->
                            -- In this event, we can be sure that the finalization record
                            -- was used to finalize a block; so in particular, the block it
                            -- finalizes is the named one.
                            -- Check that the parent block is still live: potentially, the
                            -- block might not be descended from the one it has a finalization
                            -- record for.
                            isJust <$> resolveBlock (bpHash parentP)
                        ResultDuplicate -> return True
                        _ -> return False
                    check finOK $ do
                        -- check that the finalized block at the previous index
                        -- is the last finalized block of the parent
                        previousFinalized <- getFinalizedAtIndex (finalizationIndex - 1)
                        check (previousFinalized == Just (bpLastFinalized parentP)) $
                            -- Check that the finalized block at the given index
                            -- is actually the one named in the finalization record.
                            getFinalizedAtIndex finalizationIndex >>= \case
                                Just fbp -> check (bpHash fbp == finalizationBlockPointer) $
                                                tryAddParentLastFin parentP fbp
                                Nothing -> invalidBlock
        tryAddParentLastFin :: BlockPointer m -> BlockPointer m -> m UpdateResult
        tryAddParentLastFin parentP lfBlockP =
            -- Check that the blockSlot is beyond the parent slot
            check (blockSlot parentP < blockSlot block) $ do
                -- get Birk parameters from the __parent__ block. The baker must have existed in that
                -- block's state in order that the current block is valid
                bps@BirkParameters{..} <- getBirkParameters (blockSlot block) parentP
                case birkEpochBaker (blockBaker block) bps of
                    Nothing -> invalidBlock
                    Just (BakerInfo{..}, lotteryPower) ->
                        -- Check the block proof
                        check (verifyProof
                                    (_birkLeadershipElectionNonce bps)
                                    _birkElectionDifficulty
                                    (blockSlot block)
                                    _bakerElectionVerifyKey
                                    lotteryPower
                                    (blockProof block)) $
                        -- The block nonce
                        check (verifyBlockNonce
                                    (_birkLeadershipElectionNonce bps)
                                    (blockSlot block)
                                    _bakerElectionVerifyKey
                                    (blockNonce block)) $
                        -- And the block signature
                        check (verifyBlockSignature _bakerSignatureVerifyKey block) $ do
                            let ts = blockTransactions block
                            -- possibly add the block nonce in the seed state
                                bps' = bps{_birkSeedState = updateSeedState (blockSlot block) (blockNonce block) _birkSeedState}
                            slotTime <- getSlotTimestamp (blockSlot block)
                            executeFrom (blockSlot block) slotTime parentP lfBlockP (blockBaker block) bps' ts >>= \case
                                Left err -> do
                                    logEvent Skov LLWarning ("Block execution failure: " ++ show err)
                                    invalidBlock
                                Right (gs, energyUsed) -> do
                                    -- If necessary, finalize
                                    case blockFinalizationData block of
                                        BlockFinalizationData fr -> processFinalization lfBlockP fr
                                        _ -> return ()
                                    -- Add the block to the tree
                                    blockP <- blockArrive block parentP lfBlockP gs energyUsed
                                    -- Notify of the block arrival (for finalization)
                                    finalizationBlockArrival blockP
                                    onBlock blockP
                                    -- Process finalization records
                                    -- Handle any blocks that are waiting for this one
                                    children <- takePendingChildren (getHash block)
                                    forM_ children $ \childpb -> do
                                        childStatus <- getBlockStatus (getHash childpb)
                                        let
                                            isPending Nothing = True
                                            isPending (Just (BlockPending _)) = True
                                            isPending _ = False
                                        when (isPending childStatus) $ addBlock childpb >>= \case
                                            ResultSuccess -> onPendingLive
                                            _ -> return ()
                                    return ResultSuccess

-- |Add a valid, live block to the tree.
-- This is used by 'addBlock' and 'doStoreBakedBlock', and should not
-- be called directly otherwise.
blockArrive :: (HasCallStack, TreeStateMonad m, SkovMonad m) 
        => PendingBlock m    -- ^Block to add
        -> BlockPointer m     -- ^Parent pointer
        -> BlockPointer m    -- ^Last finalized pointer
        -> BlockState m      -- ^State
        -> Energy            -- ^Energy used by transactions in the block
        -> m (BlockPointer m)
blockArrive block parentP lfBlockP gs energyUsed = do
        let height = bpHeight parentP + 1
        curTime <- currentTime
        blockP <- makeLiveBlock block parentP lfBlockP gs curTime energyUsed
        logEvent Skov LLInfo $ "Block " ++ show block ++ " arrived"
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
            if insertIndex == branchLen then
                putBranches $ brs Seq.|> [blockP]
            else do
                -- This should not be possible, since the parent block should either be
                -- the last finalized block (in which case insertIndex == 0)
                -- or the child of a live block (in which case insertIndex <= branchLen)
                let errMsg = "Attempted to add block at invalid height (" ++ show (theBlockHeight height) ++ ") while last finalized height is " ++ show (theBlockHeight finHght)
                logEvent Skov LLError errMsg
                error errMsg
        return blockP

-- |Store a block (as received from the network) in the tree.
-- This checks for validity of the block, and may add the block
-- to a pending queue if its prerequisites are not met.
doStoreBlock :: (TreeStateMonad m, FinalizationMonad m, SkovMonad m, OnSkov m) => PendingBlock m -> m UpdateResult
{-# INLINE doStoreBlock #-}
doStoreBlock = \pb -> do
    let cbp = getHash pb
    oldBlock <- getBlockStatus cbp
    case oldBlock of
        Nothing -> do
            -- The block is new, so we have some work to do.
            logEvent Skov LLDebug $ "Received block " ++ show pb
            txList <- sequence <$> forM (blockTransactions pb) (\tr -> fst <$> doReceiveTransactionInternal tr (blockSlot pb))
            case txList of
              Nothing -> do
                blockArriveDead cbp
                return ResultInvalid
              Just newTransactions -> do
                newPb <- updateBlockTransactions newTransactions pb
                updateReceiveStatistics newPb
                addBlock newPb
        Just _ -> return ResultDuplicate

-- |Store a block that is baked by this node in the tree.  The block
-- is presumed to be valid.
doStoreBakedBlock :: (TreeStateMonad m, SkovMonad m, FinalizationMonad m, OnSkov m)
        => PendingBlock m     -- ^Block to add
        -> BlockPointer m    -- ^Parent pointer
        -> BlockPointer m     -- ^Last finalized pointer
        -> BlockState m      -- ^State
        -> Energy            -- ^Energy used by transactions in this block
        -> m (BlockPointer m)
{-# INLINE doStoreBakedBlock #-}
doStoreBakedBlock = \pb parent lastFin st energyUsed -> do
        bp <- blockArrive pb parent lastFin st energyUsed
        finalizationBlockArrival bp
        onBlock bp
        return bp

-- |Add a transaction to the transaction table.  The 'Slot' should be
-- the slot number of the block that the transaction was received with,
-- and 0 if the transaction was received separately from a block.
-- This returns
--
--   * 'ResultSuccess' if the transaction is freshly added.
--   * 'ResultDuplicate', which indicates that either the transaction is a duplicate
--   * 'ResultStale' which indicates that a transaction with the same sender
--     and nonce has already been finalized. In this case the transaction is not added to the table.
--   * 'ResultInvalid' which indicates that the transaction signature was invalid.
doReceiveTransaction :: (TreeStateMonad m) => Transaction -> Slot -> m UpdateResult
doReceiveTransaction tr slot = snd <$> doReceiveTransactionInternal tr slot

-- |Add a transaction to the transaction table.  The 'Slot' should be
-- the slot number of the block that the transaction was received with.
-- This function should only be called when a transaction is received as part of a block.
-- The difference from the above function is that this function returns an already existing
-- transaction in case of a duplicate, ensuring more sharing of transaction data.
doReceiveTransactionInternal :: (TreeStateMonad m) => Transaction -> Slot -> m (Maybe Transaction, UpdateResult)
doReceiveTransactionInternal tr slot =
        addCommitTransaction tr slot >>= \case
          Added tx -> do
              ptrs <- getPendingTransactions
              focus <- getFocusBlock
              st <- blockState focus
              macct <- getAccount st $! transactionSender tr
              let nextNonce = maybe minNonce _accountNonce macct
              -- If a transaction with this nonce has already been run by
              -- the focus block, then we do not need to add it to the
              -- pending transactions.  Otherwise, we do.
              when (nextNonce <= transactionNonce tr) $
                  putPendingTransactions $! extendPendingTransactionTable nextNonce tx ptrs
              return (Just tx, ResultSuccess)
          Duplicate tx -> return (Just tx, ResultDuplicate)
          ObsoleteNonce -> return (Nothing, ResultStale)
