{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    ViewPatterns #-}
module Concordium.Skov.Update where

import Control.Monad
import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Foldable

import GHC.Stack

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Updates
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Block as GB (PendingBlock(..))
import Concordium.GlobalState.Block hiding (PendingBlock)
import Concordium.GlobalState.Finalization
import Concordium.Types.Transactions
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.AccountTransactionIndex

import Concordium.Scheduler.TreeStateEnvironment(executeFrom, ExecutionResult'(..), ExecutionResult, FinalizerInfo)

import Concordium.Kontrol
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.UpdateLeaderElectionParameters
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Skov.Statistics
import Data.Maybe (fromMaybe)


-- |Determine if one block is an ancestor of another.
-- A block is considered to be an ancestor of itself.
isAncestorOf :: BlockPointerMonad m => BlockPointerType m -> BlockPointerType m -> m Bool
isAncestorOf b1 b2 = case compare (bpHeight b1) (bpHeight b2) of
        GT -> return False
        EQ -> return (b1 == b2)
        LT -> do
          parent <- bpParent b2
          isAncestorOf b1 parent

-- |Update the focus block, together with the pending transaction table.
updateFocusBlockTo :: (TreeStateMonad m) => BlockPointerType m -> m ()
updateFocusBlockTo newBB = do
        oldBB <- getFocusBlock
        pts <- getPendingTransactions
        upts <- updatePTs oldBB newBB [] pts
        putPendingTransactions upts
        putFocusBlock newBB
    where
        updatePTs :: (BlockPointerMonad m) => BlockPointerType m -> BlockPointerType m -> [BlockPointerType m] -> PendingTransactionTable -> m PendingTransactionTable
        updatePTs oBB nBB forw pts = case compare (bpHeight oBB) (bpHeight nBB) of
                LT -> do
                  parent <- (bpParent nBB)
                  updatePTs oBB parent (nBB : forw) pts
                EQ -> if oBB == nBB then
                            return $ foldl (\p f-> forwardPTT (blockTransactions f) p) pts forw
                        else do
                            parent1 <- (bpParent oBB)
                            parent2 <- (bpParent nBB)
                            updatePTs parent1 parent2 (nBB : forw) (reversePTT (blockTransactions oBB) pts)
                GT -> do
                  parent <- bpParent oBB
                  updatePTs parent nBB forw (reversePTT (blockTransactions oBB) pts)

makeFinalizerInfo :: FinalizationCommittee -> FinalizerInfo
makeFinalizerInfo = fmap finfo . parties
    where
        finfo p = (partyBakerId p, partyWeight p)

-- |A monad implementing 'OnSkov' provides functions for responding to
-- a block being added to the tree, and a finalization record being verified.
-- It also provides a function for logging transfers at finalization time.
class OnSkov m where
    -- |Called when a block arrives.
    onBlock :: BlockPointerType m -> m ()
    -- |Called when a finalization record is validated.  This is
    -- only called for the block that is explicitly finalized (i.e.
    -- once per finalization record).
    onFinalize :: FinalizationRecord -> BlockPointerType m -> m ()
    -- |Called when a block or finalization record that was previously
    -- pending becomes live.
    onPendingLive :: m ()

-- |Handle a block arriving that is dead.  That is, the block has never
-- been in the tree before, and now it never can be.  Any descendents of
-- this block that have previously arrived cannot have been added to the
-- tree, and we purge them recursively from '_skovPossiblyPendingTable'.
blockArriveDead :: (HasCallStack, BlockPointerMonad m, MonadLogger m, TreeStateMonad m) => BlockHash -> m ()
blockArriveDead cbp = do
        markDead cbp
        logEvent Skov LLDebug $ "Block " ++ show cbp ++ " arrived dead"
        children <- map getHash <$> takePendingChildren cbp
        forM_ children blockArriveDead

-- |Purge pending blocks with slot numbers predating the last finalized slot.
purgePending :: (HasCallStack, TreeStateMonad m, MonadLogger m) => m ()
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

doTrustedFinalize :: (TreeStateMonad m, SkovMonad m, OnSkov m) => FinalizationRecord -> m (Either UpdateResult (BlockPointerType m))
doTrustedFinalize finRec =
    getBlockStatus (finalizationBlockPointer finRec) >>= \case
        Just (BlockAlive bp) -> Right bp <$ processFinalization bp finRec
        Just BlockDead -> return $ Left ResultInvalid
        Just BlockFinalized{} -> return $ Left ResultInvalid
        Just BlockPending{} -> return $ Left ResultUnverifiable
        Nothing -> return $ Left ResultUnverifiable

-- |Process the finalization of a block.  The following are assumed:
--
-- * The block is either live or finalized.
-- * The finalization record is valid and finalizes the given block.
processFinalization :: forall m. (TreeStateMonad m, SkovMonad m, OnSkov m) => BlockPointerType m -> FinalizationRecord -> m ()
processFinalization newFinBlock finRec@FinalizationRecord{..} = do
    nextFinIx <- getNextFinalizationIndex
    when (nextFinIx == finalizationIndex) $ do
        -- We actually have a new block to finalize.
        logEvent Skov LLInfo $ "Block " ++ show (bpHash newFinBlock) ++ " is finalized at height " ++ show (theBlockHeight $ bpHeight newFinBlock) ++ " with finalization delta=" ++ show finalizationDelay
        updateFinalizationStatistics
        -- Check if the focus block is a descendent of the block we are finalizing
        focusBlockSurvives <- isAncestorOf newFinBlock =<< getFocusBlock
        -- If not, update the focus to the new finalized block.
        -- This is to ensure that the focus block is always a live (or finalized) block.
        unless focusBlockSurvives $ updateFocusBlockTo newFinBlock
        lastFinHeight <- getLastFinalizedHeight
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
            pruneTrunk :: BlockPointerType m -> Branches m -> m ()
            pruneTrunk _ Seq.Empty = return ()
            pruneTrunk keeper (brs Seq.:|> l) = do
                forM_ l $ \bp -> if bp == keeper then do
                                    markFinalized (getHash bp) finRec
                                    logEvent Skov LLDebug $ "Block " ++ show bp ++ " marked finalized"
                                else do
                                    markLiveBlockDead bp
                                    logEvent Skov LLDebug $ "Block " ++ show bp ++ " marked dead"
                parent <- bpParent keeper
                pruneTrunk parent brs
                -- Finalize the transactions of the surviving block.
                -- (This is handled in order of finalization.)
                finalizeTransactions (getHash keeper) (blockSlot keeper) (blockTransactions keeper)
                ati <- bpTransactionAffectSummaries keeper
                bcTime <- getSlotTimestamp (blockSlot keeper)
                let ctx = BlockContext{
                        bcHash = getHash keeper,
                        bcHeight = bpHeight keeper,
                        ..}
                flushBlockSummaries ctx ati =<< getSpecialOutcomes =<< blockState keeper

        pruneTrunk newFinBlock (Seq.take pruneHeight oldBranches)
        -- Archive the states of blocks up to but not including the new finalized block
        let doArchive b = case compare (bpHeight b) lastFinHeight of
                LT -> return ()
                EQ -> archiveBlockState =<< blockState b
                GT -> do
                        doArchive =<< bpParent b
                        archiveBlockState =<< blockState b
        doArchive =<< bpParent newFinBlock
        -- Prune the branches: mark dead any block that doesn't descend from
        -- the newly-finalized block.
        let
            pruneBranches _ Seq.Empty = return Seq.empty
            pruneBranches parents (brs Seq.:<| rest) = do
                survivors <- foldrM (\bp l -> do
                    parent <- bpParent bp
                    if parent `elem` parents then
                        return (bp:l)
                    else do
                        markLiveBlockDead bp
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
addBlock :: forall m. (HasCallStack, TreeStateMonad m, SkovMonad m, FinalizationMonad m, OnSkov m) => PendingBlock -> m UpdateResult
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
        invalidBlock :: String -> m UpdateResult
        invalidBlock reason = do
            logEvent Skov LLWarning $ "Block is not valid (" ++ reason ++ "): " ++ show block
            blockArriveDead $ getHash block
            return ResultInvalid
        parent = blockPointer block
        check reason q a = if q then a else invalidBlock reason
        tryAddLiveParent :: BlockPointerType m -> m UpdateResult
        tryAddLiveParent parentP = -- The parent block must be Alive or Finalized here.
            -- Determine if the block's finalized data is valid and if so what
            -- its last finalized block pointer should be.
            case blockFinalizationData block of
                -- If the block contains no finalization data, it is trivially valid and
                -- inherits the last finalized pointer from the parent.
                NoFinalizationData -> tryAddParentLastFin parentP Nothing =<< bpLastFinalized parentP
                -- If the block contains a finalization record...
                BlockFinalizationData finRec@FinalizationRecord{finalizationBlockPointer=finBP,..} -> do
                    -- Get whichever block was finalized at the previous index.
                    -- We do this before calling finalization because there is a (slightly) greater
                    -- chance that this is the last finalized block, which saves a DB lookup.
                    previousFinalized <- fmap finalizationBlockPointer <$> recordAtFinIndex (finalizationIndex - 1)
                    -- send it for finalization processing
                    finOK <- finalizationReceiveRecord True finRec >>= \case
                        ResultSuccess ->
                            -- In this event, we can be sure that the finalization record
                            -- was used to finalize a block; so in particular, the block it
                            -- finalizes is the named one.
                            -- Check that the parent block is still live: potentially, the
                            -- block might not be descended from the one it has a finalization
                            -- record for.  Furthermore, if the parent block is finalized now,
                            -- it has to be the last finalized block.
                            getBlockStatus (bpHash parentP) >>= \case
                                Just BlockAlive{} -> return True
                                Just BlockFinalized{} -> do
                                    -- The last finalized block may have changed as a result
                                    -- of the call to finalizationReceiveRecord.
                                    (lf, _) <- getLastFinalized
                                    return (parentP == lf)
                                _ -> return False
                        ResultDuplicate -> return True
                        _ -> return False
                    -- check that the finalized block at the previous index
                    -- is the last finalized block of the parent
                    check "invalid finalization" (finOK && previousFinalized == Just (bpLastFinalizedHash  parentP)) $
                        finalizationUnsettledRecordAt finalizationIndex >>= \case
                            Nothing -> invalidBlock $ "no unsettled finalization at index " ++ show finalizationIndex
                            Just (_,committee,_) ->
                                -- Check that the finalized block at the given index
                                -- is actually the one named in the finalization record.
                                blockAtFinIndex finalizationIndex >>= \case
                                    Just fbp -> check "finalization inconsistency" (bpHash fbp == finBP) $
                                                    tryAddParentLastFin parentP (Just (makeFinalizerInfo committee)) fbp
                                    Nothing -> invalidBlock $ "no finalized block at index " ++ show finalizationIndex
        tryAddParentLastFin :: BlockPointerType m -> Maybe FinalizerInfo -> BlockPointerType m -> m UpdateResult
        tryAddParentLastFin parentP mfinInfo lfBlockP =
            -- Check that the blockSlot is beyond the parent slot
            check ("block slot (" ++ show (blockSlot block) ++ ") not later than parent block slot (" ++ show (blockSlot parentP) ++ ")") (blockSlot parentP < blockSlot block) $ do
                -- get Birk parameters from the __parent__ block. The baker must have existed in that
                -- block's state in order that the current block is valid
                parentState <- blockState parentP
                -- Determine the baker and its lottery power
                bakers <- getSlotBakers parentState (blockSlot block)
                let baker = lotteryBaker bakers (blockBaker block)
                -- Determine the leadership election nonce
                parentSeedState <- getSeedState parentState
                let nonce = computeLeadershipElectionNonce parentSeedState (blockSlot block)
                -- Determine the election difficulty
                slotTime <- getSlotTimestamp (blockSlot block)
                elDiff <- getElectionDifficulty parentState slotTime
                logEvent Skov LLTrace $ "Verifying block with election difficulty " ++ show elDiff
                case baker of
                    Nothing -> invalidBlock $ "unknown baker " ++ show (blockBaker block)
                    Just (BakerInfo{..}, lotteryPower) ->
                        -- Check the block proof
                        check "invalid block proof" (verifyProof
                                    nonce
                                    elDiff
                                    (blockSlot block)
                                    _bakerElectionVerifyKey
                                    lotteryPower
                                    (blockProof block)) $
                        -- The block nonce
                        check "invalid block nonce" (verifyBlockNonce
                                    nonce
                                    (blockSlot block)
                                    _bakerElectionVerifyKey
                                    (blockNonce block)) $
                        -- And check baker key matches claimed key.
                        -- The signature is checked using the claimed key already in doStoreBlock for blocks which were received from the network.
                        check "Baker key claimed in block did not match actual baker key" (_bakerSignatureVerifyKey == blockBakerKey block) $ do
                            -- Update the seed state with the block nonce
                            let newSeedState = updateSeedState (blockSlot block) (blockNonce block) parentSeedState
                            let ts = blockTransactions block
                            executeFrom (getHash block) (blockSlot block) slotTime parentP (blockBaker block) mfinInfo newSeedState ts >>= \case
                                Left err -> do
                                    logEvent Skov LLWarning ("Block execution failure: " ++ show err)
                                    invalidBlock "execution failure"
                                Right result -> do
                                    -- Check that the StateHash is correct
                                    stateHash <- getStateHash (_finalState result)
                                    check "Claimed stateHash did not match calculated stateHash"(stateHash == blockStateHash block) $ do
                                        -- Check that the TransactionOutcomeHash is correct
                                        tohash <- getTransactionOutcomesHash (_finalState result) 
                                        check "Claimed transactionOutcomesHash did not match actual transactionOutcomesHash"(tohash ==  blockTransactionOutcomesHash block) $ do
                                            -- Add the block to the tree
                                            blockP <- blockArrive block parentP lfBlockP result
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
-- This is used by 'addBlock' and 'doBakeForSlot', and should not
-- be called directly otherwise.
blockArrive :: (HasCallStack, TreeStateMonad m, SkovMonad m)
        => PendingBlock           -- ^Block to add
        -> BlockPointerType m     -- ^Parent pointer
        -> BlockPointerType m    -- ^Last finalized pointer
        -> ExecutionResult m -- ^Result of block execution (state, energy used, ...)
        -> m (BlockPointerType m)
blockArrive block parentP lfBlockP ExecutionResult{..} = do
        let height = bpHeight parentP + 1
        curTime <- currentTime
        blockP <- makeLiveBlock block parentP lfBlockP _finalState _transactionLog curTime _energyUsed
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
doStoreBlock :: (TreeStateMonad m, FinalizationMonad m, SkovMonad m, OnSkov m) => PendingBlock -> m UpdateResult
{- - INLINE doStoreBlock - -}
doStoreBlock pb@GB.PendingBlock{..} = unlessShutDown $ do
    let cbp = getHash pb
        BakedBlock{..} = pbBlock
    oldBlock <- getBlockStatus cbp
    case oldBlock of
        Nothing ->  
            -- Check that the claimed key matches the signature/blockhash
            checkClaimedSignature pb $ do
            -- The block is new, so we have some work to do.
            logEvent Skov LLDebug $ "Received block " ++ show pb
            txList <- sequence <$> forM (blockTransactions pb) (\tr -> fst <$> doReceiveTransactionInternal tr (blockSlot pb))
            case txList of
              Nothing -> do
                blockArriveDead cbp
                return ResultInvalid
              Just newTransactions -> do
                purgeTransactionTable False =<< currentTime
                let block1 = GB.PendingBlock{pbBlock = BakedBlock{bbTransactions = newTransactions, ..}, ..}
                updateReceiveStatistics block1
                addBlock block1
        Just _ -> return ResultDuplicate
    where
        checkClaimedSignature b a = if verifyBlockSignature b then a else do
            logEvent Skov LLWarning $ "Dropping block where signature did not match claimed key or blockhash: " 
            return ResultInvalid

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
--   * 'ResultShutDown' which indicates that consensus was shut down, and so the transaction was not added.
doReceiveTransaction :: (TreeStateMonad m, TimeMonad m, SkovQueryMonad m) => BlockItem -> Slot -> m UpdateResult
doReceiveTransaction tr slot = unlessShutDown $ do
  (_, ur) <- doReceiveTransactionInternal tr slot
  when (ur == ResultSuccess) $ purgeTransactionTable False =<< currentTime
  return ur

-- |Add a transaction to the transaction table.  The 'Slot' should be
-- the slot number of the block that the transaction was received with.
-- This function should only be called when a transaction is received as part of a block.
-- The difference from the above function is that this function returns an already existing
-- transaction in case of a duplicate, ensuring more sharing of transaction data.
doReceiveTransactionInternal :: (TreeStateMonad m) => BlockItem -> Slot -> m (Maybe BlockItem, UpdateResult)
doReceiveTransactionInternal tr slot =
        addCommitTransaction tr slot >>= \case
          Added bi@WithMetadata{..} -> do
              ptrs <- getPendingTransactions
              case wmdData of
                NormalTransaction tx -> do
                  focus <- getFocusBlock
                  st <- blockState focus
                  macct <- getAccount st $! transactionSender tx
                  nextNonce <- fromMaybe minNonce <$> mapM getAccountNonce macct
                  -- If a transaction with this nonce has already been run by
                  -- the focus block, then we do not need to add it to the
                  -- pending transactions.  Otherwise, we do.
                  when (nextNonce <= transactionNonce tx) $
                      putPendingTransactions $! addPendingTransaction nextNonce WithMetadata{wmdData=tx,..} ptrs
                CredentialDeployment _ ->
                  putPendingTransactions $! addPendingDeployCredential wmdHash ptrs
                ChainUpdate cu -> do
                    focus <- getFocusBlock
                    st <- blockState focus
                    nextSN <- getNextUpdateSequenceNumber st (updateType (uiPayload cu))
                    when (nextSN <= updateSeqNumber (uiHeader cu)) $
                        putPendingTransactions $! addPendingUpdate nextSN cu ptrs
              return (Just bi, ResultSuccess)
          Duplicate tx -> return (Just tx, ResultDuplicate)
          ObsoleteNonce -> return (Nothing, ResultStale)
