{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module implements the catch-up mechanism for consensus version 1.
--
-- The catch-up mechanism defines three types of catch-up messages, represented by 'CatchUpMessage':
--    * A catch-up status message ('CatchUpStatusMessage') is used to inform a peer that this node
--      may have relevant data that it has not sent to the peer. The message includes a summary of
--      the node's state allowing the peer to determine if catch-up is necessary.
--    * A catch-up request message ('CatchUpRequestMessage') is used to request catch-up from a
--      peer. The message also includes a summary of the node's state (but with a bit more detail
--      on the branches of the block tree than for the status message), which is used by the peer
--      to determine what data should be sent in order to update the node. The peer should respond
--      by sending a number of blocks (possibly 0, if no catch-up is required) followed by a
--      catch-up response message.
--    * A catch-up response message ('CatchUpResponseMessage') is used to signal the end of a
--      response to a catch-up request. This should be sent after any blocks that are sent to the
--      peer as part of the catch-up. If all relevant blocks were sent, the response also includes
--      terminal data ('CatchUpTerminalData') that may include quorum certificates, timeout
--      certificates, quorum messages and timeout messages that are responsive to the catch-up
--      request.
--
-- The catch-up mechanism first catches up nodes with the finalized chain, followed by the
-- non-finalized blocks ordered by height. QCs for the highest certified block and (if different)
-- the certified block that causes the last finalized block to be finalized are included in the
-- terminal data. If the last round timed-out, then a TC for that round is also included.
-- Finally, quorum and timeout messages for the current round that the peer does not already have
-- are also included in the terminal data.
module Concordium.KonsensusV1.Consensus.CatchUp (
    module Concordium.KonsensusV1.Consensus.CatchUp.Types,
    module Concordium.KonsensusV1.Consensus.CatchUp,
)
where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont
import Data.Foldable
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lens.Micro.Platform

import Concordium.Genesis.Data.BaseV1
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import Concordium.KonsensusV1.Consensus.CatchUp.Types
import Concordium.KonsensusV1.Consensus.Finality
import qualified Concordium.KonsensusV1.Consensus.Quorum as Quorum
import qualified Concordium.KonsensusV1.Consensus.Timeout as Timeout
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Types.Parameters

-- |'CatchUpPartialResponse' represents a stream of blocks to send as a response to a catch-up
-- request. The catch up response message should include the catch-up terminal data if all required
-- blocks are sent. As the stream of blocks may be cut off early (e.g. to send max 100 blocks),
-- if the last result is 'CatchUpPartialResponseBlock' then 'cuprFinish' should be called to
-- determine if the terminal data should be included.
data CatchUpPartialResponse m
    = -- |The next block in the stream.
      CatchUpPartialResponseBlock
        { -- |Next block.
          cuprNextBlock :: SignedBlock,
          -- |Continuation for getting any further blocks.
          cuprContinue :: m (CatchUpPartialResponse m),
          -- |Continuation that gets the terminal data in the case where there are no further
          -- blocks.
          cuprFinish :: m (Option CatchUpTerminalData)
        }
    | -- |There are no further blocks, just the terminal data.
      CatchUpPartialResponseDone
        { -- |Terminal data.
          cuprFinished :: CatchUpTerminalData
        }

-- |Construct a 'TimeoutSet' summary from 'TimeoutMessages'.
makeTimeoutSet :: TimeoutMessages -> TimeoutSet
makeTimeoutSet TimeoutMessages{..} =
    TimeoutSet
        { tsFirstEpoch = tmFirstEpoch,
          tsFirstEpochTimeouts = finalizerSet (Map.keys tmFirstEpochTimeouts),
          tsSecondEpochTimeouts = finalizerSet (Map.keys tmSecondEpochTimeouts)
        }

-- |Generate a catch up status for the current state of the consensus.
makeCatchUpStatus :: SkovData pv -> CatchUpStatus
makeCatchUpStatus sd = CatchUpStatus{..}
  where
    lfBlock = sd ^. lastFinalized
    cusLastFinalizedBlock = getHash lfBlock
    cusLastFinalizedRound = blockRound lfBlock
    cusCurrentRound = sd ^. roundStatus . rsCurrentRound
    cusCurrentRoundQuorum =
        view _3
            <$> sd ^. currentQuorumMessages . smBlockToWeightsAndSignatures
    cusCurrentRoundTimeouts = makeTimeoutSet <$> sd ^. currentTimeoutMessages
    getLeavesBranches ::
        Set.Set BlockHash -> -- Hashes of blocks with known children
        [BlockHash] -> -- Accumulated leaf blocks
        [BlockHash] -> -- Accumulated branch blocks
        Seq.Seq [BlockPointer pv] -> -- Unprocessed non-finalized blocks by height
        ( [BlockHash], -- Leaves
          [BlockHash] -- Branches
        )
    getLeavesBranches _ curLeaves curBranches Seq.Empty = (curLeaves, curBranches)
    getLeavesBranches parentBranches curLeaves curBranches (rest Seq.:|> cur) =
        getLeavesBranches newParentBranches newLeaves newBranches rest
      where
        (newLeaves, newBranches, newParentBranches) = foldl' up (curLeaves, curBranches, Set.empty) cur
        up (l, b, p) blk
            | Set.member hsh parentBranches = (l, hsh : b, p')
            | otherwise = (hsh : l, b, p')
          where
            hsh :: BlockHash
            hsh = getHash blk
            p' = case blockBakedData blk of
                Present bd -> Set.insert (blockParent bd) p
                Absent -> p
    (cusLeaves, cusBranches) = getLeavesBranches Set.empty [] [] (sd ^. branches)

-- |Determine if we should catch-up with a peer based on the catch-up status message we have
-- received. We should catch up if the peer's current round or current finalized round is greater
-- than ours. We do not need to catch up if the peer's current round is no later than our last
-- finalized round, since in that case it does not have anything useful for us. Otherwise, we
-- should catch up if any of the following hold:
--
--   * Any of the peer's leaf blocks is pending or unknown. (These blocks could trigger spurious
--     catch-up if they are behind our last finalized block. However, we assume that the peer
--     will mutually catch-up with us, at which point it will no longer report those blocks in
--     its leaves.)
--
--   * The peer's current round is the same as ours, and we are missing any of its quorum
--     messages.
--
--   * The peer's current round is the same as ours, and we are missing any of its timeout messages,
--     except where the timeout messages are for an irrelevant epoch.
--
-- As noted above, it can be the case that catch-up is triggered even when it is not necessary.
-- However, catch-up should always be triggered when it is necessary.
isCatchUpRequired ::
    (LowLevel.MonadTreeStateStore m) =>
    CatchUpStatus ->
    SkovData (MPV m) ->
    m Bool
isCatchUpRequired CatchUpStatus{..} sd
    | cusCurrentRound > myCurrentRound || cusLastFinalizedRound > myLastFinalizedRound =
        return True
    | cusCurrentRound <= myLastFinalizedRound =
        return False
    | otherwise = flip runContT return $ do
        -- Check if we are missing any of the leaf blocks
        forM_ cusLeaves $ \leaf -> do
            leafStatus <- lift $ getRecentBlockStatus leaf sd
            -- If the block is pending or unknown, we should attempt catch-up.
            case leafStatus of
                RecentBlock BlockPendingOrUnknown -> exitRequired
                _ -> return ()
        when (cusCurrentRound == myCurrentRound) $ do
            let myCurrentRoundQuorum = sd ^. currentQuorumMessages . smBlockToWeightsAndSignatures
            -- If they have any quorum messages we don't, we should catch-up.
            forM_ (Map.toList cusCurrentRoundQuorum) $ \(bh, theirFinalizers) ->
                case Map.lookup bh myCurrentRoundQuorum of
                    Just (_, _, myFinalizers)
                        | subsetFinalizerSet theirFinalizers myFinalizers -> return ()
                    _ -> exitRequired
            -- If they have any timeout messages we don't, we should catch-up.
            forM_ cusCurrentRoundTimeouts $ \theirTimeouts -> do
                case sd ^. currentTimeoutMessages of
                    Absent -> exitRequired
                    Present myTimeouts
                        | null (tmSecondEpochTimeouts myTimeouts),
                          tmFirstEpoch myTimeouts == tsFirstEpoch theirTimeouts + 1 -> do
                            exitRequired
                        | tmFirstEpoch myTimeouts == tsFirstEpoch theirTimeouts -> do
                            checkTimeoutSubset
                                (tsFirstEpochTimeouts theirTimeouts)
                                (tmFirstEpochTimeouts myTimeouts)
                            checkTimeoutSubset
                                (tsSecondEpochTimeouts theirTimeouts)
                                (tmSecondEpochTimeouts myTimeouts)
                        | tmFirstEpoch myTimeouts + 1 == tsFirstEpoch theirTimeouts -> do
                            unless
                                (tsSecondEpochTimeouts theirTimeouts == emptyFinalizerSet)
                                exitRequired
                            checkTimeoutSubset
                                (tsFirstEpochTimeouts theirTimeouts)
                                (tmSecondEpochTimeouts myTimeouts)
                        | otherwise -> return ()
                      where
                        checkTimeoutSubset theirs mine =
                            forM_ (finalizerList theirs) $ \fin ->
                                unless (Map.member fin mine) exitRequired
        return False
  where
    myCurrentRound = sd ^. roundStatus . rsCurrentRound
    myLastFinalizedRound = blockRound $ sd ^. lastFinalized
    -- Escape continuation for the case where catch-up is required.
    exitRequired = ContT $ \_ -> return True

-- |Handle a catch-up request.
-- This can safely be called without holding the global state lock and will not update the state.
-- This uses the provided snapshot of the state. The low-level store is only used to read finalized
-- blocks, and once a block is finalized it will remain so in the store. Note that the response
-- will not reflect changes to the state that happen after the state snapshot in the provided
-- 'SkovData'. It is expected that 'makeCatchUpStatus' should be called on the latest state after
-- the blocks provided by 'handleCatchUpRequest' are sent, to ensure that the peer receives the most
-- up-to-date view of the peer's state.
handleCatchUpRequest ::
    ( MonadIO m,
      LowLevel.MonadTreeStateStore m
    ) =>
    CatchUpStatus ->
    SkovData (MPV m) ->
    m (CatchUpPartialResponse m)
handleCatchUpRequest CatchUpStatus{..} skovData = do
    peerLFBStatus <- getBlockStatus cusLastFinalizedBlock skovData
    case peerLFBStatus of
        BlockFinalized peerLFBlock -> catchUpLastFinalized peerLFBlock
        BlockAlive _ -> catchUpAfterLastFinalized True
        _ -> do
            -- We do not consider the peer's last finalized block live or finalized, so we cannot
            -- do anything to catch them up.
            return $ CatchUpPartialResponseDone emptyCatchUpTerminalData
  where
    -- Last finalized block
    lfBlock = skovData ^. lastFinalized
    -- Hash set of the blocks known to the peer (from the peer's last finalized block)
    peerKnownBlocks =
        HashSet.insert
            cusLastFinalizedBlock
            (HashSet.fromList (cusLeaves ++ cusBranches))
    isPeerKnown blk = blk `HashSet.member` peerKnownBlocks
    toSignedBlock (NormalBlock sb) = sb
    toSignedBlock GenesisBlock{} = error "handleCatchUpRequest: unexpected genesis block"
    -- INVARIANT: we consider peerLFBlock to be finalized.
    catchUpLastFinalized peerLFBlock = do
        if blockHeight lfBlock == blockHeight peerLFBlock
            then do
                -- We have the same last finalized block, so no need to catch up finalized blocks.
                catchUpAfterLastFinalized True
            else do
                -- Send the finalized blocks from 1 after the peer's last finalized block,
                -- skipping while the peer claims to have seen them. Note that, since peerLFBlock
                -- is finalized, but is not the last finalized block, then we have:
                -- 0 < blockHeight peerLFBlock + 1 <= blockHeight lfBlock
                sendBlocksFromHeight True (blockHeight peerLFBlock + 1)
    -- Send our finalized blocks from the given height up to the last finalized block, if they
    -- are not in 'peerKnownBlocks'.  The boolean flag 'checkKnown' indicates whether we should
    -- check if the peer already knows the block.  We assume that if the peer does not know a
    -- block, then it does not know any children of the block.
    -- INVARIANT: 0 < hgt <= blockHeight lfBlock
    sendBlocksFromHeight checkKnown hgt
        | hgt == blockHeight lfBlock =
            if checkKnown && isPeerKnown (getHash lfBlock)
                then catchUpAfterLastFinalized True
                else do
                    -- By the invariant, it is not possible for 'lfBlock' to be a genesis block.
                    return
                        CatchUpPartialResponseBlock
                            { cuprNextBlock = toSignedBlock (bpBlock lfBlock),
                              cuprContinue = catchUpAfterLastFinalized False,
                              cuprFinish = return Absent
                            }
        | otherwise =
            LowLevel.lookupBlockByHeight hgt >>= \case
                Nothing -> do
                    -- This should not fail, since the height is below the height of the last
                    -- finalized block.
                    error $ "handleCatchUpRequest: missing finalized block at height " ++ show hgt
                Just fb
                    | checkKnown && isPeerKnown (getHash fb) -> do
                        -- If the peer already knows this block, skip it.
                        sendBlocksFromHeight checkKnown (hgt + 1)
                    | otherwise -> do
                        -- Since 0 < hgt, we can be sure that 'fb' is not a genesis block.
                        return
                            CatchUpPartialResponseBlock
                                { cuprNextBlock = toSignedBlock (LowLevel.stbBlock fb),
                                  cuprContinue = sendBlocksFromHeight False (hgt + 1),
                                  cuprFinish = return Absent
                                }
    catchUpAfterLastFinalized checkKnown = do
        catchUpBranches checkKnown checkKnown [] (skovData ^. branches)
    catchUpBranches _ _ [] Seq.Empty = CatchUpPartialResponseDone <$> endCatchUp
    catchUpBranches _ knownAtCurrentHeight [] (nxt Seq.:<| rest) =
        catchUpBranches knownAtCurrentHeight False nxt rest
    catchUpBranches checkKnown knownAtCurrentHeight (b : bs) rest
        | checkKnown && isPeerKnown (getHash b) = catchUpBranches True True bs rest
        | otherwise =
            return $
                CatchUpPartialResponseBlock
                    { cuprNextBlock = toSignedBlock (bpBlock b),
                      cuprContinue = catchUpBranches checkKnown knownAtCurrentHeight bs rest,
                      cuprFinish =
                        if null bs && null rest
                            then Present <$> endCatchUp
                            else return Absent
                    }
    endCatchUp = do
        let cutdLatestFinalizationEntry
                | cusLastFinalizedRound < skovData ^. lastFinalized . to blockRound =
                    skovData ^. latestFinalizationEntry
                | otherwise = Absent
        let (cutdHighestQuorumCertificate, cutdTimeoutCertificate)
                | cusCurrentRound >= ourCurrentRound =
                    -- The peer's current round is ahead, so there is no need to send the QC or TC.
                    (Absent, Absent)
                | Present timeout <- skovData ^. roundStatus . rsPreviousRoundTimeout,
                  cbEpoch (rtCertifiedBlock timeout) /= qcEpoch highQC =
                    -- In this case, the current round is justified by a TC, but with an earlier
                    -- epoch than the the current highest QC. We therefore send the QC from the
                    -- round timeout instead of the highest QC.
                    ( Present $ cbQuorumCertificate (rtCertifiedBlock timeout),
                      Present (rtTimeoutCertificate timeout)
                    )
                | otherwise =
                    ( Present highQC,
                      rtTimeoutCertificate <$> skovData ^. roundStatus . rsPreviousRoundTimeout
                    )
        cutdCurrentRoundQuorumMessages <- getQuorumMessages
        cutdCurrentRoundTimeoutMessages <- getTimeoutMessages
        return $ CatchUpTerminalData{..}
    highQC = skovData ^. roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate
    ourCurrentRound = skovData ^. roundStatus . rsCurrentRound
    getQuorumMessages
        | cusCurrentRound == ourCurrentRound = do
            let qms = skovData ^.. currentQuorumMessages . smFinalizerToQuorumMessage . traversed
            return $! filter newToPeer qms
        | cusCurrentRound < ourCurrentRound = do
            let qms = skovData ^.. currentQuorumMessages . smFinalizerToQuorumMessage . traversed
            return qms
        | otherwise = do
            return []
      where
        newToPeer qm = case Map.lookup (qmBlock qm) cusCurrentRoundQuorum of
            Nothing -> True
            Just s -> not (memberFinalizerSet (qmFinalizerIndex qm) s)
    getTimeoutMessages
        | cusCurrentRound <= ourCurrentRound =
            case skovData ^. currentTimeoutMessages of
                Absent -> return []
                Present TimeoutMessages{..} -> do
                    let (filterFirst, filterSecond) = case cusCurrentRoundTimeouts of
                            Present TimeoutSet{..}
                                | cusCurrentRound < ourCurrentRound -> (id, id)
                                | tsFirstEpoch + 1 == tmFirstEpoch -> (filter2, id)
                                | tsFirstEpoch == tmFirstEpoch -> (filter1, filter2)
                                | tsFirstEpoch == tmFirstEpoch + 1 -> (id, filter1)
                              where
                                filterFun ts tm =
                                    not $ memberFinalizerSet (tmFinalizerIndex $ tmBody tm) ts
                                filter1 = filter (filterFun tsFirstEpochTimeouts)
                                filter2 = filter (filterFun tsSecondEpochTimeouts)
                            _ -> (id, id)
                    return $
                        filterFirst (Map.elems tmFirstEpochTimeouts)
                            <> filterSecond (Map.elems tmSecondEpochTimeouts)
        | otherwise =
            return []

-- |The result of processing 'CatchUpTerminalData' from a peer.
-- The field 'tdrProgress' indicates if a meaningful update to the state was made as a result of
-- processing the data. This is used to notify peers (by sending a catch-up status message) that
-- we have progressed as a result of information that we have not previously sent them, giving
-- them an opportunity to catch-up.
data TerminalDataResult
    = -- |The data was valid and as expected.
      TerminalDataResultValid
        { -- |'True' if the state was updated.
          tdrProgress :: Bool
        }
    | -- |The data was not valid or would trigger a catch-up.
      TerminalDataResultInvalid
        { -- |'True' if the state was updated.
          tdrProgress :: Bool
        }

-- |Process the contents of a 'CatchUpTerminalData' record. This updates the state and so should
-- be used with the global state lock.
--
-- 1. Process the quorum certificate for the highest certified block, if present. This may
--    result in updating the highest certified block advancing the round. When we advance the
--    round, we do not immediately call 'makeBlock', which is called later.
--
-- 2. Process the finalization entry, if present. This may result in updating the last finalized
--    block, and could advance to a new epoch.
--
-- 3. Process the timeout certificate, if present. If the timeout certificate is relevant, it may
--    advance the round. As above, when we advance the round, we do not immediately call
--    'makeBlock'.
--
-- 4. Process the quorum messages. This may cause a QC to be generated and advance the round.
--    In that case, 'makeBlock' is immediately called as part of the processing.
--
-- 5. Process the timeout messages. This may cause a TC to be generated and advance the round.
--    In that case, 'makeBlock' is immediately called as part of the processing.
--
-- 6. Call 'makeBlock'. This will do nothing if the baker has already baked a block for the round,
--    so it is fine to call even if it was already called.
--
-- Note that processing the QCs and TC can both cause the round to advance. We defer calling
-- 'makeBlock' because if the node produces a block for a round that already has a TC, then it
-- will never get a QC, and so the effort would be wasted.
--
-- If any of the data is invalid or would trigger a catch-up, this returns
-- 'TerminalDataResultInvalid'. If the data is fine (in as much as we inspect it), this returns
-- 'TerminalDataResultValid'. In either case, 'tdrProgress' indicates if processing the data
-- results in an update to the consensus state. If there is progress, then we may have information
-- that is useful to peers, and thus should (queue) sending catch-up status messages to peers.
processCatchUpTerminalData ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      TimerMonad m,
      MonadProtocolVersion m,
      MonadIO m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      MonadThrow m,
      MonadConsensusEvent m,
      MonadBroadcast m,
      MonadLogger m,
      MonadTimeout m,
      IsConsensusV1 (MPV m),
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    CatchUpTerminalData ->
    m TerminalDataResult
processCatchUpTerminalData CatchUpTerminalData{..} = flip runContT return $ do
    -- Processing the quorum certificate before the finalization entry is better in the common
    -- case where the QC is for the successor block in the finalization entry. This is because
    -- it will result in a single transaction on the treestate database, rather than separate
    -- transactions for finalizing and recording the certified block.
    progress0 <- foldM processQC False cutdHighestQuorumCertificate
    progress1 <- processFE progress0 cutdLatestFinalizationEntry
    progress2 <- processTC progress1 cutdTimeoutCertificate
    progress3 <- foldM processQM progress2 cutdCurrentRoundQuorumMessages
    progress4 <- foldM processTM progress3 cutdCurrentRoundTimeoutMessages
    lift makeBlock
    return $ TerminalDataResultValid progress4
  where
    escape progress reason = ContT $ \_ -> do
        logEvent Skov LLDebug $ "Rejecting catch-up response: " ++ reason
        return (TerminalDataResultInvalid progress)
    processFE currentProgress Absent = return currentProgress
    processFE currentProgress (Present latestFinEntry) = do
        let finQC = feFinalizedQuorumCertificate latestFinEntry
        let finBlockHash = qcBlock finQC
        gets (getLiveBlock finBlockHash) >>= \case
            Just block -> do
                unless
                    (blockRound block == qcRound finQC && blockEpoch block == qcEpoch finQC)
                    ( escape
                        currentProgress
                        "finalization entry is inconsistent with the block it finalizes."
                    )
                gets (getBakersForEpoch (qcEpoch finQC)) >>= \case
                    Nothing -> return currentProgress
                    Just BakersAndFinalizers{..} -> do
                        GenesisMetadata{..} <- use genesisMetadata
                        let finEntryOK =
                                checkFinalizationEntry
                                    gmCurrentGenesisHash
                                    (toRational $ genesisSignatureThreshold gmParameters)
                                    _bfFinalizers
                                    latestFinEntry
                        unless finEntryOK (escape currentProgress "finalization entry is invalid.")
                        lift $ do
                            -- Record the checked quorum certificates
                            recordCheckedQuorumCertificate $
                                feFinalizedQuorumCertificate latestFinEntry
                            recordCheckedQuorumCertificate $
                                feSuccessorQuorumCertificate latestFinEntry
                            -- Process the finalization
                            processFinalizationEntry block latestFinEntry
                            return True
            Nothing -> return currentProgress

    processQC currentProgress qc = do
        -- The QC is only relevant if it is for a live block.
        gets (getLiveBlock (qcBlock qc)) >>= \case
            Just block -> do
                unless
                    (blockRound block == qcRound qc && blockEpoch block == qcEpoch qc)
                    ( escape
                        currentProgress
                        "quorum certificate is inconsistent with the block it certifies."
                    )
                -- We only process the quorum certificate (here) if it would advance our highest
                -- certified block.
                highestCBRound <- use $ roundStatus . rsHighestCertifiedBlock . to cbRound
                if highestCBRound < qcRound qc
                    then do
                        gets (getBakersForEpoch (qcEpoch qc)) >>= \case
                            Nothing -> return currentProgress
                            Just BakersAndFinalizers{..} -> do
                                GenesisMetadata{..} <- use genesisMetadata
                                let qcOK =
                                        checkQuorumCertificate
                                            gmCurrentGenesisHash
                                            (toRational $ genesisSignatureThreshold gmParameters)
                                            _bfFinalizers
                                            qc
                                unless
                                    qcOK
                                    (escape currentProgress "quorum certificate is invalid.")
                                lift $ do
                                    recordCheckedQuorumCertificate qc
                                    let newCertifiedBlock =
                                            CertifiedBlock
                                                { cbQuorumCertificate = qc,
                                                  cbQuorumBlock = block
                                                }
                                    -- Process the certified block, checking for finalization.
                                    processCertifiedBlock newCertifiedBlock
                                    curRound <- use (roundStatus . rsCurrentRound)
                                    if curRound <= qcRound qc
                                        then advanceRoundWithQuorum newCertifiedBlock
                                        else
                                            roundStatus . rsHighestCertifiedBlock
                                                .= newCertifiedBlock
                                    return True
                    else return currentProgress
            Nothing -> return currentProgress

    -- This function determines the certified block and bakers to use to verify the timeout
    -- certificate. Typically, this is the highest certified block. However, it can be that the
    -- highest certified block is in a later epoch from that in which the timeout certificate was
    -- generated. In that case, the timeout certificate might not be valid when considered with
    -- respect to that epoch (because of changes in the finalization committee). If this is the
    -- case, then @cutdHighestQuorumCertificate@ should be present and for a block in the
    -- appropriate epoch.
    -- It is important that @processQC@ is called on @cutdHighestQuorumCertificate@ before this
    -- is called, as we rely on the highest certified block to have already been updated if
    -- necessary.
    certifiedBlockAndBakersForTC currentProgress = do
        highCB <- use (roundStatus . rsHighestCertifiedBlock)
        case cutdHighestQuorumCertificate of
            Present qc | qcEpoch qc /= cbEpoch highCB -> do
                -- In this case, the quorum certificate sent by the peer is for a different
                -- epoch than our highest certified block. This implies that we should use
                -- that epoch when evaluating the timeout certificate. (In particular, the
                -- finalization committee can be different from epoch to epoch.)
                gets (getLiveOrLastFinalizedBlock (qcBlock qc)) >>= \case
                    Nothing -> do
                        -- The timeout is for a round that is ahead of our current round,
                        -- so the QC should presumably for a live block (or the last finalized
                        -- block) since otherwise we have not been caught up with the peer.
                        escape
                            currentProgress
                            "quorum certificate is not coherent with timeout certificate \
                            \as the certified block is not live/last finalized"
                    Just block -> do
                        -- We now verify the QC
                        unless
                            (blockRound block == qcRound qc && blockEpoch block == qcEpoch qc)
                            ( escape
                                currentProgress
                                "quorum certificate is inconsistent with the block it certifies."
                            )
                        gets (getBakersForEpoch (qcEpoch qc)) >>= \case
                            Nothing -> do
                                -- If we cannot get the bakers, then the QC is for an epoch that is
                                -- not within 1 of the last finalized block. This should not even
                                -- be possible given that we have determined the block to be live
                                -- or last finalized.
                                escape currentProgress "quorum certificate is for an invalid epoch"
                            Just ebQC@BakersAndFinalizers{..} -> do
                                GenesisMetadata{..} <- use genesisMetadata
                                let qcOK =
                                        checkQuorumCertificate
                                            gmCurrentGenesisHash
                                            (toRational $ genesisSignatureThreshold gmParameters)
                                            _bfFinalizers
                                            qc
                                unless
                                    qcOK
                                    (escape currentProgress "quorum certificate is invalid.")
                                lift $ do
                                    recordCheckedQuorumCertificate qc
                                    let newCertifiedBlock =
                                            CertifiedBlock
                                                { cbQuorumCertificate = qc,
                                                  cbQuorumBlock = block
                                                }
                                    -- Process the certified block, checking for finalization.
                                    processCertifiedBlock newCertifiedBlock
                                    -- We do not need to advance the round or update the highest
                                    -- certified block. If this was necessary, it would have been
                                    -- done by @processQC@, and it would have been that
                                    -- @qcEpoch qc == cbEpoch highCB@.
                                    return (newCertifiedBlock, ebQC)
            _ -> do
                -- The SkovData invariants imply that we can always get the bakers for the
                -- epoch of the highest certified block.
                ebQC <-
                    gets $
                        fromMaybe
                            ( error
                                "processCatchUpTerminalData: bakers are not available for \
                                \highest certified block"
                            )
                            . getBakersForEpoch (cbEpoch highCB)
                return (highCB, ebQC)
    processTC currentProgress Absent = return currentProgress
    processTC currentProgress (Present tc) = do
        curRound <- use (roundStatus . rsCurrentRound)
        -- We only process the timeout certificate if it would advance the current round.
        if curRound <= tcRound tc
            then do
                GenesisMetadata{..} <- use genesisMetadata
                (correspondingCB, ebQC) <- certifiedBlockAndBakersForTC currentProgress
                -- If the timeout certificate is not consistent with our current highest certified
                -- block, then the peer has failed to catch us up with the relevant block and/or QC.
                when
                    ( cbRound correspondingCB < tcMaxRound tc
                        || cbEpoch correspondingCB < tcMaxEpoch tc
                    )
                    ( escape
                        currentProgress
                        "timeout certificate is not consistent with highest certified block"
                    )
                let checkTCValid eb1 eb2 =
                        checkTimeoutCertificate
                            gmCurrentGenesisHash
                            (toRational $ genesisSignatureThreshold gmParameters)
                            (eb1 ^. bfFinalizers)
                            (eb2 ^. bfFinalizers)
                            (ebQC ^. bfFinalizers)
                            tc
                eBkrs <- use epochBakers
                lastFinEpoch <- use $ lastFinalized . to blockEpoch
                let tcOK
                        | tcMinEpoch tc == lastFinEpoch - 1 =
                            checkTCValid
                                (eBkrs ^. previousEpochBakers)
                                (eBkrs ^. currentEpochBakers)
                        | tcMinEpoch tc == lastFinEpoch =
                            checkTCValid
                                (eBkrs ^. currentEpochBakers)
                                (eBkrs ^. nextEpochBakers)
                        | tcMinEpoch tc == lastFinEpoch + 1,
                          tcIsSingleEpoch tc =
                            checkTCValid
                                (eBkrs ^. nextEpochBakers)
                                (eBkrs ^. nextEpochBakers)
                        | otherwise = False
                if tcOK
                    then do
                        lift $
                            advanceRoundWithTimeout
                                RoundTimeout
                                    { rtTimeoutCertificate = tc,
                                      rtCertifiedBlock = correspondingCB
                                    }
                        return True
                    else do
                        escape currentProgress "timeout certificate is invalid"
            else do
                return currentProgress
    processQM currentProgress qm = do
        verRes <- lift $ Quorum.receiveQuorumMessage qm =<< get
        let process vqm = do
                lift $ Quorum.processQuorumMessage vqm (return ())
                return True
        case verRes of
            Quorum.Received vqm -> process vqm
            Quorum.ReceivedNoRelay vqm -> process vqm
            Quorum.Rejected Quorum.Duplicate -> return currentProgress
            Quorum.Rejected Quorum.ObsoleteRound -> return currentProgress
            Quorum.Rejected reason ->
                escape currentProgress $
                    "quorum message was rejected with reason " ++ show reason
            Quorum.CatchupRequired ->
                escape currentProgress "quorum message would trigger catch up"
            Quorum.ConsensusShutdown -> return currentProgress
    processTM currentProgress tm = do
        verRes <- lift $ Timeout.receiveTimeoutMessage tm =<< get
        case verRes of
            Timeout.Received vtm -> do
                lift (Timeout.executeTimeoutMessage vtm) >>= \case
                    Timeout.ExecutionSuccess -> return True
                    reason ->
                        escape currentProgress $
                            "timeout message was invalid with reason " ++ show reason
            Timeout.Rejected Timeout.Duplicate -> return currentProgress
            Timeout.Rejected Timeout.ObsoleteRound -> return currentProgress
            Timeout.Rejected Timeout.DoubleSigning -> return currentProgress
            Timeout.Rejected reason ->
                escape currentProgress $
                    "timeout message was rejected with reason " ++ show reason
            Timeout.CatchupRequired ->
                escape
                    currentProgress
                    "timeout message would trigger catch up"
            Timeout.ConsensusShutdown -> return currentProgress

makeCatchUpStatusMessage :: SkovData pv -> CatchUpMessage
makeCatchUpStatusMessage = CatchUpStatusMessage . (\s -> s{cusBranches = []}) . makeCatchUpStatus

makeCatchUpRequestMessage :: SkovData pv -> CatchUpMessage
makeCatchUpRequestMessage = CatchUpRequestMessage . makeCatchUpStatus
