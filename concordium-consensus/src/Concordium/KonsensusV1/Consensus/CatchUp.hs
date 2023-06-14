{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.CatchUp where

import Control.Monad.State
import Control.Monad.Trans.Cont
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
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
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Set as Set

-- notes

-- We need to catch up blocks,
-- quorum certificates/messages and timeout certificates/messages
--
-- In particular if receiving a quorum/timeout message triggers catch up then
-- these messages will need to be part of the catch up.
--
-- (In an aggregated fashion qcs, tcs, qms, tms)
--
-- Catchup responses must bypass the deduplication layer,
-- hence if we are catching up from a peer then their responses should
-- not be deduplicated.

{-
A node N is up to date with a peer P if:
    * N considers P's last finalized block to be finalized.
    * The round of N's highest certified block is at least the round of P's highest certified block.
    * N's current round (round(N)) is at least P's current round.
    * N's current epoch is at least P's current epoch.
    * N has all timeout signatures on round(N) that P has, and considers them valid if they are.
    * N has all quorum signatures on round(N) that P has, and considers them valid if they are.
    * If P has only one block in round(N), then N also has that block.

If P is honest and considers a timeout signature on round(N) valid, then
-}

-- |The 'CatchUpTerminalData' is sent as part of a catch-up response that concludes catch-up with
-- the peer (i.e. the peer has sent all relevant information).
--
-- If the peer is not otherwise aware of them, 'cutdQuorumCertificates' should include QCs on:
--    * The block in the round after the last finalized block (if the peer does not consider it
--      finalized already).
--    * The highest certified block (if for a later round than the peer's highest certified block).
data CatchUpTerminalData = CatchUpTerminalData
    { -- |Quorum certificates to ensure agreement on the last finalized block and highest certified
      -- block.
      cutdQuorumCertificates :: ![QuorumCertificate],
      -- |A timeout certificate for the last round, if available.
      cutdTimeoutCertificate :: !(Option TimeoutCertificate),
      -- |Valid quorum messages for the current round.
      -- TODO: Repackage all quorum messages for the same block together.
      cutdCurrentRoundQuorumMessages :: ![QuorumMessage],
      -- |Valid timeout messages for the current round.
      -- TODO: Repackage timeout messages together.
      cutdCurrentRoundTimeoutMessages :: ![TimeoutMessage]
    }
    deriving (Eq, Show)

data CatchUpPartialResponse m
    = CatchUpPartialResponseBlock
        { cuprNextBlock :: SignedBlock,
          cuprContinue :: m (CatchUpPartialResponse m),
          cuprFinish :: m (Maybe CatchUpTerminalData)
        }
    | CatchUpPartialResponseDone
        { cuprFinished :: CatchUpTerminalData
        }

data TimeoutSet = TimeoutSet
    { tsFirstEpoch :: !Epoch,
      tsFirstEpochTimeouts :: !FinalizerSet,
      tsSecondEpochTimeouts :: !FinalizerSet
    }

makeTimeoutSet :: TimeoutMessages -> TimeoutSet
makeTimeoutSet TimeoutMessages{..} =
    TimeoutSet
        { tsFirstEpoch = tmFirstEpoch,
          tsFirstEpochTimeouts = finalizerSet (Map.keys tmFirstEpochTimeouts),
          tsSecondEpochTimeouts = finalizerSet (Map.keys tmSecondEpochTimeouts)
        }

data CatchUpStatus = CatchUpStatus
    { cusLastFinalizedBlock :: BlockHash,
      cusLastFinalizedRound :: Round,
      cusLeaves :: [BlockHash],
      cusBranches :: [BlockHash],
      cusCurrentRound :: Round,
      cusCurrentRoundQuorum :: Map.Map BlockHash FinalizerSet,
      cusCurrentRoundTimeouts :: Option TimeoutSet
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

-- |Handle a catch-up request.
-- This can safely be called without holding the global state lock and will not update the state.
handleCatchUpRequest ::
    ( MonadIO m,
      LowLevel.MonadTreeStateStore m
    ) =>
    CatchUpStatus ->
    SkovData (MPV m) ->
    m (Maybe (CatchUpPartialResponse m))
handleCatchUpRequest CatchUpStatus{..} skovData = do
    peerLFBStatus <- getBlockStatus cusLastFinalizedBlock skovData
    case peerLFBStatus of
        BlockFinalized peerLFBlock -> Just <$> catchUpLastFinalized peerLFBlock
        BlockAlive _ -> Just <$> catchUpAfterLastFinalized True
        _ -> do
            -- We do not consider the peer's last finalized block live or finalized, so we cannot
            -- do anything to catch them up.
            return Nothing
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
                              cuprFinish = return Nothing
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
                                  cuprFinish = return Nothing
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
                            then Just <$> endCatchUp
                            else return Nothing
                    }
    endCatchUp = do
        cutdQuorumCertificates <- getQCs
        let rs = skovData ^. roundStatus
        -- We include the timeout certificate if we have one and our current round is higher than
        -- the peer's current round.
        let cutdTimeoutCertificate
                | cusCurrentRound < _rsCurrentRound rs =
                    rtTimeoutCertificate <$> _rsPreviousRoundTimeout rs
                | otherwise = Absent
        cutdCurrentRoundQuorumMessages <- getQuorumMessages
        cutdCurrentRoundTimeoutMessages <- getTimeoutMessages
        return $ CatchUpTerminalData{..}
    highQC = skovData ^. roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate
    getQCs
        | qcRound highQC < cusLastFinalizedRound = return []
        | qcRound highQC == blockRound lfBlock || blockRound lfBlock <= cusLastFinalizedRound =
            return [highQC]
        | otherwise = do
            LowLevel.lookupLatestFinalizationEntry <&> \case
                Nothing -> [highQC]
                Just finEntry -> [feSuccessorQuorumCertificate finEntry, highQC]
    ourCurrentRound = skovData ^. roundStatus . rsCurrentRound
    getQuorumMessages
        | cusCurrentRound == ourCurrentRound = do
            let qms = skovData ^.. currentQuorumMessages . smFinalizerToQuorumMessage . traversed
            return $! filter newToPeer qms
        | otherwise = do
            return []
      where
        newToPeer qm = case Map.lookup (qmBlock qm) cusCurrentRoundQuorum of
            Nothing -> True
            Just s -> not (memberFinalizerSet (qmFinalizerIndex qm) s)
    getTimeoutMessages
        | cusCurrentRound == ourCurrentRound =
            case skovData ^. currentTimeoutMessages of
                Absent -> return []
                Present TimeoutMessages{..} -> do
                    let (filterFirst, filterSecond) = case cusCurrentRoundTimeouts of
                            Present TimeoutSet{..}
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

data TerminalDataResult
    = TerminalDataResultValid {tdrProgress :: Bool}
    | TerminalDataResultInvalid {tdrProgress :: Bool}

-- |Process the contents of a 'CatchUpTerminalData' record. This updates the state and so should
-- be used with the global state lock.
--
-- 1. Process the quorum certificates. There should be up to two of these. At most one can
--    result in updating the highest certified block and advancing the round. (The other one may
--    update the last finalized block.) When we advance the round, we do not immediately call
--    'makeBlock', which is called later.
--
-- 2. Process the timeout certificate, if present. If the timeout certificate is relevant, it may
--    advance the round. As above, when we advance the round, we do not immediately call
--    'makeBlock'.
--
-- 3. Process the quorum messages. This may cause a QC to be generated and advance the round.
--    In that case, 'makeBlock' is immediately called as part of the processing.
--
-- 4. Process the timeout messages. This may cause a TC to be generated and advance the round.
--    In that case, 'makeBlock' is immediately called as part of the processing.
--
-- 5. Call 'makeBlock'. This will do nothing if the baker has already baked a block for the round,
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
    progress1 <- foldM processQC False cutdQuorumCertificates
    progress2 <- processTC progress1 cutdTimeoutCertificate
    progress3 <- foldM processQM progress2 cutdCurrentRoundQuorumMessages
    progress4 <- foldM processTM progress3 cutdCurrentRoundTimeoutMessages
    lift makeBlock
    return $ TerminalDataResultValid progress4
  where
    escape progress = ContT $ \_ -> return (TerminalDataResultInvalid progress)
    processQC currentProgress qc = do
        -- The QC is only relevant if it is for a live block.
        gets (getLiveBlock (qcBlock qc)) >>= \case
            Just block -> do
                unless
                    (blockRound block == qcRound qc && blockEpoch block == qcEpoch qc)
                    (escape currentProgress)
                use (roundExistingQuorumCertificate (qcRound qc)) >>= \case
                    Just _ -> return currentProgress
                    Nothing -> do
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
                                unless qcOK (escape currentProgress)
                                lift $ do
                                    recordCheckedQuorumCertificate qc
                                    checkFinalityWithBlock qc block
                                    curRound <- use (roundStatus . rsCurrentRound)
                                    when (curRound <= qcRound qc) $
                                        advanceRoundWithQuorum
                                            CertifiedBlock
                                                { cbQuorumCertificate = qc,
                                                  cbQuorumBlock = block
                                                }
                                    return True
            Nothing -> return currentProgress
    processTC currentProgress Absent = return currentProgress
    processTC currentProgress (Present tc) = do
        curRound <- use (roundStatus . rsCurrentRound)
        if curRound <= tcRound tc
            then do
                GenesisMetadata{..} <- use genesisMetadata
                highestCB <- use (roundStatus . rsHighestCertifiedBlock)
                let highBlock = cbQuorumBlock highestCB
                -- If the timeout certificate is not consistent with our current highest certified
                -- block, then the peer has failed to catch us up with the relevant block and/or QC.
                when
                    (blockRound highBlock < tcMaxRound tc || blockEpoch highBlock < tcMaxEpoch tc)
                    (escape currentProgress)
                -- The SkovData invariants imply that we can always get the bakers for the epoch
                -- of the highest certified block.
                ebQC <-
                    gets $
                        fromMaybe
                            ( error
                                "processCatchUpTerminalData: bakers are not available for \
                                \highest certified block"
                            )
                            . getBakersForEpoch (blockEpoch highBlock)
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
                                      rtCertifiedBlock = highestCB
                                    }
                        return True
                    else do
                        escape currentProgress
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
            _ -> escape currentProgress
    processTM currentProgress tm = do
        verRes <- lift $ Timeout.receiveTimeoutMessage tm =<< get
        case verRes of
            Timeout.Received vtm -> do
                lift (Timeout.executeTimeoutMessage vtm) >>= \case
                    Timeout.ExecutionSuccess -> return True
                    _ -> escape currentProgress
            Timeout.Rejected Timeout.Duplicate -> return currentProgress
            Timeout.Rejected Timeout.ObsoleteRound -> return currentProgress
            _ -> escape currentProgress
