{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.KonsensusV1.Consensus.CatchUp where

import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import Data.Serialize (runPut)
import Data.Word
import Lens.Micro.Platform

import Concordium.Types.HashableTo

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types

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

data CatchUpPartialResponse m
    = CatchUpPartialResponseBlock
        { cuprNextBlock :: SignedBlock,
          cuprContinue :: m (CatchUpPartialResponse m),
          cuprFinish :: m (Maybe CatchUpTerminalData)
        }
    | CatchUpPartialResponseDone
        { cuprFinished :: Maybe CatchUpTerminalData
        }

data CatchUpStatus = CatchUpStatus
    { cusLastFinalizedBlock :: BlockHash,
      cusLastFinalizedRound :: Round,
      cusLeaves :: [BlockHash],
      cusBranches :: [BlockHash],
      cusCurrentRound :: Round,
      cusCurrentRoundQuorum :: Map.Map BlockHash FinalizerSet
    }

handleCatchUpRequest :: (MonadState (SkovData (MPV m)) m, MonadIO m, LowLevel.MonadTreeStateStore m) => CatchUpStatus -> m (CatchUpPartialResponse m)
handleCatchUpRequest CatchUpStatus{..} = do
    peerLFBStatus <- getBlockStatus cusLastFinalizedBlock =<< get
    case peerLFBStatus of
        BlockFinalized peerLFBlock -> catchUpLastFinalized peerLFBlock
        BlockAlive _ -> catchUpAfterLastFinalized True
        _ -> do
            -- We do not consider the peer's last finalized block live or finalized, so we cannot
            -- do anything to catch them up.
            return $ CatchUpPartialResponseDone Nothing
  where
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
        lfBlock <- use lastFinalized
        if blockHeight lfBlock == blockHeight peerLFBlock
            then do
                -- We have the same last finalized block, so no need to catch up finalized blocks.
                catchUpAfterLastFinalized True
            else do
                -- Send the finalized blocks from 1 after the peer's last finalized block,
                -- skipping while the peer claims to have seen them. Note that, since peerLFBlock
                -- is finalized, but is not the last finalized block, then we have:
                -- 0 < blockHeight peerLFBlock + 1 <= blockHeight lfBlock
                sendBlocksFromHeight True lfBlock (blockHeight peerLFBlock + 1)
    -- Send our finalized blocks from the given height up to the last finalized block, if they
    -- are not in 'peerKnownBlocks'.  The boolean flag 'checkKnown' indicates whether we should
    -- check if the peer already knows the block.  We assume that if the peer does not know a
    -- block, then it does not know any children of the block.
    -- INVARIANT: 0 < hgt <= blockHeight lfBlock
    sendBlocksFromHeight checkKnown lfBlock hgt
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
                        sendBlocksFromHeight checkKnown lfBlock (hgt + 1)
                    | otherwise -> do
                        -- Since 0 < hgt, we can be sure that 'fb' is not a genesis block.
                        return
                            CatchUpPartialResponseBlock
                                { cuprNextBlock = toSignedBlock (LowLevel.stbBlock fb),
                                  cuprContinue = sendBlocksFromHeight False lfBlock (hgt + 1),
                                  cuprFinish = return Nothing
                                }
    catchUpAfterLastFinalized checkKnown = do
        catchUpBranches checkKnown checkKnown [] =<< use branches
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
                      cuprFinish = if null bs && null rest then endCatchUp else return Nothing
                    }
    endCatchUp = do
        cutdQuorumCertificates <- getQCs
        rs <- use roundStatus
        -- We include the timeout certificate if we have one and our current round is higher than
        -- the peer's current round.
        let cutdTimeoutCertificate
                | cusCurrentRound < _rsCurrentRound rs =
                    rtTimeoutCertificate <$> _rsPreviousRoundTimeout rs
                | otherwise = Absent
        cutdCurrentRoundQuorumMessages <- getQuorumMessages
        cutdCurrentRoundTimeoutMessages <- getTimeoutMessages
        return $
            Just $
                CatchUpTerminalData{..}
    getQCs = do
        highQC <- use (roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate)
        if qcRound highQC < cusLastFinalizedRound
            then do
                return []
            else do
                lfBlock <- use lastFinalized
                if qcRound highQC == blockRound lfBlock || blockRound lfBlock <= cusLastFinalizedRound
                    then do
                        -- The high QC and the QC finalizing the LFB are the same, or the peer
                        -- already considers our last finalized block to be finalized.
                        return [highQC]
                    else do
                        LowLevel.lookupLatestFinalizationEntry <&> \case
                            Nothing -> [highQC]
                            Just finEntry -> [feSuccessorQuorumCertificate finEntry, highQC]
    getQuorumMessages = do
        -- TODO: Filter these.
        gets $ toListOf $ currentQuorumMessages . smFinalizerToQuorumMessage . traversed
    getTimeoutMessages = do
        -- TODO: Filter these.
        use currentTimeoutMessages >>= \case
            Absent -> return []
            Present tms -> do
                return $
                    Map.elems (tmFirstEpochTimeouts tms)
                        <> Map.elems (tmSecondEpochTimeouts tms)

{-
data CatchupMessage
    = -- |The 'CatchupStatus' is send when a peer
      -- joins the network and wants to get caught up
      -- or after a pending block has become alive in order
      -- to notify it peers that they potentially can initiate
      -- catch-up with this peer.
      CatchupStatus
        { -- |The current round
          csCurrentRound :: !Round,
          -- |The current epoch
          csCurrentEpoch :: !Epoch,
          -- |The last finalized block recorded by
          -- the sender of this message.
          csLastFinalizedBlock :: !BlockHash,
          -- |The tips of the branches of the node sending this status
          -- message.
          csBranchesTips :: [BlockHash]
        }
    | -- |A catch-up response based on a 'CatchupStatus' message.
      CatchupResponse
        { -- |Current round of the node sending this
          crCurrentRound :: !Round
        }
    deriving (Eq, Show)

-- |Create a 'CatchupStatus' based on the
-- tree state provided.
--
-- This function should be used when either
-- * Catchup is triggered due to a message that the consensus
--   instance cannot process before it has advanced.
--
-- * When a 'PendingBlock' becomes live, thus
--   allowing for a peer to catch up.
--
-- Note that this function does not modify the state.
getCatchupRequest :: (MonadState (SkovData pv) m) => m CatchupMessage
getCatchupRequest = do
    csCurrentRound <- use $ roundStatus . rsCurrentRound
    csCurrentEpoch <- use $ roundStatus . rsCurrentEpoch
    csLastFinalizedBlock <- getHash <$> use lastFinalized
    tips <- toList <$> use branches
    -- Note that we rely here on the fact that a branch is
    -- always non empty.
    let csBranchesTips = map (getHash . head) tips
    return CatchupStatus{..}

-- |Handle a 'CatchupMessage' based on the
-- tree state provided.
-- In particular this function creates a response.
--
-- If the peer sending the request above our progress,
-- then we send a 'CatchupRequest' back.
--
-- Note that this function does not modify the state.
handleCatchupMessage ::
    ( MonadIO m,
      MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    -- |The message to respond to.
    CatchupMessage ->
    -- |The maximum number of items to send.
    Word64 ->
    -- |The resulting 'CatchupResponse' and
    -- the items that should be sent back to the sender
    -- of the 'CatchupMessage'.
    -- Note that in case we're still not caught up after concluding
    -- a catch-up with a peer this function will create a new 'CatchupRequest' and
    -- return an empty list of serialized items.
    --
    -- This function returns 'Nothing' if there's nothing to do
    -- i.e. we and the peer are caught up.
    m (Maybe (CatchupMessage, [ByteString]))
handleCatchupMessage peerStatus limit = do
    ourCurrentRound <- use $ roundStatus . rsCurrentRound
    case peerStatus of
        -- The response is as a result of us catching up
        -- with the sender of this message.
        CatchupResponse peerEndRound
            -- We're caught up with the peer.
            | ourCurrentRound >= peerEndRound -> return Nothing
            -- We need to continue catchup, so we create
            -- a new catch-up request.
            | otherwise -> do
                newRequest <- getCatchupRequest
                return $ Just (newRequest, [])
        -- We're either receiving this because the peer wants to
        -- initiate a catch up with us or the peer is notifying
        -- that it has progressed a round, thus letting us inititate
        -- catch up with that peer.
        CatchupStatus{..}
            -- We're behind the peer sending us this status.
            -- So we initiate catch up.
            | ourCurrentRound < csCurrentRound -> fmap Just (,[]) <$> getCatchupRequest
            -- We're in the same round, so we cannot help the peer advance,
            -- and the peer cannot help us advance.
            | ourCurrentRound == csCurrentRound -> return Nothing
            -- We can help the peer get caught up.
            | otherwise -> do
                LowLevel.lookupBlock csLastFinalizedBlock >>= \case
                    -- The peer sent us an invalid finalized block,
                    -- and there is nothing to do about it.
                    Nothing -> return Nothing
                    -- We have the peers last finalized block.
                    -- Now we take up to @limit@ blocks and send it back
                    -- to its peer.
                    Just LowLevel.StoredBlock{..} -> do
                        sd <- get
                        blocks <- normalBlocksSerialized . take (fromIntegral limit) . concat <$> mapM (flip getBlocksAtHeight sd) [startHeight .. endHeight]
                        -- FIXME: add quorum/timeout certificates extending the tip of any of the branches.
                        -- FIXME: Use branches/live blocks if peer can make use of them
                        -- FIXME: Use CatchUpTerminalData if peer can make use of it.
                        return $ Just (CatchupResponse ourCurrentRound, blocks)
                      where
                        startHeight = 1 + bmHeight stbInfo
                        endHeight = startHeight + BlockHeight limit
                        -- serialize the blocks excluding if the block pointer is pointing to a genesis block.
                        normalBlocksSerialized :: [BlockPointer pv] -> [ByteString]
                        normalBlocksSerialized =
                            mapMaybe
                                ( \bp -> case bpBlock bp of
                                    (GenesisBlock _) -> Nothing
                                    NormalBlock sb -> Just $ (runPut . putSignedBlock) sb
                                )
-}
