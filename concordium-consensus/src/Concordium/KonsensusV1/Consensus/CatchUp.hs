{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.KonsensusV1.Consensus.CatchUp where

import Control.Monad.State
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Serialize (runPut)
import Data.Word
import Lens.Micro.Platform

import Concordium.Types.HashableTo

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Data.ByteString (ByteString)

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
