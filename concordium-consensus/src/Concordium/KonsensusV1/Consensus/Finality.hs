module Concordium.KonsensusV1.Consensus.Finality where

import Control.Monad.State

import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.Statistics
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimeMonad
import Concordium.Utils

-- |Check if a valid quorum certificate finalizes a block. If so, the block and its ancestors
-- are finalized, the tree is pruned to the decendants of the new last finalized block, and,
-- if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid and for a live (non-finalized) block. (If the
-- block is not live, this function will not make any change to the state, and will not error.)
checkFinality :: (MonadState (SkovData (MPV m)) m) => QuorumCertificate -> m ()
checkFinality qc = do
    sd <- get
    case getMemoryBlockStatus (qcBlock qc) sd of
        Just (BlockAlive block) -> checkFinalityWithBlock qc block
        _ -> return ()

-- |Check if a valid quorum certificate finalizes a block. If so, the block and its ancestors
-- are finalized, the tree is pruned to the decendants of the new last finalized block, and,
-- if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid and for the supplied block, which is live
-- (and not finalized).
checkFinalityWithBlock :: (MonadState (SkovData (MPV m)) m) => QuorumCertificate -> BlockPointer (MPV m) -> m ()
checkFinalityWithBlock qc blockPtr
    | NormalBlock block <- bpBlock blockPtr,
      let parentQC = blockQuorumCertificate block,
      qcRound qc == qcRound parentQC + 1,
      qcEpoch qc == qcEpoch parentQC = do
        let newFinalizationEntry =
                FinalizationEntry
                    { feFinalizedQuorumCertificate = parentQC,
                      feSuccessorQuorumCertificate = qc,
                      feSuccessorProof = getHash (sbBlock block)
                    }
        undefined
    | otherwise = return ()

processFinalization ::
    (MonadState (SkovData (MPV m)) m, TimeMonad m) =>
    BlockPointer (MPV m) ->
    FinalizationEntry ->
    m ()
processFinalization newFinalizedBlock newFinalizationEntry = do
    -- Update the finalization statistics
    now <- currentTime
    statistics %=! updateStatsOnFinalize now
