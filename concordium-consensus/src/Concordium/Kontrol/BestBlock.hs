{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables #-}
module Concordium.Kontrol.BestBlock(
    bestBlock,
    bestBlockBefore
) where

import Data.Foldable

import Concordium.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.Parameters
import Concordium.Skov.Monad
import Concordium.Birk.LeaderElection
import Concordium.GlobalState.TreeState(BlockPointer, BlockPointerData(..))
-- import Concordium.Kontrol.VerifyBlock

blockLuck :: (SkovQueryMonad m) => BlockPointer m -> m Double
blockLuck block = case blockFields block of
        Nothing -> return 1 -- Genesis block has luck 1 by definition
        Just bf -> do
            params <- getBirkParameters (blockSlot block)
            case birkBaker (blockBaker bf) params of
                Nothing -> return 0 -- This should not happen, since it would mean the block was baked by an invalid baker
                Just baker ->
                    return (electionLuck (birkElectionDifficulty params) (bakerLotteryPower baker) (blockProof bf))

compareBlocks :: (SkovQueryMonad m) => BlockPointer m -> Maybe (BlockPointer m, Maybe Double) -> m (Maybe (BlockPointer m, Maybe Double))
compareBlocks bp Nothing = return $ Just (bp, Nothing)
compareBlocks contender best@(Just (bestb, mbestLuck)) =
    case compare (blockSlot (bpBlock bestb)) (blockSlot (bpBlock contender)) of
        LT -> return $ Just (contender, Nothing)
        GT -> return best
        EQ -> do
            luck <- blockLuck contender
            bestLuck <- case mbestLuck of
                Just l -> return l
                Nothing -> blockLuck bestb
            return $ Just $ if (bestLuck, bpHash bestb) < (luck, bpHash contender) then (contender, Just luck) else (bestb, Just bestLuck)

-- |Get the best block currently in the tree.
bestBlock :: forall m. (SkovQueryMonad m) => m (BlockPointer m)
bestBlock = branchesFromTop >>= bb
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) = do
            bBlock <- foldrM compareBlocks Nothing blocks
            case bBlock of
                Nothing -> bb branches
                Just (bp, _) -> return bp

-- |Get the best non-finalized block in the tree with a slot time strictly below the given bound.
-- If there is no such block, the last finalized block is returned.
bestBlockBefore :: forall m. (SkovQueryMonad m) => Slot -> m (BlockPointer m)
bestBlockBefore slotBound = branchesFromTop >>= bb
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) = do
            let filteredBlocks = filter (\b -> blockSlot (bpBlock b) < slotBound) blocks
            bBlock <- foldrM compareBlocks Nothing filteredBlocks
            case bBlock of
                Nothing -> bb branches
                Just (bp, _) -> return bp
