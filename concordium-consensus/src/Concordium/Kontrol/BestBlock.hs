{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables #-}
module Concordium.Kontrol.BestBlock(
    bestBlock,
    bestBlockBefore
) where

import Data.Foldable

import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Birk.LeaderElection
-- import Concordium.Kontrol.VerifyBlock

blockLuck :: (SkovMonad m) => BlockPointer -> m Double
blockLuck block = do
        gb <- genesisBlock
        if block == gb then
            return 1
        else do
            params <- getBirkParameters (blockSlot (bpBlock block))
            case birkBaker (blockBaker (bpBlock block)) params of
                Nothing -> return 0 -- This should not happen, since it would mean the block was baked by an invalid baker
                Just baker ->
                    return (electionLuck (birkElectionDifficulty params) (bakerLotteryPower baker) (blockProof (bpBlock block)))

compareBlocks :: (SkovMonad m) => BlockPointer -> Maybe (BlockPointer, Maybe Double) -> m (Maybe (BlockPointer, Maybe Double))
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
bestBlock :: forall m. (SkovMonad m) => m BlockPointer
bestBlock = branchesFromTop >>= bb
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) = do
            bBlock <- foldrM compareBlocks Nothing blocks
            case bBlock of
                Nothing -> bb branches
                Just (bp, _) -> return bp

-- |Get the best block in the tree with a slot time strictly below the given bound.
bestBlockBefore :: forall m. (SkovMonad m) => Slot -> m BlockPointer
bestBlockBefore slotBound = branchesFromTop >>= bb
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) = do
            let filteredBlocks = filter (\b -> blockSlot (bpBlock b) < slotBound) blocks
            bBlock <- foldrM compareBlocks Nothing filteredBlocks
            case bBlock of
                Nothing -> bb branches
                Just (bp, _) -> return bp
