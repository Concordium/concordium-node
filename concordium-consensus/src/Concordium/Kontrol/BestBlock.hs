{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables #-}
module Concordium.Kontrol.BestBlock(
    bestBlock,
    bestBlockBefore
) where

import Data.Foldable
import Lens.Micro.Platform
import Control.Exception (assert)

import Concordium.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.Parameters
import Concordium.Skov.Monad
import Concordium.Birk.LeaderElection
import Concordium.GlobalState.BlockState(BlockPointer, BlockPointerData(..), bpParent)

blockLuck :: (SkovQueryMonad m) => BlockPointer m -> m BlockLuck
blockLuck block = case blockFields block of
        Nothing -> return genesisLuck -- Genesis block has luck 1 by definition
        Just bf -> do
            -- get Birk parameters of the __parent__ block, at the slot of the new block. 
            -- These are the parameters which determine valid bakers, election difficulty,
            -- that determine the luck of the block itself.
            params <- getBirkParameters (blockSlot (bpBlock block)) (bpParent block)
            case birkBaker (blockBaker bf) params of
                Nothing -> assert False $ return zeroLuck -- This should not happen, since it would mean the block was baked by an invalid baker
                Just (_, lotteryPower) ->
                    return (electionLuck (params ^. birkElectionDifficulty) lotteryPower (blockProof bf))

compareBlocks :: (SkovQueryMonad m) => BlockPointer m -> (BlockPointer m, Maybe BlockLuck) -> m (BlockPointer m, Maybe BlockLuck)
compareBlocks contender best@(bestb, mbestLuck) =
    case compare (blockSlot (bpBlock bestb)) (blockSlot (bpBlock contender)) of
        LT -> return (contender, Nothing)
        GT -> return best
        EQ -> do
            luck <- blockLuck contender
            bestLuck <- case mbestLuck of
                Just l -> return l
                Nothing -> blockLuck bestb
            return $ if (bestLuck, bpHash bestb) < (luck, bpHash contender) then (contender, Just luck) else (bestb, Just bestLuck)

bestBlockBranches :: forall m. (SkovQueryMonad m) => [[BlockPointer m]] -> m (BlockPointer m)
bestBlockBranches [] = lastFinalizedBlock
bestBlockBranches l = bb l
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) = do
            case blocks of
                [] -> bb branches
                (b : bs) -> fst <$> foldrM compareBlocks (b, Nothing) bs


-- |Get the best block currently in the tree.
bestBlock :: forall m. (SkovQueryMonad m) => m (BlockPointer m)
bestBlock = bestBlockBranches =<< branchesFromTop
    
-- |Get the best non-finalized block in the tree with a slot time strictly below the given bound.
-- If there is no such block, the last finalized block is returned.
bestBlockBefore :: forall m. (SkovQueryMonad m) => Slot -> m (BlockPointer m)
bestBlockBefore slotBound = bestBlockBranches . fmap (filter (\b -> blockSlot (bpBlock b) < slotBound)) =<< branchesFromTop
