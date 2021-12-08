{-# LANGUAGE
    ScopedTypeVariables #-}
module Concordium.Kontrol.BestBlock(
    bestBlock,
    bestBlockBefore,
    bestBlockOf
) where

import Data.Foldable
import qualified Data.Sequence as Seq
import Control.Exception (assert)

import Concordium.Types
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads

import Concordium.Skov.Monad
import Concordium.Birk.LeaderElection
import Concordium.GlobalState.TreeState(Branches, BlockPointerType)
import Concordium.Kontrol

blockLuck :: (SkovQueryMonad m) => BlockPointerType m -> m BlockLuck
blockLuck block = case blockFields block of
        Nothing -> return genesisLuck -- Genesis block has luck 1 by definition
        Just bf -> do
            -- get Birk parameters of the __parent__ block, at the slot of the new block.
            -- These are the parameters which determine valid bakers, election difficulty,
            -- that determine the luck of the block itself.
            parent <- bpParent block
            parentState <- blockState parent
            bakers <- getSlotBakers parentState (blockSlot block)
            let baker = lotteryBaker bakers (blockBaker bf)
            ts <- getSlotTimestamp (blockSlot block)
            elDiff <- getElectionDifficulty parentState ts
            case baker of
                Nothing -> assert False $ return zeroLuck -- This should not happen, since it would mean the block was baked by an invalid baker
                Just (_, lotteryPower) ->
                    return $! electionLuck elDiff lotteryPower (blockProof bf)

compareBlocks :: (SkovQueryMonad m)
              => BlockPointerType m
              -> (BlockPointerType m, Maybe BlockLuck)
              -> m (BlockPointerType m, Maybe BlockLuck)
compareBlocks contender best@(bestb, mbestLuck) =
    case compare (blockSlot bestb) (blockSlot contender) of
        LT -> return best -- if bestb has the smaller slot it is to be prefered 
        GT -> return (contender, Nothing)
        EQ -> do
            luck <- blockLuck contender
            bestLuck <- case mbestLuck of
                Just l -> return l
                Nothing -> blockLuck bestb
            return $ if (bestLuck, bpHash bestb) < (luck, bpHash contender) then (contender, Just luck) else (bestb, Just bestLuck)

bestBlockBranches :: forall m. (SkovQueryMonad m) => [[BlockPointerType m]] -> m (BlockPointerType m)
bestBlockBranches [] = lastFinalizedBlock
bestBlockBranches l = bb l
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) =
            case blocks of
                [] -> bb branches
                (b : bs) -> fst <$> foldrM compareBlocks (b, Nothing) bs


-- |Get the best block currently in the tree.
bestBlock :: forall m. (SkovQueryMonad m) => m (BlockPointerType m)
bestBlock = bestBlockBranches =<< branchesFromTop

-- |Get the best non-finalized block in the tree with a slot time strictly below the given bound.
-- If there is no such block, the last finalized block is returned.
bestBlockBefore :: forall m. (SkovQueryMonad m) => Slot -> m (BlockPointerType m)
bestBlockBefore slotBound = bestBlockBranches . fmap (filter (\b -> blockSlot b < slotBound)) =<< branchesFromTop

-- |Given some 'Branches', determine the best block.
-- This will always be a block at the greatest height that is non-empty.
bestBlockOf :: (SkovQueryMonad m) => Branches m -> m (Maybe (BlockPointerType m))
bestBlockOf Seq.Empty = return Nothing
bestBlockOf (bs' Seq.:|> tbs) = case tbs of
        [] -> bestBlockOf bs'
        [b] -> return $ Just b
        (b : tbs') -> do
            bb <- fst <$> foldrM compareBlocks (b, Nothing) tbs'
            return $ Just bb
