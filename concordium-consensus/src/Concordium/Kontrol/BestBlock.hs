{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables #-}
module Concordium.Kontrol.BestBlock where

import Data.Foldable

import Concordium.Types
import Concordium.Kontrol.Monad
import Concordium.Birk.LeaderElection
-- import Concordium.Kontrol.VerifyBlock

blockLuck :: (KontrolMonad m) => BlockPointer -> m Double
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

{-
bestBlock :: forall m. (KontrolMonad m) => m BlockHash
bestBlock = getCurrentHeight >>= bb
    where
        bb h = do
            blocks <- getBlocksAtHeight h
            bestBlock <- foldrM compareBlocks Nothing blocks
            case bestBlock of
                Nothing -> bb (h - 1)
                Just (bh, _) -> return bh
        compareBlocks :: BlockHash -> Maybe (BlockHash, Maybe (Block, Maybe Double)) -> m (Maybe (BlockHash, Maybe (Block, Maybe Double)))
        compareBlocks bh Nothing = do
            valid <- validateBlock bh
            return $ if valid then Just (bh, Nothing) else Nothing
        compareBlocks bh br@(Just (bhr, binfo)) = do
            valid <- validateBlock bh
            if valid then do
                (Just block) <- resolveBlock bh
                (goodBlock, l) <- case binfo of
                    Just j -> return j
                    Nothing -> do
                        Just goodBlock <- resolveBlock bhr
                        return (goodBlock, Nothing)
                case compare (blockSlot goodBlock) (blockSlot block) of
                    LT -> return $ Just (bh, Just (block, Nothing))
                    GT -> return $ Just (bhr, Just (goodBlock, l))
                    EQ -> do
                        luck <- blockLuck bh block
                        goodLuck <- case l of
                            Just goodLuck -> return goodLuck
                            Nothing -> blockLuck bhr goodBlock
                        case compare (goodLuck, bhr) (luck, bh) of
                            LT -> return $ Just (bh, Just (block, Just luck))
                            GT -> return $ Just (bhr, Just (goodBlock, Just goodLuck))
                            EQ -> return $ Just (bh, Just (block, Just luck))
            else
                return br
-}

bestBlockBefore :: forall m. (KontrolMonad m) => Slot -> m BlockPointer
bestBlockBefore slotBound = branchesFromTop >>= bb
    where
        bb [] = lastFinalizedBlock
        bb (blocks : branches) = do
            let filteredBlocks = filter (\b -> blockSlot (bpBlock b) < slotBound) blocks
            bestBlock <- foldrM compareBlocks Nothing filteredBlocks
            case bestBlock of
                Nothing -> bb branches
                Just (bp, _) -> return bp
        compareBlocks :: BlockPointer -> Maybe (BlockPointer, Maybe Double) -> m (Maybe (BlockPointer, Maybe Double))
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
