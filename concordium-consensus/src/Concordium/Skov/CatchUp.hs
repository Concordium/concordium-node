{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Concordium.Skov.CatchUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Serialize
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro.Platform
import Control.Monad

import Concordium.Types
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState hiding (getGenesisData)
import Concordium.GlobalState.Parameters

import Concordium.Afgjort.Finalize
import Concordium.Skov.Monad
import Concordium.Kontrol.BestBlock

data CatchUpStatus = CatchUpStatus {
    -- |If this flag is set, the recipient is expected to send any
    -- blocks and finalization records the sender may be missing,
    -- followed by a CatchUpStatus message.
    cusIsRequest :: Bool,
    -- |Hash of the sender's last finalized block
    cusLastFinalizedBlock :: BlockHash,
    -- |Height of the sender's last finalized block
    cusLastFinalizedHeight :: BlockHeight,
    -- |Hash of the sender's best block
    cusBestBlock :: BlockHash,
    -- |List of blocks at the height which justifies a
    -- block as a candidate for the next round of finalization
    cusFinalizationJustifiers :: [BlockHash],
    -- |List of live (non-finalized) blocks. This should
    -- only be included in a request.
    cusAdditionalBlocks :: [BlockHash]
} deriving (Show)
instance Serialize CatchUpStatus where
    put CatchUpStatus{..} = do
        put cusIsRequest
        put cusLastFinalizedBlock
        put cusLastFinalizedHeight
        put cusBestBlock
        put cusFinalizationJustifiers
        when cusIsRequest $ put cusAdditionalBlocks
    get = do
        cusIsRequest <- get
        cusLastFinalizedBlock <- get
        cusLastFinalizedHeight <- get
        cusBestBlock <- get
        cusFinalizationJustifiers <- get
        cusAdditionalBlocks <- if cusIsRequest then get else return []
        return CatchUpStatus{..}

makeCatchUpStatus :: (BlockPointerData b) => Bool -> b -> b -> [b] -> [b] -> CatchUpStatus
makeCatchUpStatus cusIsRequest lfb bb fjs abs = CatchUpStatus{..}
    where
        cusLastFinalizedBlock = bpHash lfb
        cusLastFinalizedHeight = bpHeight lfb
        cusBestBlock = bpHash bb
        cusFinalizationJustifiers = bpHash <$> fjs
        cusAdditionalBlocks = if cusIsRequest then bpHash <$> abs else []

getCatchUpStatus :: (TreeStateMonad m, SkovQueryMonad m) => Bool -> m CatchUpStatus
getCatchUpStatus cusIsRequest = do
        (lfb, lastFinRec) <- getLastFinalized
        finParams <- genesisFinalizationParameters <$> getGenesisData
        let justHeight = nextFinalizationJustifierHeight finParams lastFinRec lfb
        justifiers <- getBlocksAtHeight justHeight
        bb <- bestBlock
        branches <- getBranches
        return $ makeCatchUpStatus cusIsRequest lfb bb justifiers (concat branches)

data KnownBlocks b = KnownBlocks {
    -- |The known blocks indexed by height
    kbHeightMap :: Map.Map BlockHeight (Set.Set b),
    -- |The map should contain all ancestors of all blocks in the map
    -- with height at least 'kbAncestorsHeight'.
    kbAncestorsHeight :: BlockHeight
}

emptyKnownBlocks :: KnownBlocks b
emptyKnownBlocks = KnownBlocks Map.empty 0

addKnownBlock :: (BlockPointerData b, Ord b) => b -> KnownBlocks b -> KnownBlocks b
addKnownBlock b kb@(KnownBlocks m h) = if present then kb else KnownBlocks m' (max h (bpHeight b))
    where
        (present, m') = Map.alterF upd (bpHeight b) m
        upd Nothing = (False, Just $! Set.singleton b)
        upd (Just s) = if b `Set.member` s then (True, Just s) else (False, Just $! Set.insert b s)

makeKnownBlocks :: (BlockPointerData b, Ord b) => [b] -> KnownBlocks b
makeKnownBlocks = foldr addKnownBlock emptyKnownBlocks

updateKnownBlocksToHeight :: (BlockPointerData b, Ord b) => BlockHeight -> KnownBlocks b -> KnownBlocks b
updateKnownBlocksToHeight h kb@(KnownBlocks m hkb)
        | h >= kbAncestorsHeight kb = kb
        | otherwise = updateKnownBlocksToHeight h kb'
    where
        kb' = KnownBlocks m' (hkb - 1)
        genhkb = Map.lookup hkb m
        genhkb' = Set.fromList $ maybe [] (fmap bpParent . Set.toList) genhkb
        m' = m & at (hkb - 1) . non Set.empty %~ Set.union genhkb'

checkKnownBlock :: (BlockPointerData b, Ord b) => b -> KnownBlocks b -> (Bool, KnownBlocks b)
checkKnownBlock b kb = (b `Set.member` (m ^. at (bpHeight b) . non Set.empty), kb')
    where
        kb'@(KnownBlocks m _) = updateKnownBlocksToHeight (bpHeight b) kb


handleCatchUp :: (TreeStateMonad m, SkovQueryMonad m) => CatchUpStatus -> m (Either String (Maybe ([Either FinalizationRecord (BlockPointer m)], CatchUpStatus), Bool))
handleCatchUp peerCUS = runExceptT $ do
        (lfb, lastFinRec) <- lift $ getLastFinalized
        if cusLastFinalizedHeight peerCUS > bpHeight lfb then do
            response <-
                if cusIsRequest peerCUS then do
                    myCUS <- lift $ getCatchUpStatus False
                    return $ Just ([], myCUS)
                else
                    return Nothing
            -- We are behind, so we mark the peer as pending.
            return (response, True)
        else do
            (peerlfb, peerFinRec) <- getBlockStatus (cusLastFinalizedBlock peerCUS) >>= \case
                Just (BlockFinalized peerlfb peerFinRec) -> do
                    return (peerlfb, peerFinRec)
                _ -> do
                    throwE $ "Invalid catch up status: last finalized block not finalized." 
            peerbb <- lift $ resolveBlock (cusBestBlock peerCUS)
            let
                fj Nothing (_, l) = (True, l)
                fj (Just b) (j, l) = (j, b : l)
            (pfjMissing, peerFinJustifiers) <- (foldr fj (False, [])) <$> mapM (lift . resolveBlock) (cusFinalizationJustifiers peerCUS) 
            -- We should mark the peer as pending if we don't recognise its best block
            let catchUpWithPeer = isNothing peerbb || pfjMissing
            if cusIsRequest peerCUS then do
                -- Response required so determine finalization records and chain to send
                frs <- getFinalizationFromIndex (finalizationIndex peerFinRec + 1)
                bb <- bestBlock
                finParams <- genesisFinalizationParameters <$> getGenesisData
                let justHeight = nextFinalizationJustifierHeight finParams lastFinRec lfb
                justifiers <- getBlocksAtHeight justHeight
                -- We want our best chain up to the latest known common ancestor of the
                -- peer's best block.  If we know that block, start there; otherwise, 
                -- start with the peer's last finalized block.
                peerBranches <- catMaybes <$> mapM (lift . resolveBlock) (cusAdditionalBlocks peerCUS)
                let knownBlocks = makeKnownBlocks $ peerlfb : maybe id (:) peerbb peerFinJustifiers ++ peerBranches
                let
                    makeChain' kb b l = case checkKnownBlock b kb of
                        (True, _) -> (kb, l)
                        (False, kb') -> makeChain' kb' (bpParent b) (b : l)
                    makeChain kb b = makeChain' kb b []
                    (_, chain) = foldl (\(kbs, ch0) b -> let (kbs', ch1) = makeChain kbs b in (addKnownBlock b kbs', ch0 ++ ch1)) (knownBlocks, []) (bb : justifiers)
                    myCUS = makeCatchUpStatus False lfb bb justifiers []
                    -- Note: since the list can be truncated, we have to be careful about the
                    -- order that finalization records are interleaved with blocks.
                    -- Specifically, we send a finalization record as soon as possible after
                    -- the corresponding block; and where the block is not being sent, we
                    -- send the finalization record before all other blocks.  We also send
                    -- finalization records and blocks in order.
                    merge [] bs = Right <$> bs
                    merge fs [] = Left . fst <$> fs
                    merge fs0@((f, fb) : fs1) bs0@(b : bs1)
                        | bpHeight fb < bpHeight b = Left f : merge fs1 bs0
                        | otherwise = Right b : merge fs0 bs1
                return (Just (merge frs chain, myCUS), catchUpWithPeer)
            else
                -- No response required
                return (Nothing, catchUpWithPeer)
