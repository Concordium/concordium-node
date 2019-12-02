{-# LANGUAGE
    ViewPatterns,
    ScopedTypeVariables #-}
module Concordium.Skov.CatchUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Serialize
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Function
import Data.Foldable
import Lens.Micro.Platform
import Control.Monad

import Concordium.Types
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState hiding (getGenesisData)

import Concordium.Skov.Monad
import Concordium.Kontrol.BestBlock

data CatchUpStatus = CatchUpStatus {
    -- |If this flag is set, the recipient is expected to send any
    -- blocks and finalization records the sender may be missing,
    -- followed by a CatchUpStatus message with the response flag
    -- set.
    cusIsRequest :: Bool,
    -- |If this flag is set, this message concludes a catch-up
    -- response. (The receiver should not expect to be sent
    -- further catch-up blocks unless it sends a further catch-up
    -- request.)
    cusIsResponse :: Bool,
    -- |Hash of the sender's last finalized block.
    cusLastFinalizedBlock :: BlockHash,
    -- |Height of the sender's last finalized block.
    cusLastFinalizedHeight :: BlockHeight,
    -- |Hashes of all live non-finalized leaf blocks.
    cusLeaves :: [BlockHash],
    -- |Hashes of all live non-finalized non-leaf blocks, if the message
    -- is a request.
    cusBranches :: [BlockHash]
} deriving (Show)
instance Serialize CatchUpStatus where
    put CatchUpStatus{..} = do
        putWord8 $ case (cusIsRequest, cusIsResponse) of
            (False, False) -> 0
            (True, False) -> 1
            (False, True) -> 2
            (True, True) -> 3
        put cusLastFinalizedBlock
        put cusLastFinalizedHeight
        put cusLeaves
        when cusIsRequest $ put cusBranches
    get = do
        (cusIsRequest, cusIsResponse) <- getWord8 >>= \case
            0 -> return (False, False)
            1 -> return (True, False)
            2 -> return (False, True)
            3 -> return (True, True)
            _ -> fail "Invalid flags"
        cusLastFinalizedBlock <- get
        cusLastFinalizedHeight <- get
        cusLeaves <- get
        cusBranches <- if cusIsRequest then get else return []
        return CatchUpStatus{..}

makeCatchUpStatus :: (BlockPointerData bs b) => Bool -> Bool -> b -> [b] -> [b] -> CatchUpStatus
makeCatchUpStatus cusIsRequest cusIsResponse lfb leaves branches = CatchUpStatus{..}
    where
        cusLastFinalizedBlock = bpHash lfb
        cusLastFinalizedHeight = bpHeight lfb
        cusLeaves = bpHash <$> leaves
        cusBranches = bpHash <$> branches

-- |Given a list of lists representing branches (ordered by height),
-- produce a pair of lists @(leaves, branches)@, which partions
-- those blocks that are leves (@leaves@) from those that are not
-- (@branches@).
leavesBranches :: (BlockPointerData bs b) => [[b]] -> ([b], [b])
leavesBranches = lb ([], [])
    where
        lb lsbs [] = lsbs
        lb (ls, bs) [ls'] = (ls ++ ls', bs)
        lb (ls, bs) (s:r@(n:_))
            = let (bs', ls') = List.partition (`elem` (bpParent <$> n)) s
                in lb (ls ++ ls', bs ++ bs') r

getCatchUpStatus :: (TreeStateMonad m, SkovQueryMonad m) => Bool -> m CatchUpStatus
getCatchUpStatus cusIsRequest = do
        lfb <- lastFinalizedBlock
        (leaves, branches) <- leavesBranches . toList <$> getBranches
        return $ makeCatchUpStatus cusIsRequest False lfb leaves (if cusIsRequest then branches else [])

data KnownBlocks b = KnownBlocks {
    -- |The known blocks indexed by height
    kbHeightMap :: Map.Map BlockHeight (Set.Set b),
    -- |The map should contain all ancestors of all blocks in the map
    -- with height at least 'kbAncestorsHeight'.
    kbAncestorsHeight :: BlockHeight
}

emptyKnownBlocks :: KnownBlocks b
emptyKnownBlocks = KnownBlocks Map.empty 0

addKnownBlock :: (BlockPointerData bs b, Ord b) => b -> KnownBlocks b -> KnownBlocks b
addKnownBlock b kb@(KnownBlocks m h) = if present then kb else KnownBlocks m' (max h (bpHeight b))
    where
        (present, m') = Map.alterF upd (bpHeight b) m
        upd Nothing = (False, Just $! Set.singleton b)
        upd (Just s) = if b `Set.member` s then (True, Just s) else (False, Just $! Set.insert b s)

makeKnownBlocks :: (BlockPointerData bs b, Ord b) => [b] -> KnownBlocks b
makeKnownBlocks = foldr addKnownBlock emptyKnownBlocks

updateKnownBlocksToHeight :: (BlockPointerData bs b, Ord b) => BlockHeight -> KnownBlocks b -> KnownBlocks b
updateKnownBlocksToHeight h kb@(KnownBlocks m hkb)
        | h >= kbAncestorsHeight kb = kb
        | otherwise = updateKnownBlocksToHeight h kb'
    where
        kb' = KnownBlocks m' (hkb - 1)
        genhkb = Map.lookup hkb m
        genhkb' = Set.fromList $ maybe [] (fmap bpParent . Set.toList) genhkb
        m' = m & at (hkb - 1) . non Set.empty %~ Set.union genhkb'

checkKnownBlock :: (BlockPointerData bs b, Ord b) => b -> KnownBlocks b -> (Bool, KnownBlocks b)
checkKnownBlock b kb = (b `Set.member` (m ^. at (bpHeight b) . non Set.empty), kb')
    where
        kb'@(KnownBlocks m _) = updateKnownBlocksToHeight (bpHeight b) kb


handleCatchUp :: forall m. (TreeStateMonad m, SkovQueryMonad m) => CatchUpStatus -> m (Either String (Maybe ([Either FinalizationRecord (BlockPointer m)], CatchUpStatus), Bool))
handleCatchUp peerCUS = runExceptT $ do
        lfb <- lift lastFinalizedBlock
        if cusLastFinalizedHeight peerCUS > bpHeight lfb then do
            -- Our last finalized height is below the peer's last finalized height
            response <-
                if cusIsRequest peerCUS then do
                    myCUS <- lift $ getCatchUpStatus False
                    return $ Just ([], myCUS)
                else
                    return Nothing
            -- We are behind, so we mark the peer as pending.
            return (response, True)
        else do
            -- Our last finalized height is at least the peer's last finalized height
            -- Check if the peer's last finalized block is recognised
            (peerlfb, peerFinRec) <- getBlockStatus (cusLastFinalizedBlock peerCUS) >>= \case
                Just (BlockFinalized peerlfb peerFinRec) ->
                    return (peerlfb, peerFinRec)
                _ ->
                    -- If the peer's last finalized block is not known to be finalized (to us)
                    -- then we must effectively be on different finalized chains, so we should
                    -- reject this peer.
                    throwE $ "Invalid catch up status: last finalized block not finalized." 

            -- Determine if we need to catch up: i.e. if the peer has some
            -- leaf block we do not recognise.
            let
                testLeaves [] = return False
                testLeaves (l:ls) = resolveBlock l >>= \case
                    Nothing -> return True
                    Just _ -> testLeaves ls
            catchUpWithPeer <- testLeaves (cusLeaves peerCUS)

            if cusIsRequest peerCUS then do
                frs <- getFinalizationFromIndex (finalizationIndex peerFinRec + 1)
                let peerKnownBlocks = Set.insert (cusLastFinalizedBlock peerCUS) $
                        Set.fromList (cusLeaves peerCUS) `Set.union` Set.fromList (cusBranches peerCUS)
                let
                    extendBackBranches b bs
                        | bpHash b `Set.member` peerKnownBlocks = bs
                        | bpHeight b == 0 = bs -- Genesis block should always be known, so this case should be unreachable
                        | otherwise = extendBackBranches (bpParent b) ([b] Seq.<| bs)
                -- Take the branches; filter out all blocks that the client claims knowledge of; extend branches back
                -- to include finalized blocks until the parent is known.
                myBranches <- getBranches
                let
                    branches0 = extendBackBranches lfb . fmap (filter ((`Set.notMember` peerKnownBlocks) . bpHash)) $ myBranches
                    takeOldest Seq.Empty = (Seq.Empty, Seq.Empty)
                    takeOldest ([] Seq.:<| bs) = takeOldest bs
                    takeOldest ([o] Seq.:<| bs) = (Seq.singleton o, bs)
                    takeOldest (l Seq.:<| bs) = (Seq.singleton m, l' Seq.<| bs)
                        where
                            m = minimumBy (compare `on` bpArriveTime) l
                            l' = List.delete m l
                    (outBlocks1, branches1) = takeOldest branches0
                    trim = Seq.dropWhileR null
                    takeBranches :: Seq.Seq (BlockPointer m) -> Seq.Seq [BlockPointer m] -> ExceptT String m (Seq.Seq (BlockPointer m))
                    takeBranches out (trim -> brs) = bestBlockOf brs >>= \case
                        Nothing -> return out
                        Just bb -> (out <>) <$> innerLoop Seq.empty brs Seq.empty bb
                    innerLoop out Seq.Empty brs _ = takeBranches out brs
                    innerLoop out brsL0@(brsL Seq.:|> bs) brsR bb
                        | bb `elem` bs = innerLoop (bb Seq.<| out) brsL (List.delete bb bs Seq.<| brsR) (bpParent bb)
                        | otherwise = takeBranches out (brsL0 <> brsR)
                outBlocks2 <- takeBranches outBlocks1 branches1
                let
                    myCUS = makeCatchUpStatus False True lfb (fst $ leavesBranches $ toList myBranches) []
                    -- Note: since the returned list can be truncated, we have to be careful about the
                    -- order that finalization records are interleaved with blocks.
                    -- Specifically, we send a finalization record as soon as possible after
                    -- the corresponding block; and where the block is not being sent, we
                    -- send the finalization record before all other blocks.  We also send
                    -- finalization records and blocks in order.
                    merge [] bs = Right <$> toList bs
                    merge fs Seq.Empty = Left . fst <$> fs
                    merge fs0@((f, fb) : fs1) bs0@(b Seq.:<| bs1)
                        | bpHeight fb < bpHeight b = Left f : merge fs1 bs0
                        | otherwise = Right b : merge fs0 bs1
                return (Just (merge frs outBlocks2, myCUS), catchUpWithPeer)
            else
                -- No response required
                return (Nothing, catchUpWithPeer)
