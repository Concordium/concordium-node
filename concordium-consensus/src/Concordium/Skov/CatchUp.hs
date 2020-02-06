{-# LANGUAGE
    ViewPatterns,
    ScopedTypeVariables #-}
module Concordium.Skov.CatchUp where

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Function
import Data.Foldable
import Data.ByteString (ByteString)
import Data.Serialize

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState hiding (getGenesisData)

import Concordium.Logger
import Concordium.Skov.CatchUp.Types
import Concordium.Skov.Monad
import Concordium.Kontrol.BestBlock
import Concordium.Afgjort.Finalize


makeCatchUpStatus :: (BlockPointerData b) => Bool -> Bool -> b -> [b] -> [b] -> CatchUpStatus
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
leavesBranches :: (BlockPointerData b) => [[b]] -> ([b], [b])
leavesBranches = lb ([], [])
    where
        lb lsbs [] = lsbs
        lb (ls, bs) [ls'] = (ls ++ ls', bs)
        lb (ls, bs) (s:r@(n:_))
            = let (bs', ls') = List.partition (`elem` (bpParent <$> n)) s
                in lb (ls ++ ls', bs ++ bs') r

doGetCatchUpStatus :: (TreeStateMonad m, SkovQueryMonad m, LoggerMonad m) => Bool -> m CatchUpStatus
doGetCatchUpStatus cusIsRequest = do
        logEvent Skov LLTrace "Getting catch-up status"
        lfb <- lastFinalizedBlock
        (leaves, branches) <- leavesBranches . toList <$> getBranches
        return $ makeCatchUpStatus cusIsRequest False lfb leaves (if cusIsRequest then branches else [])

doHandleCatchUp :: forall m. (TreeStateMonad m, SkovQueryMonad m, FinalizationMonad m, LoggerMonad m) => CatchUpStatus -> m (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)
doHandleCatchUp peerCUS = do
        let resultDoCatchUp = if cusIsResponse peerCUS then ResultPendingBlock else ResultContinueCatchUp
        lfb <- lastFinalizedBlock
        if cusLastFinalizedHeight peerCUS > bpHeight lfb then do
            -- Our last finalized height is below the peer's last finalized height
            response <-
                if cusIsRequest peerCUS then do
                    myCUS <- doGetCatchUpStatus False
                    return $ Just ([], myCUS {cusIsResponse = True})
                else
                    return Nothing
            -- We are behind, so we mark the peer as pending, unless it is in progress
            -- and the message is not a response.
            return (response, if cusIsResponse peerCUS then ResultPendingBlock else ResultContinueCatchUp)
        else
            -- Our last finalized height is at least the peer's last finalized height
            -- Check if the peer's last finalized block is recognised
            getBlockStatus (cusLastFinalizedBlock peerCUS) >>= \case
                Just (BlockFinalized _ peerFinRec) -> do
                    -- Determine if we need to catch up: i.e. if the peer has some
                    -- leaf block we do not recognise.
                    let
                        testLeaves [] = return False
                        testLeaves (l:ls) = resolveBlock l >>= \case
                            Nothing -> return True
                            Just _ -> testLeaves ls
                    catchUpWithPeer <- testLeaves (cusLeaves peerCUS)
                    let catchUpResult = if catchUpWithPeer then resultDoCatchUp else ResultSuccess

                    if cusIsRequest peerCUS then do
                        frs <- (finalizationUnsettledRecords (finalizationIndex peerFinRec)) >>= mapM (\r ->
                                    (r,) . maybe 0 bpHeight <$> resolveBlock (finalizationBlockPointer r)
                                    )
                        let peerKnownBlocks = Set.insert (cusLastFinalizedBlock peerCUS) $
                                Set.fromList (cusLeaves peerCUS) `Set.union` Set.fromList (cusBranches peerCUS)
                        let
                            extendBackBranches b bs
                                | bpHash b `Set.member` peerKnownBlocks = bs
                                | bpHeight b == 0 = bs -- Genesis block should always be known, so this case should be unreachable
                                | otherwise = extendBackBranches (bpParent b) (b Seq.<| bs)
                            unknownFinTrunk = extendBackBranches lfb Seq.Empty
                        -- Take the branches; filter out all blocks that the client claims knowledge of; extend branches back
                        -- to include finalized blocks until the parent is known.
                        myBranches <- getBranches
                        let
                            -- Filter out blocks that are known to the peer
                            filterUnknown :: [BlockPointer m] -> [BlockPointer m]
                            filterUnknown = filter ((`Set.notMember` peerKnownBlocks) . bpHash)
                            -- Given a branches structure, filter out the blocks known to the peer and split off
                            -- the oldest block (not known to the peer).  These are returned as two sequences
                            filterTakeOldest Seq.Empty = (Seq.Empty, Seq.Empty)
                            filterTakeOldest (l Seq.:<| bs)
                                | null l' = filterTakeOldest bs
                                | otherwise = filterTakeOldest' m (Seq.singleton $ List.delete m l') (Seq.singleton l') bs
                                where
                                    l' = filterUnknown l
                                    m = minimumBy (compare `on` bpArriveTime) l'
                            filterTakeOldest' oldest sansOldest _ Seq.Empty = (Seq.singleton oldest, sansOldest)
                            filterTakeOldest' oldest sansOldest withOldest r@(l Seq.:<| bs)
                                -- If all blocks at this level are no older than the oldest block so far,
                                -- then that is the oldest block, and we can just filter the remaining blocks
                                | all (\b -> bpArriveTime b >= bpArriveTime oldest) l = (Seq.singleton oldest, sansOldest <> fmap filterUnknown r)
                                -- Otherwise, there could be an older block
                                | otherwise = filterTakeOldest' newOldest newSansOldest (withOldest Seq.|> l') bs
                                    where
                                        l' = filterUnknown l
                                        m = minimumBy (compare `on` bpArriveTime) l'
                                        (newOldest, newSansOldest) = if null l' || bpArriveTime oldest <= bpArriveTime m
                                            then (oldest, sansOldest Seq.|> l')
                                            else (m, withOldest Seq.|> List.delete m l')
                            (outBlocks1, branches1) = case unknownFinTrunk of
                                (b Seq.:<| bs) -> (Seq.singleton b, fmap (:[]) bs <> fmap filterUnknown myBranches)
                                Seq.Empty -> filterTakeOldest myBranches
                            trim = Seq.dropWhileR null
                            takeBranches :: Seq.Seq (BlockPointer m) -> Seq.Seq [BlockPointer m] -> m (Seq.Seq (BlockPointer m))
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
                            encodeBlock b = (MessageBlock, runPut (putBlock b))
                            encodeFinRec fr = (MessageFinalizationRecord, encode fr)
                            -- Note: since the returned list can be truncated, we have to be careful about the
                            -- order that finalization records are interleaved with blocks.
                            -- Specifically, we send a finalization record as soon as possible after
                            -- the corresponding block; and where the block is not being sent, we
                            -- send the finalization record before all other blocks.  We also send
                            -- finalization records and blocks in order.
                            merge Seq.Empty bs = encodeBlock <$> toList bs
                            merge fs Seq.Empty = encodeFinRec <$> toList fs
                            merge fs0@((f, fh) Seq.:<| fs1) bs0@(b Seq.:<| bs1)
                                | fh < bpHeight b = encodeFinRec f : merge fs1 bs0
                                | otherwise = encodeBlock b : merge fs0 bs1
                        return (Just (merge frs outBlocks2, myCUS), catchUpResult)
                    else
                        -- No response required
                        return (Nothing, catchUpResult)
                _ -> do
                    -- If the peer's last finalized block is not known to be finalized (to us)
                    -- then we must effectively be on different finalized chains, so we should
                    -- reject this peer.
                    logEvent Skov LLWarning $ "Invalid catch up status: last finalized block not finalized."
                    return (Nothing, ResultInvalid)



