{-# LANGUAGE
    ViewPatterns,
    ScopedTypeVariables #-}
module Concordium.Skov.CatchUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Function
import Data.Foldable
import Control.Monad

import Concordium.Types
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState hiding (getGenesisData)

import Concordium.Logger
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

makeCatchUpStatus :: (BlockPointerMonad m) => Bool -> Bool -> (BlockPointerType m) -> [BlockPointerType m] -> [BlockPointerType m] -> m CatchUpStatus
makeCatchUpStatus cusIsRequest cusIsResponse lfb leaves branches = return CatchUpStatus{..}
    where
        cusLastFinalizedBlock = bpHash lfb
        cusLastFinalizedHeight = bpHeight lfb
        cusLeaves = bpHash <$> leaves
        cusBranches = bpHash <$> branches

-- |Given a list of lists representing branches (ordered by height),
-- produce a pair of lists @(leaves, branches)@, which partions
-- those blocks that are leaves (@leaves@) from those that are not
-- (@branches@).
leavesBranches :: forall m. (BlockPointerMonad m) => [[BlockPointerType m]] -> m ([BlockPointerType m], [BlockPointerType m])
leavesBranches = lb ([], [])
    where
        lb :: ([BlockPointerType m], [BlockPointerType m]) -> [[BlockPointerType m]] -> m ([BlockPointerType m], [BlockPointerType m])
        lb lsbs [] = return lsbs
        lb (ls, bs) [ls'] = return (ls ++ ls', bs)
        lb (ls, bs) (s:r@(n:_)) = do
          parent <- mapM bpParent n
          let (bs', ls') = List.partition (`elem` parent) s
          lb (ls ++ ls', bs ++ bs') r

getCatchUpStatus :: (BlockPointerMonad m, TreeStateMonad m, SkovQueryMonad m, LoggerMonad m) => Bool -> m CatchUpStatus
getCatchUpStatus cusIsRequest = do
        logEvent Skov LLTrace "Getting catch-up status"
        lfb <- lastFinalizedBlock
        br <- toList <$> getBranches
        (leaves, branches) <- leavesBranches br
        makeCatchUpStatus cusIsRequest False lfb leaves (if cusIsRequest then branches else [])

handleCatchUp :: forall m. (BlockPointerMonad m, TreeStateMonad m, SkovQueryMonad m, LoggerMonad m) => CatchUpStatus -> m (Either String (Maybe ([Either FinalizationRecord (BlockPointerType m)], CatchUpStatus), Bool))
handleCatchUp peerCUS = runExceptT $ do
        lfb <- lift lastFinalizedBlock
        if cusLastFinalizedHeight peerCUS > bpHeight lfb then do
            -- Our last finalized height is below the peer's last finalized height
            response <-
                if cusIsRequest peerCUS then do
                    myCUS <- lift $ getCatchUpStatus False
                    return $ Just ([], myCUS {cusIsResponse = True})
                else
                    return Nothing
            -- We are behind, so we mark the peer as pending.
            return (response, True)
        else do
            -- Our last finalized height is at least the peer's last finalized height
            -- Check if the peer's last finalized block is recognised
            peerFinRec <- getBlockStatus (cusLastFinalizedBlock peerCUS) >>= \case
                Just (BlockFinalized _ peerFinRec) -> return peerFinRec
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
                        | bpHash b `Set.member` peerKnownBlocks = return bs
                        | bpHeight b == 0 = return bs -- Genesis block should always be known, so this case should be unreachable
                        | otherwise = do
                                 parent <- bpParent b
                                 extendBackBranches parent (b Seq.<| bs)
                    unknownFinTrunk = extendBackBranches lfb Seq.Empty
                -- Take the branches; filter out all blocks that the client claims knowledge of; extend branches back
                -- to include finalized blocks until the parent is known.
                myBranches <- lift getBranches
                let
                    -- Filter out blocks that are known to the peer
                    filterUnknown :: [BlockPointerType m] -> [BlockPointerType m]
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
                unk <- lift unknownFinTrunk
                let (outBlocks1, branches1) =
                       case unk of
                        (b Seq.:<| bs) -> (Seq.singleton b, fmap (:[]) bs <> fmap filterUnknown myBranches)
                        Seq.Empty -> filterTakeOldest myBranches
                let trim = Seq.dropWhileR null
                    takeBranches :: Seq.Seq (BlockPointerType m) -> Seq.Seq [BlockPointerType m] -> ExceptT String m (Seq.Seq (BlockPointerType m))
                    takeBranches out (trim -> brs) = bestBlockOf brs >>= \case
                        Nothing -> return out
                        Just bb -> (out <>) <$> innerLoop Seq.empty brs Seq.empty bb
                    innerLoop out Seq.Empty brs _ = takeBranches out brs
                    innerLoop out brsL0@(brsL Seq.:|> bs) brsR bb
                        | bb `elem` bs = do
                              parent <- bpParent bb
                              innerLoop (bb Seq.<| out) brsL (List.delete bb bs Seq.<| brsR) parent
                        | otherwise = takeBranches out (brsL0 <> brsR)
                outBlocks2 <- takeBranches outBlocks1 branches1
                lvs <- leavesBranches $ toList myBranches
                myCUS <- makeCatchUpStatus False True lfb (fst lvs) []
                    -- Note: since the returned list can be truncated, we have to be careful about the
                    -- order that finalization records are interleaved with blocks.
                    -- Specifically, we send a finalization record as soon as possible after
                    -- the corresponding block; and where the block is not being sent, we
                    -- send the finalization record before all other blocks.  We also send
                    -- finalization records and blocks in order.
                let merge [] bs = Right <$> toList bs
                    merge fs Seq.Empty = Left . fst <$> fs
                    merge fs0@((f, fb) : fs1) bs0@(b Seq.:<| bs1)
                        | bpHeight fb < bpHeight b = Left f : merge fs1 bs0
                        | otherwise = Right b : merge fs0 bs1
                return (Just (merge frs outBlocks2, myCUS), catchUpWithPeer)
            else
                -- No response required
                return (Nothing, catchUpWithPeer)
