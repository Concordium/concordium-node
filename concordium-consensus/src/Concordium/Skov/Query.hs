{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.Skov.Query where

import Control.Monad
import Data.Functor
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Foldable

import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Finalization
import Concordium.Types
import Concordium.Types.Updates
import Concordium.Skov.CatchUp.Types

doResolveBlock :: TreeStateMonad pv m => BlockHash -> m (Maybe (BlockPointerType m))
{- - INLINE doResolveBlock - -}
doResolveBlock cbp = getBlockStatus cbp <&> \case
        Just (BlockAlive bp) -> Just bp
        Just (BlockFinalized bp _) -> Just bp
        _ -> Nothing

doIsFinalized :: TreeStateMonad pv m => BlockHash -> m Bool
{- - INLINE doIsFinalized - -}
doIsFinalized = getBlockStatus >=> \case
        Just (BlockFinalized _ _) -> return True
        _ -> return False

doGetCurrentHeight :: TreeStateMonad pv m => m BlockHeight
{- - INLINE doGetCurrentHeight - -}
doGetCurrentHeight = do
        lfHeight <- getLastFinalizedHeight
        branchLen <- fromIntegral . Seq.length <$> getBranches
        return $ lfHeight + branchLen

doBranchesFromTop :: TreeStateMonad pv m => m [[BlockPointerType m]]
{- - INLINE doBranchesFromTop - -}
doBranchesFromTop = revSeqToList <$> getBranches
    where
        revSeqToList Seq.Empty = []
        revSeqToList (r Seq.:|> t) = t : revSeqToList r


doGetBlocksAtHeight :: TreeStateMonad pv m => BlockHeight -> m [BlockPointerType m]
{- - INLINE doGetBlocksAtHeight - -}
doGetBlocksAtHeight h = do
        lastFin <- fst <$> getLastFinalized
        case compare h (bpHeight lastFin) of
            EQ -> return [lastFin]
            GT -> do
                brs <- getBranches
                case brs Seq.!? fromIntegral (h - bpHeight lastFin - 1) of
                    Nothing -> return []
                    Just bs -> return bs
            LT -> do
                mb <- getFinalizedAtHeight h
                return (toList mb)

doBlockLastFinalizedIndex :: TreeStateMonad pv m => BlockPointerType m -> m FinalizationIndex
{- - INLINE doBlockLastFinalizedIndex - -}
doBlockLastFinalizedIndex bp = getBlockStatus (bpLastFinalizedHash bp) <&> \case
        Just (BlockFinalized _ fr) -> finalizationIndex fr
        _ -> error "Invariant violation: last finalized block is not finalized."


-- |Get a catch-up status message. The flag indicates if the
-- message should be a catch-up request.
doGetCatchUpStatus :: (TreeStateMonad pv m) => Bool -> m CatchUpStatus
doGetCatchUpStatus cusIsRequest = do
        lfb <- fst <$> getLastFinalized
        br <- toList <$> getBranches
        (leaves, branches) <- leavesBranches br
        makeCatchUpStatus cusIsRequest False lfb leaves (if cusIsRequest then branches else [])

doGetProtocolUpdateStatus :: (TreeStateMonad pv m) => m (Either ProtocolUpdate [(TransactionTime, ProtocolUpdate)])
doGetProtocolUpdateStatus = do
        (lastFin, _) <- getLastFinalized
        lastFinState <- blockState lastFin
        getProtocolUpdateStatus lastFinState

doIsShutDown :: (TreeStateMonad pv m) => m Bool
doIsShutDown = do
        status <- doGetProtocolUpdateStatus
        return $ case status of
            Left _ -> True
            Right _ -> False

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
