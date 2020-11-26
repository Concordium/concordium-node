{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.Skov.Query where

import Control.Monad
import Data.Functor
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Foldable

import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Finalization
import Concordium.Types
import Concordium.Skov.CatchUp.Types

doResolveBlock :: TreeStateMonad m => BlockHash -> m (Maybe (BlockPointerType m))
{- - INLINE doResolveBlock - -}
doResolveBlock cbp = getBlockStatus cbp <&> \case
        Just (BlockAlive bp) -> Just bp
        Just (BlockFinalized bp _) -> Just bp
        _ -> Nothing

doIsFinalized :: TreeStateMonad m => BlockHash -> m Bool
{- - INLINE doIsFinalized - -}
doIsFinalized = getBlockStatus >=> \case
        Just (BlockFinalized _ _) -> return True
        _ -> return False

{-
doGetBirkParameters :: (BlockPointerMonad m, BlockStateQuery m) => Slot -> BlockPointerType m -> m (BirkParameters m)
{- - INLINE doGetBirkParameters - -}
doGetBirkParameters slot bp = do
        params <- getBlockBirkParameters =<< blockState bp
        slotDependentBirkParameters slot params
-}

doGetCurrentHeight :: TreeStateMonad m => m BlockHeight
{- - INLINE doGetCurrentHeight - -}
doGetCurrentHeight = do
        lfHeight <- getLastFinalizedHeight
        branchLen <- fromIntegral . Seq.length <$> getBranches
        return $ lfHeight + branchLen

doBranchesFromTop :: TreeStateMonad m => m [[BlockPointerType m]]
{- - INLINE doBranchesFromTop - -}
doBranchesFromTop = revSeqToList <$> getBranches
    where
        revSeqToList Seq.Empty = []
        revSeqToList (r Seq.:|> t) = t : revSeqToList r


doGetBlocksAtHeight :: TreeStateMonad m => BlockHeight -> m [BlockPointerType m]
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

doBlockLastFinalizedIndex :: TreeStateMonad m => BlockPointerType m -> m FinalizationIndex
{- - INLINE doBlockLastFinalizedIndex - -}
doBlockLastFinalizedIndex bp = getBlockStatus (bpLastFinalizedHash bp) <&> \case
        Just (BlockFinalized _ fr) -> finalizationIndex fr
        _ -> error "Invariant violation: last finalized block is not finalized."


-- |Get a catch-up status message. The flag indicates if the
-- message should be a catch-up request.
doGetCatchUpStatus :: (TreeStateMonad m) => Bool -> m CatchUpStatus
doGetCatchUpStatus cusIsRequest = do
        lfb <- fst <$> getLastFinalized
        br <- toList <$> getBranches
        (leaves, branches) <- leavesBranches br
        makeCatchUpStatus cusIsRequest False lfb leaves (if cusIsRequest then branches else [])

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
