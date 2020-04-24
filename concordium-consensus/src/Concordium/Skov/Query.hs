module Concordium.Skov.Query where

import Control.Monad
import Data.Functor
import qualified Data.Sequence as Seq

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Finalization
import Concordium.Types
import Concordium.Kontrol.UpdateLeaderElectionParameters

doResolveBlock :: TreeStateMonad m => BlockHash -> m (Maybe (BlockPointerType m))
{-# INLINE doResolveBlock #-}
doResolveBlock cbp = getBlockStatus cbp <&> \case
        Just (BlockAlive bp) -> Just bp
        Just (BlockFinalized bp _) -> Just bp
        _ -> Nothing

doIsFinalized :: TreeStateMonad m => BlockHash -> m Bool
{-# INLINE doIsFinalized #-}
doIsFinalized = getBlockStatus >=> \case
        Just (BlockFinalized _ _) -> return True
        _ -> return False

doGetBirkParameters :: (BlockPointerMonad m, BlockStateQuery m) => Slot -> BlockPointerType m -> m (BirkParameters m)
{-# INLINE doGetBirkParameters #-}
doGetBirkParameters slot bp = do
        params <- getBlockBirkParameters =<< blockState bp
        slotDependentBirkParameters slot params

doGetCurrentHeight :: TreeStateMonad m => m BlockHeight
{-# INLINE doGetCurrentHeight #-}
doGetCurrentHeight = do
        lfHeight <- getLastFinalizedHeight
        branchLen <- fromIntegral . Seq.length <$> getBranches
        return $ lfHeight + branchLen

doBranchesFromTop :: TreeStateMonad m => m [[BlockPointerType m]]
{-# INLINE doBranchesFromTop #-}
doBranchesFromTop = revSeqToList <$> getBranches
    where
        revSeqToList Seq.Empty = []
        revSeqToList (r Seq.:|> t) = t : revSeqToList r


doGetBlocksAtHeight :: (BlockPointerMonad m, TreeStateMonad m) => BlockHeight -> m [BlockPointerType m]
{-# INLINE doGetBlocksAtHeight #-}
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
              par <- bpParent lastFin
              parPar <- findFrom par
              return [parPar] -- TODO: replace with more efficient search
    where
        findFrom bp
            | bpHeight bp == h = return bp
            | otherwise = do
                par <- bpParent bp
                findFrom par

doBlockLastFinalizedIndex :: TreeStateMonad m => BlockPointerType m -> m FinalizationIndex
{-# INLINE doBlockLastFinalizedIndex #-}
doBlockLastFinalizedIndex bp = getBlockStatus (bpLastFinalizedHash bp) <&> \case
        Just (BlockFinalized _ fr) -> finalizationIndex fr
        _ -> error "Invariant violation: last finalized block is not finalized."
