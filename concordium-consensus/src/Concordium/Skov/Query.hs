{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DerivingStrategies, DerivingVia, UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Concordium.Skov.Query where

import Control.Monad
import Data.Functor
import qualified Data.Sequence as Seq

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.TreeState
import Concordium.Types
import qualified Concordium.GlobalState.Parameters as Param

doResolveBlock :: TreeStateMonad m => BlockHash -> m (Maybe (BlockPointer m))
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

doGetBirkParameters :: TreeStateMonad m => Slot -> BlockPointer m -> m Param.BirkParameters
{-# INLINE doGetBirkParameters #-}
doGetBirkParameters slot bp = getBlockBirkParameters (bpState bp)

doGetCurrentHeight :: TreeStateMonad m => m BlockHeight
{-# INLINE doGetCurrentHeight #-}
doGetCurrentHeight = do
        lfHeight <- getLastFinalizedHeight
        branchLen <- fromIntegral . Seq.length <$> getBranches
        return $ lfHeight + branchLen

doBranchesFromTop :: TreeStateMonad m => m [[BlockPointer m]]
{-# INLINE doBranchesFromTop #-}
doBranchesFromTop = revSeqToList <$> getBranches
    where
        revSeqToList Seq.Empty = []
        revSeqToList (r Seq.:|> t) = t : revSeqToList r


doGetBlocksAtHeight :: TreeStateMonad m => BlockHeight -> m [BlockPointer m]
{-# INLINE doGetBlocksAtHeight #-}
doGetBlocksAtHeight h = do
        lastFin <- getLastFinalized
        case compare h (bpHeight lastFin) of
            EQ -> return [lastFin]
            GT -> do
                brs <- getBranches
                case brs Seq.!? (fromIntegral $ h - bpHeight lastFin - 1) of
                    Nothing -> return []
                    Just bs -> return bs
            LT -> return [findFrom (bpParent lastFin)] -- TODO: replace with more efficient search
    where
        findFrom bp
            | bpHeight bp == h = bp
            | otherwise = findFrom (bpParent bp)

