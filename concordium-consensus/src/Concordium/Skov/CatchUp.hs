{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Concordium.Skov.CatchUp where

import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Maybe
import Data.Functor

import Concordium.Types
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import Concordium.Logger

import Concordium.Skov.Monad
import Concordium.Kontrol.BestBlock

data CatchUpStatus = CatchUpStatus {
    -- |If this flag is set, the recipient is required to respond with its own
    -- 'CatchUpStatus'.
    cusIsRequest :: Bool,
    cusLastFinalizedBlock :: BlockHash,
    cusLastFinalizedHeight :: BlockHeight,
    cusBestBlock :: BlockHash,
    cusBestHeight :: BlockHeight
} deriving (Show)

getCatchUpStatus :: SkovQueryMonad m => Bool -> m CatchUpStatus
getCatchUpStatus cusIsRequest = do
        lfb <- lastFinalizedBlock
        let 
            cusLastFinalizedBlock = bpHash lfb
            cusLastFinalizedHeight = bpHeight lfb
        bb <- bestBlock
        let
            cusBestBlock = bpHash bb
            cusBestHeight = bpHeight bb
        return CatchUpStatus{..}

handleCatchUp :: (TreeStateMonad m, SkovQueryMonad m, LoggerMonad m) => CatchUpStatus -> m (Maybe ([FinalizationRecord], [BlockPointer m], Maybe CatchUpStatus))
handleCatchUp peerCUS = runMaybeT $ do
        lfb <- lastFinalizedBlock
        if cusLastFinalizedHeight peerCUS > bpHeight lfb then do
            myCUS <- getCatchUpStatus True
            return ([], [], Just myCUS)
        else do
            (peerlfb, frs) <- getBlockStatus (cusLastFinalizedBlock peerCUS) >>= \case
                Just (BlockFinalized peerlfb peerFinRec) -> do
                    frs <- getFinalizationFromIndex (finalizationIndex peerFinRec + 1)
                    return (peerlfb, frs)
                _ -> do
                    logEvent Skov LLWarning $ "Invalid catch up status: last finalized block not finalized." 
                    mzero
            bb <- bestBlock
            -- We want our best chain up to the latest known common ancestor of the
            -- peer's best block.  If we know that block, start there; otherwise, 
            -- start with the peer's last finalized block.
            peerbb <- resolveBlock (cusBestBlock peerCUS)
            let
                makeChain c b l = case compare (bpHeight c) (bpHeight b) of
                    LT -> makeChain c (bpParent b) (b : l)
                    EQ -> makeChain' c b l
                    GT -> makeChain (bpParent c) b l
                makeChain' c b l
                    | c == b = l
                    | otherwise = makeChain' (bpParent c) (bpParent b) (b : l)
                chain = makeChain (fromMaybe peerlfb peerbb) bb []
                myCUS = CatchUpStatus {
                        cusIsRequest = isNothing peerbb,
                        cusLastFinalizedBlock = bpHash lfb,
                        cusLastFinalizedHeight = bpHeight lfb,
                        cusBestBlock = bpHash bb,
                        cusBestHeight = bpHeight bb
                    }
                mmyCUS = myCUS <$ guard (cusIsRequest peerCUS || not (null frs) || not (null chain))
            return (frs, chain, mmyCUS)
