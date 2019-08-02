{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Concordium.Skov.CatchUp where

import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Maybe
import Data.Serialize

import Concordium.Types
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import Concordium.Logger

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
    cusBestBlock :: BlockHash
} deriving (Show)
instance Serialize CatchUpStatus where
    put CatchUpStatus{..} = put cusIsRequest <> put cusLastFinalizedBlock <> put cusLastFinalizedHeight <> put cusBestBlock
    get = CatchUpStatus <$> get <*> get <*> get <*> get

makeCatchUpStatus :: (BlockPointerData b) => Bool -> b -> b -> CatchUpStatus
makeCatchUpStatus cusIsRequest lfb bb = CatchUpStatus{..}
    where
        cusLastFinalizedBlock = bpHash lfb
        cusLastFinalizedHeight = bpHeight lfb
        cusBestBlock = bpHash bb

getCatchUpStatus :: SkovQueryMonad m => Bool -> m CatchUpStatus
getCatchUpStatus cusIsRequest = do
        lfb <- lastFinalizedBlock
        bb <- bestBlock
        return $ makeCatchUpStatus cusIsRequest lfb bb

handleCatchUp :: (TreeStateMonad m, SkovQueryMonad m, LoggerMonad m) => CatchUpStatus -> m (Maybe (Maybe ([FinalizationRecord], [BlockPointer m], CatchUpStatus), Bool))
handleCatchUp peerCUS = runMaybeT $ do
        lfb <- lastFinalizedBlock
        if cusLastFinalizedHeight peerCUS > bpHeight lfb then do
            response <-
                if cusIsRequest peerCUS then do
                    myCUS <- getCatchUpStatus False
                    return $ Just ([], [], myCUS)
                else
                    return Nothing
            -- We are behind, so we mark the peer as pending.
            return (response, True)
        else do
            (peerlfb, peerFinRec) <- getBlockStatus (cusLastFinalizedBlock peerCUS) >>= \case
                Just (BlockFinalized peerlfb peerFinRec) -> do
                    return (peerlfb, peerFinRec)
                _ -> do
                    logEvent Skov LLWarning $ "Invalid catch up status: last finalized block not finalized." 
                    mzero
            peerbb <- resolveBlock (cusBestBlock peerCUS)
            -- We should mark the peer as pending if we don't recognise its best block
            let catchUpWithPeer = isNothing peerbb
            if cusIsRequest peerCUS then do
                -- Response required so determine finalization records and chain to send
                frs <- getFinalizationFromIndex (finalizationIndex peerFinRec + 1)
                bb <- bestBlock
                -- We want our best chain up to the latest known common ancestor of the
                -- peer's best block.  If we know that block, start there; otherwise, 
                -- start with the peer's last finalized block.
                let
                    makeChain c b l = case compare (bpHeight c) (bpHeight b) of
                        LT -> makeChain c (bpParent b) (b : l)
                        EQ -> makeChain' c b l
                        GT -> makeChain (bpParent c) b l
                    makeChain' c b l -- Invariant: bpHeight c == bpHeight b
                        | c == b = l
                        | otherwise = makeChain' (bpParent c) (bpParent b) (b : l)
                    chain = makeChain (fromMaybe peerlfb peerbb) bb []
                    myCUS = makeCatchUpStatus False lfb bb
                return (Just (frs, chain, myCUS), catchUpWithPeer)
            else
                -- No response required
                return (Nothing, catchUpWithPeer)
