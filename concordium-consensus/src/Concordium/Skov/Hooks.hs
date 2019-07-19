{-# LANGUAGE TemplateHaskell, RecordWildCards, LambdaCase, OverloadedStrings #-}
module Concordium.Skov.Hooks where

import Data.Time
import Lens.Micro.Platform
import qualified Data.HashPSQ as PSQ
import Control.Monad.State.Class
import Control.Monad
import qualified Data.Aeson as AE
import Data.Aeson.TH
import Data.Char
import Data.Text (pack)

import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Execution
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Block
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Finalization

import Concordium.Skov.Monad

data Hook = Hook {
    hookBlocks :: [BlockHash]
}

data TransactionHooks = TransactionHooks {
    _hooksPSQ :: PSQ.HashPSQ TransactionHash UTCTime Hook
}

makeLenses ''TransactionHooks

class TransactionHookLenses s where
    hooks :: Lens' s TransactionHooks

purgeExpiredHooks :: (MonadState s m, TransactionHookLenses s, TimeMonad m) => m ()
purgeExpiredHooks = do
        now <- currentTime
        let purge = snd . PSQ.atMostView now
        hooks . hooksPSQ %= purge

hookOnBlock :: (SkovQueryMonad m, MonadState s m, TransactionHookLenses s, TimeMonad m, TreeStateMonad m) => BlockPointer m -> m ()
hookOnBlock bp = do
        purgeExpiredHooks
        let
            addBk Nothing = ((), Nothing)
            addBk (Just (p, h)) = ((), Just (p, h {hookBlocks = getHash bp : hookBlocks h}))
        forM_ (blockTransactions bp) $ \tr -> hooks . hooksPSQ %= snd . PSQ.alter addBk (transactionHash tr)

hookOnFinalize :: (SkovQueryMonad m, MonadState s m, TransactionHookLenses s, TimeMonad m, TreeStateMonad m) => FinalizationRecord -> BlockPointer m -> m ()
hookOnFinalize _ _ = purgeExpiredHooks

data TransactionStatus = TSAbsent | TSPending | TSCommitted | TSFinalized deriving (Eq, Ord)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower . drop 2} ''TransactionStatus)

data HookResult = HookResult {
    hookExpires :: UTCTime,
    hookTransactionHash :: TransactionHash,
    hookTransactionStatus :: TransactionStatus,
    hookTransactionResults :: [(BlockHash, ValidResult)]
}
instance AE.ToJSON HookResult where
    toJSON HookResult{..} = AE.object [
            "expires" AE..= hookExpires,
            "transactionHash" AE..= pack (show hookTransactionHash),
            "status" AE..= hookTransactionStatus,
            "results" AE..= (hookTransactionResults <&> encTR)
        ]
        where
            encTR (bh, vr) = AE.object $ ["blockHash" AE..= pack (show bh)] ++ encVR vr
            encVR (Left rej) = ["result" AE..= AE.String "reject", "rejectReason" AE..= pack (show rej)]
            encVR (Right evs) = ["result" AE..= AE.String "success", "events" AE..= (pack . show <$> evs)]

hookQueryTransaction :: (SkovQueryMonad m, MonadState s m, TransactionHookLenses s, TimeMonad m, TreeStateMonad m) => TransactionHash -> m HookResult
hookQueryTransaction th = do
        now <- currentTime
        let hookExpires = addUTCTime 600 now
        let hookTransactionHash = th
        tstat <- lookupTransaction th
        psq0 <- use (hooks . hooksPSQ)
        bps <- case PSQ.lookup th psq0 of
            Nothing -> return []
            Just (_, Hook blocks) -> do
                let filterBlocks acc b = resolveBlock b >>= \case
                            Nothing -> return acc
                            Just bp -> getTransactionOutcome (bpState bp) th >>= \case
                                Nothing -> return acc
                                Just oc -> return ((bpHash bp, oc) : acc)
                foldM filterBlocks [] blocks
        let hookTransactionStatus = case (tstat, bps) of
                (Nothing, _) -> TSAbsent
                (Just (_, True), _) -> TSFinalized
                (_, []) -> TSPending
                _ -> TSCommitted
        unless (hookTransactionStatus == TSFinalized && null bps) $
            hooks . hooksPSQ .= PSQ.insert th hookExpires (Hook $ fst <$> bps) psq0
        let hookTransactionResults = bps
        return HookResult{..}