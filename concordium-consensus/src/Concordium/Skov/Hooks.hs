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
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Block
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Finalization

import Concordium.Skov.Monad

data Hook = Hook {
    hookBlocks :: [BlockHash]
}

data TransactionHooks = TransactionHooks {
    _hooksPSQ :: !(PSQ.HashPSQ TransactionHash UTCTime Hook)
}

makeLenses ''TransactionHooks

emptyHooks :: TransactionHooks
emptyHooks = TransactionHooks PSQ.empty

class TransactionHookLenses s where
    hooks :: Lens' s TransactionHooks

purgeExpiredHooks :: (MonadState s m, TransactionHookLenses s, TimeMonad m, LoggerMonad m) => m ()
purgeExpiredHooks = do
        now <- currentTime
        oldHooks <- use $ hooks . hooksPSQ
        let (purgedHooks, newHooks) = PSQ.atMostView now oldHooks
        unless (null purgedHooks) $ logEvent Skov LLTrace $ "Purging expired hooks for transactions: " ++ show (purgedHooks <&> (^. _2))
        hooks . hooksPSQ .= newHooks

hookOnBlock :: (SkovQueryMonad m, MonadState s m, TransactionHookLenses s, TimeMonad m, LoggerMonad m, TreeStateMonad m) => BlockPointer m -> m ()
hookOnBlock bp = do
        purgeExpiredHooks
        oldHooks <- use $ hooks . hooksPSQ
        let
            addBk Nothing = (False, Nothing)
            addBk (Just (p, h)) = (True, Just (p, h {hookBlocks = getHash bp : hookBlocks h}))
            trigger psq tr = case PSQ.alter addBk (transactionHash tr) psq of
                (True, psq') -> do
                    logEvent Skov LLTrace $ "Hook for " ++ show tr ++ " triggered by block " ++ show bp
                    return psq'
                _ -> return psq
        newHooks <- foldM trigger oldHooks (blockTransactions bp)
        hooks . hooksPSQ .= newHooks

hookOnFinalize :: (MonadState s m, TransactionHookLenses s, TimeMonad m, LoggerMonad m) => FinalizationRecord -> BlockPointer m -> m ()
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
            encVR (TxReject rej execCost energyCost) =
              ["result" AE..= AE.String "reject",
               "rejectReason" AE..= pack (show rej),
               "executionCost" AE..= toInteger execCost,
               "executionEnergyCost" AE..= toInteger energyCost]
            encVR (TxSuccess evs execCost energyCost) =
              ["result" AE..= AE.String "success",
               "events" AE..= (pack . show <$> evs),
               "executionCost" AE..= toInteger execCost,
               "executionEnergyCost" AE..= toInteger energyCost]

hookQueryTransaction :: (SkovQueryMonad m, MonadState s m, TransactionHookLenses s, TimeMonad m, LoggerMonad m, TreeStateMonad m) => TransactionHash -> m HookResult
hookQueryTransaction th = do
        now <- currentTime
        let hookExpires = addUTCTime 600 now
        let hookTransactionHash = th
        tstat <- lookupTransaction th
        psq0 <- use (hooks . hooksPSQ)
        bps <- case PSQ.lookup th psq0 of
            Nothing -> do
                logEvent Skov LLDebug $ "New hook requested for transaction " ++ show th
                return []
            Just (_, Hook blocks) -> do
                logEvent Skov LLDebug $ "Existing hook requested for transaction " ++ show th
                let filterBlocks acc b = resolveBlock b >>= \case
                            Nothing -> do
                                logEvent Skov LLDebug $ "Filtering out non-live block " ++ show b ++ " for hooked transaction " ++ show th
                                return acc
                            Just bp -> getTransactionOutcome (bpState bp) th >>= \case
                                Nothing -> do
                                    logEvent Skov LLDebug $ "Filtering out block (missing transaction) " ++ show b ++ " for hooked transaction " ++ show th
                                    return acc
                                Just oc -> return ((bpHash bp, oc) : acc)
                foldM filterBlocks [] blocks
        let hookTransactionStatus = case (tstat, bps) of
                (Nothing, _) -> TSAbsent
                (Just (_, True), _) -> TSFinalized
                (_, []) -> TSPending
                _ -> TSCommitted
        unless (hookTransactionStatus == TSFinalized && null bps) $ do
            hooks . hooksPSQ .= PSQ.insert th hookExpires (Hook $ fst <$> bps) psq0
            logEvent Skov LLTrace $ "Installed hook for transaction " ++ show th ++ " (expires " ++ show hookExpires ++ ")"
        let hookTransactionResults = bps
        return HookResult{..}
