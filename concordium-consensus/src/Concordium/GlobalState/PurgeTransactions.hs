{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.PurgeTransactions where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Data.Semigroup
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.Utils

import Concordium.GlobalState.TransactionTable
import qualified Concordium.TransactionVerification as TVer

-- |Transaction hash table parameterized by @a@ which is an instance of 'Ord'.
type TransactionHashTable a = HM.HashMap TransactionHash (BlockItem, TransactionStatus)

-- |Purge transactions that are not present in any live or finalized blocks
-- and either have expired or were received before the oldest permitted arrival
-- time.
--
--  1. Traverse the account non-finalized transaction table. For each account:
--
--     a) Iterate over the nonces.  For each transaction, check
--        if it can be purged, removing it from the set and the transaction table
--        if so.  If the set at a nonce ends up empty, remove the entry in the
--        non-finalized transaction table.
--
--     b) Determine the maximal non-empty nonce.  If there is an entry in the
--        pending transaction table for the account, update it by either
--        removing the entry (if the new max nonce is lower than the old low value)
--        or updating the high nonce to the new max nonce (otherwise).
--
--  2. For each credential deployment in the pending transaction table, if it can
--     be purged, remove it from the set and the transaction table.
--
-- A transaction is eligible for purging only if it does not belong to any
-- current blocks. This is considered to be the case if the corresponding
-- entry in the transaction table is a `Received` or `Committed` and the
-- `_tsCommitPoint` field is less than or equal to the slot of the last finalized block.
--
-- Transactions are only purged if they have expired (so they would not be valid
-- in any block created from now on) or if they arrived sufficiently long ago
-- (where sufficiently long ago should be determined by subtracting the keep-alive
-- time from the current time).
purgeTables ::
    -- |'CommitPoint' of last finalized block
    CommitPoint ->
    -- |Oldest permitted transaction arrival time
    TransactionTime ->
    -- |Current time
    Timestamp ->
    -- |Transaction table to purge
    TransactionTable ->
    -- |Pending transaction table to purge
    PendingTransactionTable ->
    (TransactionTable, PendingTransactionTable)
purgeTables lastFinCommitPoint oldestArrivalTime currentTime TransactionTable{..} ptable = (ttable', ptable')
  where
    -- A transaction is too old if its arrival predates the oldest allowed
    -- arrival time, or if its expiry time has passed.
    tooOld :: (BIMetadata a, HasMessageExpiry a) => a -> Bool
    tooOld tx = biArrivalTime tx < oldestArrivalTime || transactionExpired (msgExpiry tx) currentTime
    -- Determine if an entry in the transaction hash table indicates that a
    -- transaction is eligible for removal.  This is the case if the recorded
    -- slot preceeds the last finalized slot.
    removable (Just (_, Received{..})) = _tsCommitPoint <= lastFinCommitPoint
    removable (Just (_, Committed{..})) = _tsCommitPoint <= lastFinCommitPoint
    -- This case should not occur, since it would mean that a transaction we
    -- are trying to remove is either finalized or unknown.
    removable _ = False
    -- Purge the set of pending transactions at a given nonce.  The state
    -- tracks the maximum nonce (if any) for which there are still pending
    -- transactions and the transaction hash table, from which transactions
    -- are purged.  The return value is the updated set of transactions, or
    -- @Nothing@ if all transactions at this nonce have been purged.
    purgeTxs :: Nonce -> Map.Map Transaction TVer.VerificationResult -> State (Maybe (Max Nonce), TransactionHashTable o) (Maybe (Map.Map Transaction TVer.VerificationResult))
    purgeTxs n ts = do
        (mmnonce, tht) <- get
        let
            -- Remove a transaction if it is too old and removable.
            -- Transactions that are not removed are accumulated.
            purgeTx (tsacc, thtacc) txAndVerRes@(tx, _)
                | tooOld tx,
                  removable (thtacc ^? ix (biHash tx)) =
                    (tsacc, HM.delete (biHash tx) thtacc)
                | otherwise =
                    (txAndVerRes : tsacc, thtacc)
            (tsl', tht') = foldl' purgeTx ([], tht) (Map.toDescList ts)
            -- Since we start with the set in descending order and foldl',
            -- the result will be a list in ascending order.
            ts' = Map.fromAscList tsl'
            (!mmnonce', !mres)
                -- No transactions left, so remove the set and the max nonce doesn't change
                | null tsl' = (mmnonce, Nothing)
                -- Some transactions left, so keep the updated set and update the max nonce.
                | otherwise = (mmnonce <> Just (Max n), Just ts')
        put (mmnonce', tht')
        return mres
    -- Purge the non-finalized transactions for a specific account.
    purgeAccount :: AccountAddressEq -> AccountNonFinalizedTransactions -> State (PendingTransactionTable, TransactionHashTable o) AccountNonFinalizedTransactions
    purgeAccount addr AccountNonFinalizedTransactions{..} = do
        (ptt0, trs0) <- get
        -- Purge the transactions from the transaction table.
        let (newANFTMap, (mmax, !trs1)) = runState (Map.traverseMaybeWithKey purgeTxs _anftMap) (Nothing, trs0)
        -- Update the pending transaction table.
        let updptt (Just (Max newHigh)) (Just (low, _))
                | newHigh < low = Nothing
                | otherwise = Just (low, newHigh)
            updptt _ _ = Nothing
            !ptt1 = ptt0 & pttWithSender . at' addr %~ updptt mmax
        put (ptt1, trs1)
        return AccountNonFinalizedTransactions{_anftMap = newANFTMap, ..}
    -- Purge the deploy credential transactions that are pending.
    purgeDeployCredentials = do
        dc0 <- use (_1 . pttDeployCredential)
        trs0 <- use _2
        let
            -- Remove entry from the transaction table if eligible
            p Nothing = Nothing
            p r@(Just (bi, _))
                | tooOld bi,
                  removable r =
                    Nothing
                | otherwise =
                    r
            -- Purge the hash from the transaction table and pending
            -- transaction table
            purgeDC (dc, trs) cdihash = case trs & at' cdihash <%~ p of
                -- The CDI is no longer in the transaction table, so delete it.
                (Nothing, trs') ->
                    (HS.delete cdihash dc, trs')
                -- The CDI was kept, so do nothing.
                _ ->
                    (dc, trs)
            -- Fold over the set of credential deployments and purge them
            (dc1, trs1) = HS.foldl' purgeDC (dc0, trs0) dc0
        _1 . pttDeployCredential .= dc1
        _2 .= trs1
    purgeUpds ::
        UpdateSequenceNumber ->
        Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult ->
        State (Maybe (Max UpdateSequenceNumber), TransactionHashTable o) (Maybe (Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult))
    purgeUpds sn uis = state $ \(mmsn, tht) ->
        let
            purgeUpd (uisacc, thtacc) uiAndVerRes@(ui, _)
                | tooOld ui,
                  removable (thtacc ^? ix (biHash ui)) =
                    (uisacc, HM.delete (biHash ui) thtacc)
                | otherwise =
                    (uiAndVerRes : uisacc, thtacc)
            (uisl', tht') = foldl' purgeUpd ([], tht) (Map.toDescList uis)
            (!mmsn', !mres)
                | null uisl' = (mmsn, Nothing)
                | otherwise = (mmsn <> Just (Max sn), Just (Map.fromDistinctAscList uisl'))
        in
            (mres, (mmsn', tht'))
    purgeUpdates :: UpdateType -> NonFinalizedChainUpdates -> State (PendingTransactionTable, TransactionHashTable o) NonFinalizedChainUpdates
    purgeUpdates uty nfcu@NonFinalizedChainUpdates{..} = state $ \(ptt0, trs0) ->
        let (newNFCUMap, (mmax, !uis1)) = runState (Map.traverseMaybeWithKey purgeUpds _nfcuMap) (Nothing, trs0)
            updptt (Just (Max newHigh)) (Just (low, _))
                | newHigh < low = Nothing
                | otherwise = Just (low, newHigh)
            updptt _ _ = Nothing
            !ptt1 = ptt0 & pttUpdates . at' uty %~ updptt mmax
        in  (nfcu{_nfcuMap = newNFCUMap}, (ptt1, uis1))
    purge = do
        -- Purge each account
        nnft <- HM.traverseWithKey purgeAccount _ttNonFinalizedTransactions
        -- Purge credential deployments
        purgeDeployCredentials
        -- Purge chain updates
        nnfcu <- Map.traverseWithKey purgeUpdates _ttNonFinalizedChainUpdates
        return (nnft, nnfcu)

    ((newNFT, newNFCU), (ptable', finalTT)) = runState purge (ptable, _ttHashMap)
    ttable' =
        TransactionTable
            { _ttHashMap = finalTT,
              _ttNonFinalizedTransactions = newNFT,
              _ttNonFinalizedChainUpdates = newNFCU
            }
