{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.PurgeTransactions where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.HashSet as HS
import Data.Semigroup
import Control.Monad.Trans.State.Strict
import Lens.Micro.Platform

import Concordium.Utils
import Concordium.Types.Transactions
import Concordium.Types

import Concordium.GlobalState.TransactionTable

type TransactionHashTable = HM.HashMap TransactionHash (BlockItem, TransactionStatus)

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
-- `_tsSlot` field is less than or equal to the slot of the last finalized block.
--
-- Transactions are only purged if they have expired (so they would not be valid
-- in any block created from now on) or if they arrived sufficiently long ago
-- (where sufficiently long ago should be determined by subtracting the keep-alive
-- time from the current time).
purgeTables
    :: Slot
    -- ^Slot of last finalized block
    -> TransactionTime
    -- ^Oldest permitted transaction arrival time
    -> Timestamp
    -- ^Current time
    -> TransactionTable
    -- ^Transaction table to purge
    -> PendingTransactionTable
    -- ^Pending transaction table to purge
    -> (TransactionTable, PendingTransactionTable)
purgeTables lastFinSlot oldestArrivalTime currentTime TransactionTable{..} ptable = (ttable', ptable')
    where
        -- A transaction is too old if its arrival predates the oldest allowed
        -- arrival time, or if its expiry time has passed.
        tooOld tx = biArrivalTime tx < oldestArrivalTime
                    || transactionExpired (thExpiry (btrHeader (wmdData tx))) currentTime
        removable (Just (_, Received{..})) = _tsSlot <= lastFinSlot
        removable (Just (_, Committed{..})) = _tsSlot <= lastFinSlot
        removable _ = False
        purgeTxs :: Nonce -> Set.Set Transaction -> State (Maybe (Max Nonce), TransactionHashTable) (Maybe (Set.Set Transaction))
        purgeTxs n ts = do
            (mmnonce, tht) <- get
            let
                purgeTx tx (tsacc, thtacc)
                    | tooOld tx
                    , removable (thtacc ^? ix (biHash tx))
                        = (tsacc, HM.delete (biHash tx) thtacc)
                    | otherwise
                        = (tx : tsacc, thtacc)
                (tsl', tht') = foldr purgeTx ([], tht) (Set.toAscList ts)
                ts' = Set.fromAscList tsl'
                (mmnonce', mres)
                    | null tsl' = (mmnonce, Nothing)
                    | otherwise = (mmnonce <> Just (Max n), Just ts')
            put (mmnonce', tht')
            return mres
        purgeAccount :: AccountAddress -> AccountNonFinalizedTransactions -> State (PendingTransactionTable, TransactionHashTable) AccountNonFinalizedTransactions
        purgeAccount addr AccountNonFinalizedTransactions{..} = do
            (ptt0, trs0) <- get
            let (newANFTMap, (mmax, !trs1)) = runState (Map.traverseMaybeWithKey purgeTxs _anftMap) (Nothing, trs0)
            let updptt (Just (Max newHigh)) (Just (low, _))
                    | newHigh < low = Nothing
                    | otherwise = Just (low, newHigh)
                updptt _ _ = Nothing
                !ptt1 = ptt0 & pttWithSender . at' addr %~ updptt mmax
            put (ptt1, trs1)
            return AccountNonFinalizedTransactions{_anftMap = newANFTMap, ..}
        purgeDeployCredentials = do
            dc0 <- use (_1 . pttDeployCredential)
            trs0 <- use _2
            let
                p Nothing = Nothing
                p r@(Just (bi, _))
                    | biArrivalTime bi < oldestArrivalTime
                    , removable r
                        = Nothing
                    | otherwise
                        = r
                purgeDC (dc, trs) cdihash = case trs & at cdihash <%~ p of
                    (Nothing, trs') -> (HS.delete cdihash dc, trs')
                    _ -> (dc, trs)
                (dc1, trs1) = HS.foldl' purgeDC (dc0, trs0) dc0
            _1 . pttDeployCredential .= dc1
            _2 .= trs1
        purge = do
            -- Purge each account
            nnft <- HM.traverseWithKey purgeAccount _ttNonFinalizedTransactions
            -- Purge credential deployments
            purgeDeployCredentials
            return nnft

        (newNFT, (ptable', finalTT)) = runState purge (ptable, _ttHashMap)
        ttable' = TransactionTable{
            _ttHashMap = finalTT,
            _ttNonFinalizedTransactions = newNFT
        }
        
