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
import Concordium.Scheduler (FilteredTransactions (..))
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types.HashableTo

-- |Transaction hash table. One component of the 'TransactionTable'.
type TransactionHashTable = HM.HashMap TransactionHash (BlockItem, LiveTransactionStatus)

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
    -- slot precedes the last finalized slot.
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
    purgeTxs :: Nonce -> Map.Map Transaction TVer.VerificationResult -> State (Maybe (Max Nonce), TransactionHashTable) (Maybe (Map.Map Transaction TVer.VerificationResult))
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
    purgeAccount :: AccountAddressEq -> AccountNonFinalizedTransactions -> State (PendingTransactionTable, TransactionHashTable) AccountNonFinalizedTransactions
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
        State (Maybe (Max UpdateSequenceNumber), TransactionHashTable) (Maybe (Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult))
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
    purgeUpdates :: UpdateType -> NonFinalizedChainUpdates -> State (PendingTransactionTable, TransactionHashTable) NonFinalizedChainUpdates
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

-- |Update the transaction table and pending transaction table as a result of constructing a block.
-- The transactions added to the block are marked as committed.
-- Failed transactions are purged from the transaction table if they are not committed since the
-- last finalized block. Unprocessed account transactions are similarly purged if they are at least
-- the supplied maximum block size and not already committed.
-- The pending transaction table is updated as by executing the block (with 'forwardPTT'), except
-- that the pending account transactions are created anew. This is done by adding all account
-- transactions that were neither added as part of the block nor (successfully) purged for failing
-- or being too large to the pending transaction table, after the pending account transactions are
-- cleared.
--
-- PRECONDITION: All of the filtered transactions are present in the transaction table and pending
-- transaction table. When an account transaction is added, any other account transactions from
-- the same account with the same nonce are failed. When a chain update is added, any other chain
-- updates of the same type with the same sequence number are failed. (Note: @filterTransactions@
-- should ensure this.)
filterTables ::
    (IsCommitPoint cp) =>
    -- |'CommitPoint' of the last finalized block
    cp ->
    -- |Maximum block size.
    -- Unprocessed transactions are only retained if they are smaller than this.
    Int ->
    -- |'CommitPoint' of block that transactions were added in
    cp ->
    -- |'BlockHash' of block that transactions were added in
    BlockHash ->
    -- |Filtered transactions as a result of constructing the block.
    FilteredTransactions ->
    -- |Transaction table to update
    TransactionTable ->
    -- |Pending transaction table to update
    PendingTransactionTable ->
    (TransactionTable, PendingTransactionTable)
filterTables lastFinCommit maxSize commitAt commitBlock FilteredTransactions{..} tt0 ptt0 =
    restoreUnprocessedUpdates
        . filterFailedUpdates
        . restoreUnprocessedCredentials
        . filterFailedCredentials
        . restoreUnprocessedTransactions
        . filterFailedTransactions
        $ (commitAdded tt0, emptyPendingTransactionTable)
  where
    addedTransactions = fst . fst <$> ftAdded
    -- Mark added transactions as committed
    commitAdded = commitAdded' 0 addedTransactions
    commitAdded' _ [] tt = tt
    commitAdded' i (bi : bis) tt =
        commitAdded' (i + 1) bis $!
            tt & ttHashMap . ix (getHash bi) . _2 %~ addResult commitBlock commitAt i
    forwarded = forwardPTT addedTransactions ptt0
    nextNonce acct = forwarded ^? pttWithSender . ix acct . _1
    nextSeqNum uty = forwarded ^? pttUpdates . ix uty . _1

    purgeTransaction t =
        ( ttHashMap . at' (wmdHash t)
            .~ Nothing
        )
            . ( ttNonFinalizedTransactions
                    . at' sender
                    . non emptyANFT
                    . anftMap
                    . at' nonce
                    . non Map.empty
                    %~ Map.delete t
              )
      where
        sender = accountAddressEmbed (transactionSender t)
        nonce = transactionNonce t
    maintainTransaction t ptt = case nextNonce (accountAddressEmbed (transactionSender t)) of
        Just nonce
            | nonce <= transactionNonce t -> addPendingTransaction nonce t ptt
        -- nextNonce should only return 'Nothing' if transactions up to the maximum nonce in the
        -- original pending transaction table have all been committed. Since we assume all
        -- transactions are accounted for in the pending table, we do not need to add an entry in
        -- this case as it would be duplicative.
        _ -> ptt
    maintainCredential = addPendingDeployCredential . wmdHash
    maintainUpdate upd ptt = case nextSeqNum updType of
        Just nextSN
            | nextSN <= seqNum -> addPendingUpdate nextSN (wmdData upd) ptt
        _ -> ptt
      where
        updType = updateType (uiPayload (wmdData upd))
        seqNum = updateSeqNumber (uiHeader (wmdData upd))
    purgeCredential cred = ttHashMap . at' (wmdHash cred) .~ Nothing
    purgeUpdate upd =
        (ttHashMap . at' (wmdHash upd) .~ Nothing)
            . ( ttNonFinalizedChainUpdates
                    . at' updType
                    . non emptyNFCU
                    . nfcuMap
                    . at' seqNum
                    . non Map.empty
                    %~ Map.delete upd
              )
      where
        updType = updateType (uiPayload (wmdData upd))
        seqNum = updateSeqNumber (uiHeader (wmdData upd))

    -- If a transaction is not committed, remove it from the transaction table with the supplied
    -- 'purge' function.
    -- If it is committed, maintain it in the pending transaction table with the supplied 'maintain'
    -- function.
    purgeOrMaintain ::
        (WithMetadata a -> TransactionTable -> TransactionTable) ->
        (WithMetadata a -> PendingTransactionTable -> PendingTransactionTable) ->
        WithMetadata a ->
        (TransactionTable, PendingTransactionTable) ->
        (TransactionTable, PendingTransactionTable)
    purgeOrMaintain purge maintain t (tt, ptt) =
        case tt ^? ttHashMap . ix (wmdHash t) . _2 . tsCommitPoint of
            Nothing -> (tt, ptt) -- Transaction is already absent
            Just transactionCP
                | commitPoint lastFinCommit >= commitPoint transactionCP ->
                    -- Transaction is not committed (since the last finalized block), so purge
                    let !tt' = purge t tt in (tt', ptt)
                | otherwise ->
                    -- Transaction is committed, so maintain it in the pending table.
                    let !ptt' = maintain t ptt in (tt, ptt')

    filterFailedTransactions tables = foldl' f tables ftFailed
      where
        f (tt, ptt) ((t, _), _) = purgeOrMaintain purgeTransaction maintainTransaction t (tt, ptt)

    restoreUnprocessedTransactions tables = foldl' f tables ftUnprocessed
      where
        f (tt, ptt) (t, _) =
            if transactionSize t < maxSize
                then let !ptt' = maintainTransaction t ptt in (tt, ptt')
                else purgeOrMaintain purgeTransaction maintainTransaction t (tt, ptt)

    filterFailedCredentials tables = foldl' f tables ftFailedCredentials
      where
        f (tt, ptt) ((cred, _), _) = purgeOrMaintain purgeCredential maintainCredential cred (tt, ptt)

    restoreUnprocessedCredentials (tt, ptt) = (tt, ptt')
      where
        !ptt' = foldl' (flip (maintainCredential . fst)) ptt ftUnprocessedCredentials

    filterFailedUpdates tables = foldl' f tables ftFailedUpdates
      where
        f (tt, ptt) ((upd, _), _) = purgeOrMaintain purgeUpdate maintainUpdate upd (tt, ptt)

    restoreUnprocessedUpdates tables = foldl' f tables ftUnprocessedUpdates
      where
        f (tt, ptt) (upd, _) =
            if wmdSize upd < maxSize
                then let !ptt' = maintainUpdate upd ptt in (tt, ptt')
                else purgeOrMaintain purgeUpdate maintainUpdate upd (tt, ptt)
