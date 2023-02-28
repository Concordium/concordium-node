{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- |Consensus V1
module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import Data.Maybe (fromMaybe, isJust)

import qualified Data.Vector as Vector
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Types.Transactions
import Concordium.Types.Updates (uiHeader, uiPayload, updateType)
import Concordium.Utils

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.TransactionTable
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TransactionVerifier
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Scheduler.Types (updateSeqNumber)
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer

-- |Result of attempting to put a
-- 'BlockItem' into tree state.
data PutBlockItemResult
    = -- |The transaction was accepted into
      -- the tree state.
      Accepted
    | -- |The transaction was rejected. See
      -- the 'TVer.VerificationResult' for why it
      -- was rejected.
      Rejected !TVer.VerificationResult
    | -- |The transaction was already present
      -- in the tree state.
      Duplicate
    | -- |The transaction nonce was obsolete,
      -- i.e. inferior to the next available nonce.
      OldNonce
    deriving (Eq, Show)

-- |Puts a transaction into the pending transaction table
-- if it's eligible.
-- A transaction is eligible if the nonce of the transaction is at least
-- as high as the next available nonce observed with respect to the focus block.
--
-- We always check that the nonce of the transaction (if it has origin 'Block') is
-- at least what is recorded with respect to next available nonce for the
-- account. This is not necessary for transactions received individually
-- as they have already been pre-verified, and they will get rejected if they
-- do not yield exactly the recorded next available nonce.
--
-- This is an internal function only and should not be called directly.
putPendingTransaction ::
    ( MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- |Origin of the transaction.
    TransactionOrigin ->
    -- |The transaction.
    BlockItem ->
    m ()
putPendingTransaction Block bi = do
    case wmdData bi of
        NormalTransaction tx -> do
            fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
            macct <- getAccount fbState $! transactionSender tx
            nextNonce <- fromMaybe minNonce <$> mapM (getAccountNonce . snd) macct
            when (nextNonce <= transactionNonce tx) $ do
                pendingTransactionTable %=! addPendingTransaction nextNonce tx
                doPurgeTransactionTable False =<< currentTime
        CredentialDeployment _ -> do
            pendingTransactionTable %=! addPendingDeployCredential txHash
            doPurgeTransactionTable False =<< currentTime
        ChainUpdate cu -> do
            fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
            nextSN <- getNextUpdateSequenceNumber fbState (updateType (uiPayload cu))
            when (nextSN <= updateSeqNumber (uiHeader cu)) $ do
                pendingTransactionTable %=! addPendingUpdate nextSN cu
                doPurgeTransactionTable False =<< currentTime
  where
    txHash = getHash bi
putPendingTransaction Individual bi = do
    case wmdData bi of
        NormalTransaction tx -> do
            pendingTransactionTable %=! addPendingTransaction (transactionNonce tx) tx
            doPurgeTransactionTable False =<< currentTime
        CredentialDeployment _ -> do
            pendingTransactionTable %=! addPendingDeployCredential txHash
            doPurgeTransactionTable False =<< currentTime
        ChainUpdate cu -> do
            pendingTransactionTable %=! addPendingUpdate (updateSeqNumber (uiHeader cu)) cu
            doPurgeTransactionTable False =<< currentTime
  where
    txHash = getHash bi

-- |Attempt to put the 'BlockItem' into the tree state.
-- If the the 'BlockItem' was successfully added then it will be
-- in 'Received' state where the associated 'CommitPoint' will be set to zero.
-- Return the resulting 'PutBlockItemResult'.
processBlockItem ::
    ( MonadProtocolVersion m,
      IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- |The transaction we want to put into the state.
    BlockItem ->
    -- |Result of the put operation.
    m PutBlockItemResult
processBlockItem bi = do
    -- First we check whether the transaction already exists in the transaction table.
    tt' <- gets' _transactionTable
    if isDuplicate tt'
        then return Duplicate
        else do
            -- The transaction is new to us. Before adding it to the transaction table,
            -- we verify it.
            theTime <- utcTimeToTimestamp <$> currentTime
            verRes <- runTransactionVerifierT (TVer.verify theTime bi) =<< getCtx
            case verRes of
                okRes@(TVer.Ok _) -> do
                    added <- doAddTransaction 0 bi okRes
                    if added
                        then do
                            putPendingTransaction Individual bi
                            return Accepted
                        else -- If the transaction was not added it means it contained an old nonce.
                            return OldNonce
                notAccepted -> return $! Rejected notAccepted
  where
    -- Create a context suitable for verifying a transaction within a 'Individual' context.
    getCtx = do
        _ctxSkovData <- get
        _ctxBlockState <- bpState <$> gets' _lastFinalized
        return $! Context{_ctxTransactionOrigin = Individual, ..}
    isDuplicate tt = isJust $! tt ^. ttHashMap . at' txHash
    txHash = getHash bi

-- |Attempt to put the 'BlockItem's of a 'BakedBlock' into the tree state.
-- Return 'True' of the transactions were added otherwise 'False'.
--
-- Post-condition: Only transactions that are deemed verifiable
-- (i.e. the verification yields a 'TVer.OkResult' or a 'TVer.MaybeOkResult') up to the point where
-- a transaction processing might fail are added to the tree state.
-- Hence if 'False' is returned then the caller *must* roll back the state.
-- This is an internal function and should only be called when processing a block.
processBlockItems ::
    ( MonadProtocolVersion m,
      IsConsensusV1 pv,
      MonadState (SkovData pv) m,
      BlockStateQuery m,
      TimeMonad m,
      MPV m ~ pv,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- |Pointer to the parent block.
    BlockPointer pv ->
    -- |The baked block
    BakedBlock ->
    -- |Return 'True' if all transactions were
    -- successfully processed.
    m Bool
processBlockItems parentPointer bb = processBis $! bbTransactions bb
  where
    -- Create a context suitable for verifying a transaction within a 'Block' context.
    getCtx = do
        _ctxSkovData <- get
        return $! Context{_ctxTransactionOrigin = Block, _ctxBlockState = bpState parentPointer, ..}
    processBis !txs
        -- If no transactions are present we return 'True'.
        | Vector.length txs == 0 = return True
        -- There's work to do.
        | otherwise = snd <$> process txs True
    theRound = bbRound bb
    process !txs !res
        | Vector.length txs == 0 = return (Vector.empty, res)
        | otherwise = do
            !theTime <- utcTimeToTimestamp <$> currentTime
            let !bi = Vector.head txs
                !txHash = getHash bi
            !tt' <- gets' _transactionTable
            -- Check whether we already have the transaction.
            case tt' ^. ttHashMap . at' txHash of
                Just (_, results) -> do
                    -- If we have received the tranaction before we update the maximum committed round
                    -- if the new round is higher.
                    when (commitPoint theRound > results ^. tsCommitPoint) $
                        transactionTable . ttHashMap . at' txHash . mapped . _2 %=! updateSlot theRound
                    -- And we continue processing the remaining transactions.
                    process (Vector.tail txs) True
                Nothing -> do
                    -- We verify the transaction and check whether it's acceptable i.e. Ok or MaybeOk.
                    -- If that is the case then we add it to the transaction table and pending transactions.
                    -- If it is NotOk then we stop verifying the transactions as the block can never be valid now.
                    !verRes <- runTransactionVerifierT (TVer.verify theTime bi) =<< getCtx
                    -- Continue processing the transactions.
                    -- If the transaction was *not* added then it means that it yields a lower nonce with
                    -- respect to the non finalized transactions. We tolerate this and keep processing the remaining transactions
                    -- of the block as it could be the case that we have received other transactions from the given account via other blocks.
                    -- We only add the transaction to the pending transaction table if its nonce is at least the next available nonce for the
                    -- account.
                    let continue !added =
                            if not added
                                then process (Vector.tail txs) True
                                else putPendingTransaction Block bi >> process (Vector.tail txs) True
                    case verRes of
                        -- The transaction was deemed non verifiable i.e., it can never be
                        -- valid. We short circuit the recursion here and return 'False'.
                        (TVer.NotOk _) -> return (Vector.empty, False)
                        -- The transaction is either 'Ok' or 'MaybeOk' and that is acceptable
                        -- when processing transactions which originates from a block.
                        -- We add it to the transaction table and continue with the next transaction.
                        acceptedRes -> doAddTransaction theRound bi acceptedRes >>= continue
