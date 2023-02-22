{-# LANGUAGE TypeFamilies #-}

-- |Consensus V1
module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import qualified Data.ByteString as BS
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

class MonadMulticast m where
    sendMessage :: BS.ByteString -> m ()

-- uponTimeoutEvent :: ()
-- uponTimeoutEvent = ()

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

-- |Attempt to put the 'BlockItem' into the tree state.
-- If the the 'BlockItem' was successfully added then it will be
-- in 'Received' state where the associated 'CommitPoint' will be set to zero.
-- Return the resulting 'PutBlockItemResult'.
putBlockItem ::
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
putBlockItem bi = do
    -- First we check whether the transaction already exists in the transaction table.
    tt' <- gets' _transactionTable
    if isJust $! tt' ^. ttHashMap . at' txHash
        then -- The transaction is already present so we do nothing and simply
        -- returns the fact that it is a duplicate.
            return Duplicate
        else do
            -- The transaction is new to us. Before adding it to the transaction table,
            -- we verify it.
            theTime <- utcTimeToTimestamp <$> currentTime
            ctx <- getCtx
            verRes <- runTransactionVerifierT (TVer.verify theTime bi) ctx
            case verRes of
                okRes@(TVer.Ok _) -> do
                    added <- doAddTransaction 0 bi okRes
                    if added
                        then do
                            -- The transaction was added and we record this in the pending transactions table
                            -- if the transaction nonce is at least the next available nonce from the perspective
                            -- of the focus block.
                            case wmdData bi of
                                NormalTransaction tx -> do
                                    fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
                                    macct <- getAccount fbState $! transactionSender tx
                                    nextNonce <- fromMaybe minNonce <$> mapM (getAccountNonce . snd) macct
                                    when (nextNonce <= transactionNonce tx) $ do
                                        pendingTransactionTable %=! addPendingTransaction nextNonce tx
                                        doPurgeTransactionTable False =<< currentTime
                                    return Accepted
                                CredentialDeployment _ -> do
                                    pendingTransactionTable %=! addPendingDeployCredential txHash
                                    doPurgeTransactionTable False =<< currentTime
                                    return Accepted
                                ChainUpdate cu -> do
                                    fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
                                    nextSN <- getNextUpdateSequenceNumber fbState (updateType (uiPayload cu))
                                    when (nextSN <= updateSeqNumber (uiHeader cu)) $ do
                                        pendingTransactionTable %=! addPendingUpdate nextSN cu
                                        doPurgeTransactionTable False =<< currentTime
                                    return Accepted
                        else -- If the transaction was not added it means it contained an old nonce.
                            return OldNonce
                notAccepted -> return $! Rejected notAccepted
  where
    -- The transaction hash.
    txHash = getHash bi
    -- Create the context for verifying the transaction within.
    getCtx = do
        _ctxSkovData <- get
        _ctxBlockState <- bpState <$> gets' _lastFinalized
        return $! Context{_ctxTransactionOrigin = Individual, ..}

-- |Attempt to put the 'BlockItem's of a 'BakedBlock' into the tree state.
-- Return 'True' of the transactions were added otherwise 'False'.
--
-- Post-condition: The transactions are only added to the tree state if they could
-- *all* be deemed verifiable i.e. the verification of each transaction either yields a
-- 'TVer.OkResult' or a 'TVer.MaybeOkResult'.
putBlockItems ::
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
putBlockItems parentPointer bb = processBis $! bbTransactions bb
  where
    getCtx = do
        _ctxSkovData <- get
        return $! Context{_ctxTransactionOrigin = Block, _ctxBlockState = bpState parentPointer, ..}
    processBis txs
        -- If no transactions are present we return 'True'.
        | Vector.length txs == 0 = return True
        -- There's work to do.
        | otherwise = snd <$> process txs True
    theRound = bbRound bb
    process _ False = return (Vector.empty, False)
    process txs True = do
        ctx <- getCtx
        theTime <- utcTimeToTimestamp <$> currentTime
        let bi = Vector.head txs
        tt' <- gets' _transactionTable
        -- Check whether we already have the transaction.
        if isJust $! tt' ^. ttHashMap . at' (getHash bi)
            then -- We already have the transaction so we proceed to the next one.
                process (Vector.tail txs) True
            else do
                -- We verify the transaction and check whether it's acceptable i.e. Ok or MaybeOk.
                -- If that is the case then we add it to the transaction table and pending transactions.
                -- If it is NotOk then we stop verifying the transactions as the block can never be valid now.
                verRes <- runTransactionVerifierT (TVer.verify theTime bi) ctx
                case verRes of
                    (TVer.NotOk _) -> return (Vector.empty, False)
                    acceptedRes -> do
                        added <- doAddTransaction theRound bi acceptedRes
                        if not added
                            then -- The transaction was not added meaning it yields a lower nonce with respect
                            -- to the non finalized transactions. We tolerate this and keep processing transactions from the
                            -- block as it could be the case that we have received other transactions from the account by other blocks.
                                process (Vector.tail txs) True
                            else do
                                case wmdData bi of
                                    NormalTransaction tx -> do
                                        fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
                                        macct <- getAccount fbState $! transactionSender tx
                                        nextNonce <- fromMaybe minNonce <$> mapM (getAccountNonce . snd) macct
                                        when (nextNonce <= transactionNonce tx) $ do
                                            pendingTransactionTable %=! addPendingTransaction nextNonce tx
                                            doPurgeTransactionTable False =<< currentTime
                                        process (Vector.tail txs) True
                                    CredentialDeployment _ -> do
                                        pendingTransactionTable %=! addPendingDeployCredential (getHash bi)
                                        doPurgeTransactionTable False =<< currentTime
                                        process (Vector.tail txs) True
                                    ChainUpdate cu -> do
                                        fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
                                        nextSN <- getNextUpdateSequenceNumber fbState (updateType (uiPayload cu))
                                        when (nextSN <= updateSeqNumber (uiHeader cu)) $ do
                                            pendingTransactionTable %=! addPendingUpdate nextSN cu
                                            doPurgeTransactionTable False =<< currentTime
                                        process (Vector.tail txs) True
