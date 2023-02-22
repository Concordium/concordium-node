{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)

import Lens.Micro.Platform

import Concordium.Scheduler.Types (updateSeqNumber)
import Concordium.TimeMonad
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
    BlockItem ->
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
putBlockItems :: (MonadState (SkovData pv) m) => BakedBlock -> m Bool
putBlockItems = undefined
