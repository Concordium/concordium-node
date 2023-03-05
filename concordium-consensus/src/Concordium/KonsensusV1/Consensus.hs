{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |Consensus V1
module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import Data.Maybe (fromMaybe)

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
import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Scheduler.Types (updateSeqNumber)
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer

-- |A Monad for multicasting timeout messages.
class MonadMulticast m where
    -- |Multicast a timeout message over the network
    sendTimeoutMessage :: TimeoutMessage -> m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: BakerIdentity
    }
makeClassy ''BakerContext

-- |Adds a transaction into the pending transaction table
-- if it's eligible.
--
-- Pre condition: A transaction must've been pre-verified prior to being called here.
--
-- Transactions received individually are always added to the pending transactions as
-- it is checked that the transaction nonce is at least what is recorded for the focus block.
-- (That is a pre condition of this function)
--
-- This ensures the invariant of the pending transaction table and the focus block.
-- Namely that the recorded next availble nonce with respect to the pending transaction table
-- must be the same of what is recorded in the focus block.
--
-- For transactions received as part of a block we must check that the transaction nonce
-- is at least what the next available nonce recorded in the focus block before adding
-- it to the pending transaction table.
--
-- This is to ensure the above mentioned invariant of the pending transaction table and focus block
-- as to make sure that if the parent block we verified the transaction within is above the focus block
-- then we need to record this fact in the pending transaction table as the transaction nonce
-- would very likely be above what is recorded in the focus block.
--
-- This is an internal function only and should not be called directly.
addPendingTransaction ::
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
addPendingTransaction origin bi = do
    case wmdData bi of
        NormalTransaction tx -> do
            fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
            macct <- getAccount fbState $! transactionSender tx
            nextNonce <- fromMaybe minNonce <$> mapM (getAccountNonce . snd) macct
            when (nextNonce <= transactionNonce tx || origin == Individual) $ do
                pendingTransactionTable %=! TT.addPendingTransaction nextNonce tx
                doPurgeTransactionTable False =<< currentTime
        CredentialDeployment _ -> do
            pendingTransactionTable %=! TT.addPendingDeployCredential txHash
            doPurgeTransactionTable False =<< currentTime
        ChainUpdate cu -> do
            fbState <- bpState <$> (_focusBlock <$> gets' _skovPendingTransactions)
            nextSN <- getNextUpdateSequenceNumber fbState (updateType (uiPayload cu))
            when (nextSN <= updateSeqNumber (uiHeader cu) || origin == Individual) $ do
                pendingTransactionTable %=! TT.addPendingUpdate nextSN cu
                doPurgeTransactionTable False =<< currentTime
  where
    txHash = getHash bi

-- |Attempt to put the 'BlockItem' into the tree state.
-- If the the 'BlockItem' was successfully added then it will be
-- in 'Received' state where the associated 'CommitPoint' will be set to zero.
-- Return the resulting 'AddBlockItemResult'.
processBlockItem ::
    ( MonadProtocolVersion m,
      IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      BlockStateQuery m,
      AccountNonceQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- |The transaction we want to put into the state.
    BlockItem ->
    -- |Whether it was @Accepted@, @Rejected@, @Duplicate@ or @Obsolete@.
    m AddTransactionResult
processBlockItem bi = do
    -- First we check whether the transaction already exists in the transaction table.
    tt <- use' transactionTable
    case tt ^. TT.ttHashMap . at' txHash of
        Just (duplicateTransaction, dupStatus) -> return $! Duplicate duplicateTransaction (Just $! dupStatus ^. TT.tsVerRes)
        Nothing -> do
            -- The transaction is new to us. Before adding it to the transaction table,
            -- we verify it.
            theTime <- utcTimeToTimestamp <$> currentTime
            verRes <- runTransactionVerifierT (TVer.verify theTime bi) =<< getCtx
            case verRes of
                (TVer.Ok res) -> insertTransaction res
                notAccepted -> return $! NotAdded notAccepted
  where
    -- Insert the transaction into the transaction table and pending transaction table.
    insertTransaction okRes = do
        added <- doAddTransaction 0 bi $! TVer.Ok okRes
        if added
            then do
                addPendingTransaction Individual bi
                return $! Added bi $! TVer.Ok okRes
            else -- If the transaction was not added it means it contained an old nonce.
                return ObsoleteNonce
    -- Create a context suitable for verifying a transaction within a 'Individual' context.
    getCtx = do
        _ctxSkovData <- get
        _ctxBs <- bpState <$> gets' _lastFinalized
        chainParams <- Concordium.GlobalState.BlockState.getChainParameters _ctxBs
        let _ctxMaxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit
        return $! Context{_ctxTransactionOrigin = Individual, ..}
    -- 'TransactionHash' of the transaction we're processing.
    txHash = getHash bi

-- |Attempt to put the 'BlockItem's of a 'BakedBlock' into the tree state.
-- Return 'True' of the transactions were added otherwise 'False'.
--
-- Post-condition: Only transactions that are deemed verifiable
-- (i.e. the verification yields a 'TVer.OkResult' or a 'TVer.MaybeOkResult') up to the point where
-- a transaction processing might fail are added to the tree state.
processBlockItems ::
    forall m pv.
    ( MonadProtocolVersion m,
      IsConsensusV1 pv,
      MonadState (SkovData pv) m,
      BlockStateQuery m,
      TimeMonad m,
      AccountNonceQuery m,
      MPV m ~ pv,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- |The baked block
    BakedBlock ->
    -- |Pointer to the parent block.
    BlockPointer pv ->
    -- |Return 'True' only if all transactions were
    -- successfully processed otherwise 'False'.
    m Bool
processBlockItems bb parentPointer = process True $! bbTransactions bb
  where
    -- Create a context suitable for verifying a transaction within a 'Block' context.
    getCtx = do
        _ctxSkovData <- get
        let _ctxBs = bpState parentPointer
        chainParams <- Concordium.GlobalState.BlockState.getChainParameters _ctxBs
        let _ctxMaxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit
        return $! Context{_ctxTransactionOrigin = Block, ..}
    theRound = bbRound bb
    -- Process the vector of transactions recursively.
    process :: Bool -> Vector.Vector BlockItem -> m Bool
    process !res !txs
        -- If no transactions are present then all were added.
        | Vector.length txs == 0 = return res
        -- There's work to do.
        | otherwise = do
            !theTime <- utcTimeToTimestamp <$> currentTime
            let !bi = Vector.head txs
                !txHash = getHash bi
            !tt' <- gets' _transactionTable
            -- Check whether we already have the transaction.
            case tt' ^. TT.ttHashMap . at' txHash of
                Just (_, results) -> do
                    -- If we have received the transaction before we update the maximum committed round
                    -- if the new round is higher.
                    when (TT.commitPoint theRound > results ^. TT.tsCommitPoint) $
                        transactionTable . TT.ttHashMap . at' txHash . mapped . _2 %=! TT.updateSlot theRound
                    -- And we continue processing the remaining transactions.
                    process True (Vector.tail txs)
                Nothing -> do
                    -- We verify the transaction and check whether it's acceptable i.e. Ok or MaybeOk.
                    -- If that is the case then we add it to the transaction table and pending transactions.
                    -- If it is NotOk then we stop verifying the transactions as the block can never be valid now.
                    !verRes <- runTransactionVerifierT (TVer.verify theTime bi) =<< getCtx
                    case verRes of
                        -- The transaction was deemed non verifiable i.e., it can never be
                        -- valid. We short circuit the recursion here and return 'False'.
                        (TVer.NotOk _) -> return False
                        -- The transaction is either 'Ok' or 'MaybeOk' and that is acceptable
                        -- when processing transactions which originates from a block.
                        -- We add it to the transaction table and continue with the next transaction.
                        acceptedRes ->
                            doAddTransaction theRound bi acceptedRes >>= \case
                                -- The transaction was obsolete so we stop processing the remaining transactions.
                                False -> return False
                                -- The transaction was added to the tree state,
                                -- so add it to the pending table if it's eligible (see documentation for
                                -- 'addPendingTransaction') and continue processing the remaining ones.
                                True -> addPendingTransaction Block bi >> process True (Vector.tail txs)

