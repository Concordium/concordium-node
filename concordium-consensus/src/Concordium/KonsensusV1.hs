{-# LANGUAGE TypeFamilies #-}

-- |This module provides high-level entrypoints for the version 1 consensus.
module Concordium.KonsensusV1 where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Transactions as Transactions
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import qualified Concordium.KonsensusV1.Consensus.Quorum as Quorum
import qualified Concordium.KonsensusV1.Consensus.Timeout as Timeout
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Concordium.Skov.Monad (UpdateResult (..), transactionVerificationResultToUpdateResult)
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Types
import Concordium.Types.Parameters

-- |Handle receiving a finalization message (either a 'QuorumMessage' or a 'TimeoutMessage').
-- Returns @Left res@ in the event of a failure, with the appropriate failure code.
-- Otherwise, returns @Right followup@, in which case the message should be relayed to peers
-- and the @followup@ action invoked (while retaining the global state lock).
receiveFinalizationMessage ::
    ( IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      MonadProtocolVersion m,
      BlockStateStorage m,
      TimeMonad m,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m,
      MonadReader r m,
      HasBakerContext r,
      MonadConsensusEvent m,
      MonadLogger m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadTreeStateStore m,
      MonadBroadcast m,
      TimerMonad m
    ) =>
    FinalizationMessage ->
    m (Either UpdateResult (m ()))
receiveFinalizationMessage (FMQuorumMessage qm) = do
    res <- Quorum.receiveQuorumMessage qm =<< get
    case res of
        Quorum.Received vqm -> return $ Right $ Quorum.processQuorumMessage vqm makeBlock
        Quorum.ReceivedNoRelay vqm -> do
            Quorum.processQuorumMessage vqm makeBlock
            return $ Left ResultDuplicate
        Quorum.Rejected Quorum.Duplicate -> return $ Left ResultDuplicate
        Quorum.Rejected Quorum.ObsoleteRound -> return $ Left ResultStale
        Quorum.Rejected _ -> return $ Left ResultInvalid
        Quorum.CatchupRequired -> return $ Left ResultUnverifiable
receiveFinalizationMessage (FMTimeoutMessage tm) = do
    res <- Timeout.receiveTimeoutMessage tm =<< get
    case res of
        Timeout.Received vtm -> return $ Right $ void $ Timeout.executeTimeoutMessage vtm
        Timeout.Rejected Timeout.Duplicate -> return $ Left ResultDuplicate
        Timeout.Rejected Timeout.ObsoleteRound -> return $ Left ResultStale
        Timeout.Rejected _ -> return $ Left ResultInvalid
        Timeout.CatchupRequired -> return $ Left ResultUnverifiable

-- |Convert an 'Transactions.AddTransactionResult' to the corresponding 'UpdateResult'.
addTransactionResult :: Transactions.AddTransactionResult -> UpdateResult
addTransactionResult Transactions.Duplicate{} = ResultDuplicate
addTransactionResult Transactions.Added{} = ResultSuccess
addTransactionResult Transactions.ObsoleteNonce{} = ResultStale
addTransactionResult (Transactions.NotAdded verRes) =
    transactionVerificationResultToUpdateResult verRes

-- |Force a purge of the transaction table.
purgeTransactions :: (TimeMonad m, MonadState (SkovData pv) m) => m ()
purgeTransactions = purgeTransactionTable True =<< currentTime

-- |Start the timeout timer and trigger baking (if possible).
startEvents ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadState (SkovData (MPV m)) m,
      MonadProtocolVersion m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      TimeMonad m,
      TimerMonad m,
      MonadBroadcast m,
      MonadThrow m,
      MonadIO m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    m ()
startEvents = do
    resetTimerWithCurrentTimeout
    makeBlock
