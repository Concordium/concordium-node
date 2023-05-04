{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1 where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Class

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus
import qualified Concordium.KonsensusV1.Consensus.Quorum as Quorum
import qualified Concordium.KonsensusV1.Consensus.Timeout as Timeout
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Concordium.Skov.Monad (UpdateResult (..))
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.Parameters

receiveFinalizationMessage ::
    ( IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      TimeMonad m,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m,
      MonadConsensusEvent m,
      MonadLogger m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadTreeStateStore m
    ) =>
    FinalizationMessage ->
    m (Either UpdateResult (m ()))
receiveFinalizationMessage (FMQuorumMessage qm) = do
    res <- Quorum.receiveQuorumMessage qm =<< get
    case res of
        Quorum.Received vqm -> return $ Right $ Quorum.processQuorumMessage vqm
        Quorum.Rejected _ -> return $ Left ResultInvalid
        Quorum.CatchupRequired -> return $ Left ResultUnverifiable
        Quorum.Duplicate -> return $ Left ResultDuplicate
receiveFinalizationMessage (FMTimeoutMessage tm) = do
    res <- Timeout.receiveTimeoutMessage tm =<< get
    case res of
        Timeout.Received vtm -> return $ Right $ void $ Timeout.executeTimeoutMessage vtm
        Timeout.Rejected _ -> return $ Left ResultInvalid
        Timeout.CatchupRequired -> return $ Left ResultUnverifiable
        Timeout.Duplicate -> return $ Left ResultDuplicate
