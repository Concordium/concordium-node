{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe (isJust)
import qualified Data.Vector as Vector

import Lens.Micro.Platform

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.BakerIdentity

-- |A Monad for multicasting timeout messages.
class MonadMulticast m where
    -- |Multicast a timeout message over the network
    sendTimeoutMessage :: TimeoutMessage -> m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: BakerIdentity
    }

makeClassy ''BakerContext

-- |A Monad for timer related actions.
class MonadTimeout m where
    -- |Reset the timeout from the supplied 'Duration'.
    resetTimer :: Duration -> m ()

-- |Produce a block and multicast it onto the network.
makeBlock :: MonadState (SkovData (MPV m)) m => m ()
makeBlock = return ()

-- |Make a block if the consensus runner is leader for the
-- current round.
-- TODO: call 'makeBlock' if we're leader for the current round.
makeBlockIfLeader :: MonadState (SkovData (MPV m)) m => m ()
makeBlockIfLeader = return ()

-- |Advance to the provided 'Round'.
--
-- This function does the following:
-- * Update the current 'RoundStatus'.
-- * Persist the new 'RoundStatus'.
-- * If the consensus runner is leader in the new
--   round then make the new block.
advanceRound ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadTimeout m,
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |The 'Round' to progress to.
    Round ->
    -- |If we are advancing from a round that timed out
    -- then this will be @Left 'TimeoutCertificate, 'QuorumCertificate')@
    -- The 'TimeoutCertificate' is from the round we're
    -- advancing from and the associated 'QuorumCertificate' verifies it.
    --
    -- Otherwise if we're progressing via a 'QuorumCertificate' then @Right QuorumCertificate@
    -- should be the QC we're advancing round via.
    Either (TimeoutCertificate, QuorumCertificate) QuorumCertificate ->
    m ()
advanceRound newRound newCertificate = do
    myBakerId <- bakerId <$> view bakerIdentity
    currentRoundStatus <- use roundStatus
    -- Reset the timeout timer if the consensus runner is part of the
    -- finalization committee.
    resetTimerIfFinalizer myBakerId (rsCurrentTimeout currentRoundStatus) (rsCurrentEpoch currentRoundStatus)
    -- Advance and save the round.
    setRoundStatus $! advanceRoundStatus newRound newCertificate currentRoundStatus
    -- Make a new block if the consensus runner is leader of
    -- the 'Round' progressed to.
    makeBlockIfLeader
  where
    -- Reset the timer if this consensus instance is member of the
    -- finalization committee for the current 'Epoch'.
    resetTimerIfFinalizer bakerId currentTimeout currentEpoch = do
        gets (getBakersForLiveEpoch currentEpoch) >>= \case
            Nothing -> return () -- No bakers or finalizers could be looked up for the current 'Epoch' so we do nothing.
            Just bakersAndFinalizers -> do
                when (isJust $ finalizerByBakerId (bakersAndFinalizers ^. bfFinalizers) bakerId) $
                    -- The consensus runner is a finalizer for the current epoch then we reset the timer
                    resetTimer currentTimeout
                    
-- |Compute and return the 'LeadershipElectionNonce' for
-- the provided 'Epoch' and 'FinalizationEntry'
-- TODO: implement.
computeLeadershipElectionNonce ::
    -- |The 'Epoch' to compute the 'LeadershipElectionNonce' for.
    Epoch ->
    -- |The witness for the new 'Epoch'
    FinalizationEntry ->
    -- |The new 'LeadershipElectionNonce'
    LeadershipElectionNonce
computeLeadershipElectionNonce epoch finalizationEntry = undefined

-- |Advance the 'Epoch' of the current 'RoundStatus'.
--
-- Advancing epochs in particular carries out the following:
-- * Updates the 'rsCurrentEpoch' to the provided 'Epoch' for the current 'RoundStatus'.
-- * Computes the new 'LeadershipElectionNonce' and updates the current 'RoundStatus'.
-- * Updates the 'rsLatestEpochFinEntry' of the current 'RoundStatus' to @Present finalizationEntry@.
-- * Persist the new 'RoundStatus' to disk.
advanceEpoch ::
    ( MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    Epoch ->
    FinalizationEntry ->
    m ()
advanceEpoch newEpoch finalizationEntry = do
    currentRoundStatus <- use roundStatus
    let newRoundStatus = advanceRoundStatusEpoch newEpoch finalizationEntry newLeadershipElectionNonce currentRoundStatus
    setRoundStatus newRoundStatus
  where
    -- compute the new leadership election nonce.
    newLeadershipElectionNonce = computeLeadershipElectionNonce newEpoch finalizationEntry
