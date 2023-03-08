{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Maybe (isJust)
import Data.Ratio
import qualified Data.Vector as Vector
import Data.Word
import Lens.Micro.Platform

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Utils

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)

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

-- |Return 'Just FinalizerInfo' if the consensus running
-- is part of the of the provided 'BakersAndFinalizers'.
-- Otherwise return 'Nothing'.
isBakerFinalizer ::
    BakerId ->
    -- |A collection of bakers and finalizers.
    BakersAndFinalizers ->
    -- |'True' if the consensus is part of the finalization committee.
    -- Otherwise 'False'
    Maybe FinalizerInfo
isBakerFinalizer bakerId bakersAndFinalizers = do
    -- This is O(n) but in principle we could do binary search here as the 'committeeFinalizers' are
    -- sorted by ascending baker id.
    Vector.find (\finalizerInfo -> finalizerBakerId finalizerInfo == bakerId) finalizers
  where
    finalizers = committeeFinalizers $ bakersAndFinalizers ^. bfFinalizers

-- |Advance to the provided 'Round'.
advanceRound ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadTimeout m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |The 'Round' to progress to.
    Round ->
    -- |If we are advancing from a round that timed out
    -- then this will be @Just 'TimeoutCertificate, 'QuorumCertificate')@ otherwise
    -- 'Nothing'.
    --
    -- In case of the former then the 'TimeoutCertificate' is from the round we're
    -- advancing from and the associated 'QuorumCertificate' verifies it.
    Maybe (TimeoutCertificate, QuorumCertificate) ->
    m ()
advanceRound newRound timedOut = do
    myBakerId <- bakerId <$> view bakerIdentity
    currentRoundStatus <- use roundStatus
    resetTimerIfFinalizer myBakerId (rsCurrentTimeout currentRoundStatus)
    -- Advance the round.
    roundStatus .=! advanceRoundStatus newRound timedOut currentRoundStatus
  where
    -- TODO: If we're baker in 'newRound' then call 'makeBlock'

    -- \|Reset the timer if this consensus instance is member of the
    -- finalization committee for the current 'Epoch'.
    resetTimerIfFinalizer bakerId currentTimeout = do
        currentEpoch <- rsCurrentEpoch <$> use roundStatus
        gets (getBakersForLiveEpoch currentEpoch) >>= \case
            Nothing -> return () -- well this is awkward.
            Just bakersAndFinalizers -> do
                if isJust $! isBakerFinalizer bakerId bakersAndFinalizers
                    then -- If we're a finalizer for the current epoch then we reset the timer
                        resetTimer currentTimeout
                    else return ()
