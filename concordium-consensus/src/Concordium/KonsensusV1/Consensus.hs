{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set
import Data.Word

import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import qualified Concordium.Types.Accounts as Accounts

import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils

-- |A Monad for multicasting timeout messages.
class MonadMulticast m where
    -- |Multicast a timeout message.
    sendTimeoutMessage :: TimeoutMessage -> m ()

    -- |Multicast a quorum message.
    sendQuorumMessage :: QuorumMessage -> m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: Maybe BakerIdentity
    }

makeClassy ''BakerContext

-- |A Monad for timer related actions.
class MonadTimeout m where
    -- |Reset the timeout from the supplied 'Duration'.
    resetTimer :: Duration -> m ()

-- |Make a block if the consensus runner is leader for the
-- current round.
-- TODO: call 'makeBlock' if we're leader for the current round.
makeBlockIfLeader :: MonadState (SkovData (MPV m)) m => m ()
makeBlockIfLeader = return ()

-- |Advance the provided 'RoundStatus' to the provided 'Round'.
--
-- The consensus protocol can advance round in two ways.
-- 1. Via a QC i.e. @Right QuorumCertificate@
-- 2. Via a TC i.e. @Left (TimeoutCertificate, QuorumCertificate)@
--
-- All properties from the old 'RoundStatus' are being carried over to new 'RoundStatus'
-- except for the following.
-- * 'rsCurrentRound' will become the provided 'Round'.
-- * 'rsPreviousRoundTC' will become 'Absent' if we're progressing via a 'QuorumCertificate' otherwise
--   it will become the values of the supplied @Left (TimeoutCertificate, QuorumCertificate)@.
-- * 'rsHighestQC' will become the supplied @Right QuorumCertificate@ otherwise it is carried over.
advanceRoundStatus ::
    -- |The round to advance to.
    Round ->
    -- |@Left (tc, qc)@ if consensus is advancing from a TC.
    -- @Right qc@ if consensus is advancing from a QC.
    Either (TimeoutCertificate, QuorumCertificate) QuorumCertificate ->
    -- |The 'RoundStatus' we are advancing from.
    RoundStatus ->
    -- |The advanced 'RoundStatus'.
    RoundStatus
advanceRoundStatus toRound (Left (tc, qc)) currentRoundStatus =
    currentRoundStatus
        { _rsCurrentRound = toRound,
          _rsPreviousRoundTC = Present (tc, qc)
        }
advanceRoundStatus toRound (Right qc) currentRoundStatus =
    currentRoundStatus
        { _rsCurrentRound = toRound,
          _rsHighestQC = qc,
          _rsPreviousRoundTC = Absent
        }

-- |Advance to the provided 'Round'.
--
-- This function does the following:
-- * Update the current 'RoundStatus'.
-- * Persist the new 'RoundStatus'.
-- * If the consensus runner is leader in the new
--   round then make the new block.
advanceRound ::
    ( MonadTimeout m,
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
    currentRoundStatus <- use roundStatus
    -- We always reset the timer.
    -- This ensures that the timer is correct for consensus runners which have been
    -- leaving or joining the finalization committee (if we're advancing to the first round
    -- of that new epoch)
    -- Hence it is crucial when throwing the timeout then it must be checked that
    -- the consensus runner is either part of the current epoch (i.e. the new one) OR
    -- the prior epoch, as it could be the case that the consensus runner left the finalization committee
    -- coming into this new (current) epoch - but we still want to ensure that a timeout is thrown either way.
    resetTimer =<< use currentTimeout
    -- Clear the quorum messages collected.
    currentQuorumMessages .= emptyQuorumMessages
    -- Advance and save the round.
    setRoundStatus $! advanceRoundStatus newRound newCertificate currentRoundStatus
    -- Make a new block if the consensus runner is leader of
    -- the 'Round' progressed to.
    makeBlockIfLeader

-- |Compute the finalization committee given the bakers and the finalization committee parameters.
computeFinalizationCommittee :: FullBakers -> FinalizationCommitteeParameters -> FinalizationCommittee
computeFinalizationCommittee FullBakers{..} FinalizationCommitteeParameters{..} =
    FinalizationCommittee{..}
  where
    -- We use an insertion sort to construct the '_fcpMaxFinalizers' top bakers.
    -- Order them by descending stake and ascending baker ID.
    insert ::
        Map.Map (Down Amount, BakerId) FullBakerInfo ->
        FullBakerInfo ->
        Map.Map (Down Amount, BakerId) FullBakerInfo
    insert m fbi
        | Map.size m == fromIntegral _fcpMaxFinalizers = case Map.maxViewWithKey m of
            Nothing -> error "computeFinalizationCommittee: _fcpMaxFinalizers must not be 0"
            Just ((k, _), m')
                | insKey < k -> Map.insert insKey fbi m'
                | otherwise -> m
        | otherwise = Map.insert insKey fbi m
      where
        insKey = (Down (fbi ^. bakerStake), fbi ^. Accounts.bakerIdentity)
    amountSortedBakers = Map.elems $ foldl' insert Map.empty fullBakerInfos
    -- Threshold stake required to be a finalizer
    finalizerAmountThreshold :: Amount
    finalizerAmountThreshold =
        ceiling $
            partsPerHundredThousandsToRational _fcpFinalizerRelativeStakeThreshold
                * toRational bakerTotalStake
    -- Given the bakers sorted by their stakes, takes the first 'n' and then those that are
    -- at least at the threshold.
    takeFinalizers 0 fs = takeWhile ((>= finalizerAmountThreshold) . view bakerStake) fs
    takeFinalizers n (f : fs) = f : takeFinalizers (n - 1) fs
    takeFinalizers _ [] = []
    -- Compute the set of finalizers by applying the caps.
    cappedFinalizers = takeFinalizers _fcpMinFinalizers amountSortedBakers
    -- Sort the finalizers by baker ID.
    sortedFinalizers = sortOn (view Accounts.bakerIdentity) cappedFinalizers
    -- Construct finalizer info given the index and baker info.
    mkFinalizer finalizerIndex bi =
        FinalizerInfo
            { finalizerWeight = fromIntegral (bi ^. bakerStake),
              finalizerSignKey = bi ^. Accounts.bakerSignatureVerifyKey,
              finalizerVRFKey = bi ^. Accounts.bakerElectionVerifyKey,
              finalizerBlsKey = bi ^. Accounts.bakerAggregationVerifyKey,
              finalizerBakerId = bi ^. Accounts.bakerIdentity,
              ..
            }
    committeeFinalizers = Vec.fromList $ zipWith mkFinalizer [FinalizerIndex 0 ..] sortedFinalizers
    committeeTotalWeight = sum $ finalizerWeight <$> committeeFinalizers
