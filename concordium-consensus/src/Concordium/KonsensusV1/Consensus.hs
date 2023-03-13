{-# LANGUAGE TemplateHaskell #-}

module Concordium.KonsensusV1.Consensus where

import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import qualified Concordium.Types.Accounts as Accounts
import Concordium.Types.Parameters

import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.Types
import Concordium.Types.BakerIdentity

-- |A Monad for multicasting timeout messages.
class MonadMulticast m where
    -- |Multicast a timeout message over the network.
    sendTimeoutMessage :: TimeoutMessage -> m ()
    -- |Multicast a quorum signature message over the network.
    sendQuorumMessage :: QuorumMessage -> m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: BakerIdentity
    }

makeClassy ''BakerContext

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
