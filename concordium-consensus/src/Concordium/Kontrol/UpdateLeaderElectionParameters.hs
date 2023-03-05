{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

-- |Functionality for updating and computing leadership election nonces.
-- These functions pertain to consensus version 0. The analogues for consensus version 1 are
-- found in 'Concordium.KonsensusV1.LeaderElection'.
module Concordium.Kontrol.UpdateLeaderElectionParameters (
    updateSeedState,
    computeLeadershipElectionNonce,
    predictLeadershipElectionNonce,
) where

import Data.Serialize
import Lens.Micro.Platform

import Concordium.Crypto.SHA256 as H
import Concordium.Crypto.VRF
import Concordium.Types
import Concordium.Types.SeedState

-- |Compute the update for the leadership election nonce due to a particular block nonce.
updateWithBlockNonce :: BlockNonce -> H.Hash -> H.Hash
updateWithBlockNonce bn un = hash $ runPut $ put un <> put (proofToHash bn)

-- |Compute the update for the leadership election nonce due to completion of an epoch.
updateWithEpoch ::
    -- |Epoch that has been completed
    Epoch ->
    -- |Running updated leadership election nonce
    H.Hash ->
    H.Hash
updateWithEpoch e !n = hash $ runPut $ put n <> put e

-- |Update the seed state. The slot must not belong to a prior epoch.
updateSeedState :: Slot -> BlockNonce -> SeedState 'SeedStateVersion0 -> SeedState 'SeedStateVersion0
updateSeedState slot bn state = case compare newEpoch oldEpoch of
    EQ -> updateNonce state
    GT -> updateEpochs oldEpoch state
    LT -> error $ "updateSeedState: new epoch (" ++ show newEpoch ++ ") precedes current epoch (" ++ show oldEpoch ++ ")"
  where
    oldEpoch = state ^. epoch
    el = state ^. epochLength
    (fromIntegral -> newEpoch, slotRem) = slot `quotRem` el
    shouldContributeBlockNonce = 3 * slotRem < 2 * el
    -- If the slot falls within the first 2/3 of the epoch's slots,
    -- add the block nonce to the updatedNonce.
    updateNonce
        | shouldContributeBlockNonce = updatedNonce %~ updateWithBlockNonce bn
        | otherwise = id
    updateEpochs :: Epoch -> SeedState 'SeedStateVersion0 -> SeedState 'SeedStateVersion0
    updateEpochs e s
        | e == newEpoch = updateNonce (s & epoch .~ newEpoch)
        | otherwise = updateEpochs (e + 1) (s & currentLeadershipElectionNonce .~ h & updatedNonce .~ h)
      where
        h = updateWithEpoch e (s ^. updatedNonce)

-- |Derive the leadership election nonce for a particular slot from the seed state.
-- This compensates for if the slot is in a different epoch.
-- NB: If the slot is in a different epoch than the epoch of the seedstate,
-- the block nonces from the previous epoch are not accounted for in the calculation of the
-- leadership election nonce. This means that the leadership election nonce used to produce
-- the first block is different than those used for all other blocks of an epoch.
--
-- This behaviour differs from the specification in the Bluepaper. This is not a security issue, since
-- the leadership election nonce will still get updated each epoch.
computeLeadershipElectionNonce :: SeedState 'SeedStateVersion0 -> Slot -> LeadershipElectionNonce
computeLeadershipElectionNonce state slot = case compare newEpoch oldEpoch of
    EQ -> state ^. currentLeadershipElectionNonce
    GT -> updateEpochs oldEpoch (state ^. currentLeadershipElectionNonce)
    LT -> error $ "computeLeadershipElectionNonce: new epoch (" ++ show newEpoch ++ ") precedes current epoch (" ++ show oldEpoch ++ ")"
  where
    oldEpoch = state ^. epoch
    newEpoch = fromIntegral $ slot `quot` (state ^. epochLength)
    updateEpochs e n
        | e == newEpoch = n
        | otherwise = updateEpochs (e + 1) (updateWithEpoch e n)

-- |Predict a future leadership election nonce as far as possible.
-- The slot to predict for must be after the slot of the last finalized block.
-- Since the leadership election nonce used to produce a block in a slot
-- depends on whether the block is the first block in the epoch, we take
-- this into account in order to predict the correct nonce. If we don't
-- know the slot of the pending block's parent, we return two
-- leadership election nonces as we don't know which of them is the correct one.
--
-- If @predictLeadershipElectionNonce ss lastFinSlot (Just pendingParentSlot) targetSlot = Just [n]@
-- and @pendingParentSlot@ is @>= lastFinSlot@ and in the same epoch as @lastFinSlot@ then it must be that
-- @computeLeadershipElectionNonce ss targetSlot = n@.
--
-- If @predictLeadershipElectionNonce ss lastFinSlot (Just pendingParentSlot) targetSlot = Just [m]@
-- and @pendingParentSlot@ is in the epoch after @lastFinSlot@ then for any @bn@, @sl@
-- (a slot in the same epoch as @targetSlot@) and @ss'@ with
-- @sl < targetSlot@ and @ss' = updateSeedState sl bn ss@ it must be that
-- @computeLeadershipElectionNonce ss' targetSlot = m@.
--
-- If @predictLeadershipElectionNonce ss lastFinSlot maybeParentBlockSlot targetSlot = Just [n, m]@ then it must be that
-- @maybeParentBlockSlot = Nothing@, @computeLeadershipElectionNonce ss targetSlot = n@, and, for
-- for any @bn@, @sl@ (a slot in the same epoch as @targetSlot@) and @ss'@ with @sl < targetSlot@
-- and @ss' = updateSeedState sl bn ss@, it must be that @computeLeadershipElectionNonce ss' targetSlot = m@.
--
-- If @predictLeadershipElectionNonce ss lastFinSlot maybeParentBlockSlot targetSlot = Nothing@, it means that we
-- cannot predict the leadership election nonce. It will happen if @lastFinSlot@ is earlier than two thirds of
-- the epoch and @targetSlot@ is in the epoch after @lastFinSlot@, or if @targetSlot@ is in a later epoch than
-- the epoch after @lastFinSlot@.
--
-- Moreover, if @predictLeadershipElectionNonce ss lastFinSlot maybeParentBlockSlot targetSlot = Just A@,
-- for any @bn@, @sl@, and @ss'@ with @lastFinSlot < sl < targetSlot@, and
-- @ss' = updateSeedState sl bn ss@, it must be that
-- @predictLeadershipElectionNonce ss' sl maybeParentBlockSlot targetSlot = Just B@ where B is a sublist of A.
predictLeadershipElectionNonce ::
    -- |Seed state of last finalized block
    SeedState 'SeedStateVersion0 ->
    -- |Slot of last finalized block
    Slot ->
    -- |Maybe slot of pending block's parent (Just if known, Nothing otherwise)
    Maybe Slot ->
    -- |Slot to predict for
    Slot ->
    Maybe [LeadershipElectionNonce]
predictLeadershipElectionNonce SeedStateV0{..} lastFinSlot maybeParentBlockSlot targetSlot
    | slotEpoch targetSlot == ss0Epoch = Just [ss0CurrentLeadershipElectionNonce]
    | slotEpoch targetSlot == ss0Epoch + 1 && 3 * (lastFinSlot `rem` ss0EpochLength) >= 2 * ss0EpochLength =
        -- In this case, no blocks after the last finalized block can contribute to the block
        -- nonce for the next epoch.
        case maybeParentBlockSlot of
            Nothing -> Just [current, updated]
            Just parentBlockSlot ->
                if slotEpoch parentBlockSlot == slotEpoch targetSlot
                    then Just [updated]
                    else Just [current]
    | otherwise = Nothing
  where
    slotEpoch :: Slot -> Epoch
    slotEpoch slot = fromIntegral $ slot `quot` ss0EpochLength
    current = updateWithEpoch ss0Epoch ss0CurrentLeadershipElectionNonce
    updated = updateWithEpoch ss0Epoch ss0UpdatedNonce
