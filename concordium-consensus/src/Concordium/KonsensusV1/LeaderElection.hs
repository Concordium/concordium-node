{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Concordium.KonsensusV1.LeaderElection (
    -- * Leader election
    getLeader,
    getLeaderFullBakers,

    -- * Seed state
    updateSeedStateForBlock,
    updateSeedStateForEpoch,
    nonceForNewEpoch,
) where

import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.SeedState

import Concordium.Crypto.VRF (proofToHash)
import Concordium.GlobalState.BakerInfo

-- |Compute the leader for a given round, given the set of bakers and leadership election nonce
-- for the epoch. The leader is computed as follows:
--
--  * Compute the SHA256 hash of the nonce and round number.
--  * Treating this as an big-endian unsigned integer, compute the remainder when divided by the
--    total stake among all the bakers, @targetVal@.
--  * Find the least index @i@ such that the sum of stakes of bakers with indices less than or equal
--    to @i@ is greater than @targetVal@. The baker with this index is the leader.
--
-- This process has a bias towards bakers with smaller indexes. However, the chance of this bias
-- affecting the result of any given election is less than 1 in 10^57, and so is effectively
-- impossible.
--
-- Note: this implementation is linear in the number of bakers. By pre-processing the baker set,
-- we could have a logarithmic algorithm. However, as the runtime is on the order of 5 us for 1000
-- bakers and 45 us for 10000, this is not a worthwhile optimisation at the current time.
-- (See the benchmark LeaderElectionBench.)
getLeader ::
    -- |(Non-empty) list of bakers and their effective stakes.
    [(bakerInfo, Amount)] ->
    -- |Current epoch leadership election nonce
    LeadershipElectionNonce ->
    -- |Round number to compute the leader for
    Round ->
    bakerInfo
getLeader bakers nonce rnd = grabBaker 0 bakers
  where
    hsh = getHash $ runPutLazy $ put nonce >> put rnd
    totalStake = toInteger . sum $ snd <$> bakers
    targetVal = fromIntegral $ Hash.hashToInteger hsh `mod` fromIntegral totalStake
    grabBaker runningTotal ((bi, amt) : bkrs)
        | targetVal < runningTotal + amt = bi
        | otherwise = grabBaker (runningTotal + amt) bkrs
    grabBaker _ [] = error "getLeader: Empty bakers"

-- |Compute the leader for a given round, given the set of bakers and leadership election nonce
-- for the epoch. (See 'getLeader'.)
getLeaderFullBakers :: FullBakers -> LeadershipElectionNonce -> Round -> FullBakerInfo
getLeaderFullBakers = getLeader . fmap bi . Vec.toList . fullBakerInfos
  where
    bi fbi@FullBakerInfo{..} = (fbi, _bakerStake)

-- |Compute the update for the leadership election nonce due to a particular block nonce.
updateWithBlockNonce ::
    -- |Block nonce to add
    BlockNonce ->
    -- |Running updated nonce
    Hash.Hash ->
    -- |New updated nonce
    Hash.Hash
updateWithBlockNonce bn un = Hash.hash $ runPut $ put un <> put (proofToHash bn)

-- |Update the running 'updatedNonce' in seed state. Blocks after the trigger block do not
-- contribute. If the timestamp is at least the epoch transition time, then the
-- 'epochTransitionTriggered' flag is set in the seed state, indicating that a new epoch should
-- begin once the block is finalized.
--
-- If the 'shutdownTriggered' flag is set in the seed state, then this indicates that
-- a protocol update has become effective. Note however, that the protocol update itself
-- will not take place before following the epoch transition.
updateSeedStateForBlock ::
    -- |Timestamp of the block
    Timestamp ->
    -- |Block nonce of the block
    BlockNonce ->
    -- |Protocol update effective
    Bool ->
    -- |Prior seed state
    SeedState 'SeedStateVersion1 ->
    -- |Updated seed state
    SeedState 'SeedStateVersion1
updateSeedStateForBlock ts bn isEffective ss
    -- Epoch transition is already in progress.
    | ss ^. epochTransitionTriggered = ss
    -- The block timestamp is beyond the trigger time of the epoch
    -- and there is also a protocol update effective.
    | isBlockAfterTriggerTime,
      isEffective =
        ss'
            & epochTransitionTriggered .~ True
            & shutdownTriggered .~ True
    -- The block timestamp is after the epoch trigger time,
    -- so flag it.
    | isBlockAfterTriggerTime = ss' & epochTransitionTriggered .~ True
    | otherwise = ss'
  where
    isBlockAfterTriggerTime = ss ^. triggerBlockTime <= ts
    ss' = ss & updatedNonce %~ updateWithBlockNonce bn

-- |Compute the leadership election nonce for the new epoch.
nonceForNewEpoch ::
    -- |Bakers for the new epoch
    FullBakers ->
    -- |Seed state to compute updated leadership election nonce
    SeedState 'SeedStateVersion1 ->
    LeadershipElectionNonce
nonceForNewEpoch newBakers SeedStateV1{..} = Hash.hash $ runPut $ do
    put ss1UpdatedNonce
    put (ss1Epoch + 1)
    putFullBakers newBakers

-- |Update the seed state to account for a transition to a new epoch.
-- Note: This function should never be called on a seed state with the `ss1ShutdownTriggered`
-- flag set.
updateSeedStateForEpoch ::
    -- |Bakers for the new epoch
    FullBakers ->
    -- |Epoch duration
    Duration ->
    -- |Seed state to update
    SeedState 'SeedStateVersion1 ->
    SeedState 'SeedStateVersion1
updateSeedStateForEpoch newBakers epochDuration ss =
    SeedStateV1
        { ss1Epoch = ss ^. epoch + 1,
          ss1EpochTransitionTriggered = False,
          ss1ShutdownTriggered = False,
          ss1TriggerBlockTime = (ss ^. triggerBlockTime) `addDuration` epochDuration,
          ss1UpdatedNonce = newNonce,
          ss1CurrentLeadershipElectionNonce = newNonce
        }
  where
    newNonce = nonceForNewEpoch newBakers ss
