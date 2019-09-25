{-# LANGUAGE RecordWildCards #-}

module Concordium.Kontrol.UpdateLeaderElectionParameters where

import Data.Serialize

import Concordium.GlobalState.SeedState
import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.Crypto.VRF
import Concordium.Crypto.SHA256

slotDependentBirkParameters :: Slot -> BirkParameters -> BirkParameters
slotDependentBirkParameters slot bps@BirkParameters{..} = 
  bps {
    _seedState = getSlotDependentSeedState slot _seedState,
    -- save the stake distribution from the end of the epoch, use stake distribution saved from the former epoch for leader election
    _birkEpochBakers = (_birkBakers,  fst _birkEpochBakers)}


-- |Instantiate a seed state: leadership election nonce should be random, epoch length should be long, but not too long...
genesisSeedState :: LeadershipElectionNonce -> EpochLength -> SeedState
genesisSeedState nonce epochLength =
  SeedState nonce epochLength 0 []

-- |Get the seed state used in leader election for a particular slot and the seed state of the parent block
-- If the slot is in the same epoch as the slot of the parent, nothing changes
-- Else update the seed state, possibly iterating over empty epochs separating slot and parent
getSlotDependentSeedState :: Slot -- |The slot we need parameters for
                          -> SeedState -- |The seed state of the parent
                          -> SeedState
getSlotDependentSeedState slot state@SeedState{..} = 
  let
    currentEpoch = theSlot $ slot `div` epochLength
    isInSameEpoch = currentEpoch == epoch
  in
    if isInSameEpoch then 
      state
    else 
      if currentEpoch == epoch + 1 then
        SeedState{
          -- H(seed of last epoch, epoch, block nonces in reverse order)
          currentSeed = hash (runPut $ do
            put currentSeed
            put currentEpoch
            mapM_ (put . proofToHash) revBlockNonces), 
          epochLength = epochLength , 
          epoch = currentEpoch,
          revBlockNonces = []
        }
      else -- this only happens if one or several epochs are completely void of blocks:
        getSlotDependentSeedState
          slot $ getSlotDependentSeedState
          (slot - epochLength) state

-- |When a new block is added, it affects the resettable leaky beacon iff it is in the first 2/3 slots of the epoch
updateSeedState :: Slot -> BlockNonce -> SeedState -> SeedState
updateSeedState slot bn state@SeedState{..} =
  let 
    shouldContributeBlockNonce = slot `rem` epochLength <= (2 * epochLength) `div` 3
  in
    if shouldContributeBlockNonce then
      -- less than 2/3 slots into the epoch, add the new block nonce
      state {revBlockNonces = bn : revBlockNonces}
    else 
      -- more than 2/3 slots into the epoch, no update
      state