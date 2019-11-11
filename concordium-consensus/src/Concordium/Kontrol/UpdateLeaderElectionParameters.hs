module Concordium.Kontrol.UpdateLeaderElectionParameters where

import Data.Serialize

import Concordium.GlobalState.SeedState
import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.Crypto.VRF
import Concordium.Crypto.SHA256

slotDependentBirkParameters :: Slot -> BirkParameters -> BirkParameters
slotDependentBirkParameters slot bps@BirkParameters{..} = 
  let
    currentEpoch = theSlot $ slot `div` epochLength _birkSeedState
    isInSameEpoch = currentEpoch == epoch _birkSeedState
  in
    if isInSameEpoch then
      -- if the slot is in the same epoch as the predecessor, nothing changes
      bps
    else
      -- if the slot is in a newer epoch, update the state of seed and bakers
      bps {
        -- leadership election nonce is updated recursively
        _birkSeedState = getNewEpochSeedState slot _birkSeedState,
        -- use stake distribution saved from the former epoch for leader election
        _birkLotteryBakers = _birkPrevEpochBakers,
        -- save the stake distribution from the end of the epoch
        _birkPrevEpochBakers = _birkCurrentBakers}


-- |Instantiate a seed state: leadership election nonce should be random, epoch length should be long, but not too long...
genesisSeedState :: LeadershipElectionNonce -> EpochLength -> SeedState
genesisSeedState nonce epochLength =
  SeedState nonce epochLength 0 []

-- |Get the seed state of the new epoch
getNewEpochSeedState :: Slot -- |The slot we need parameters for
                          -> SeedState -- |The seed state of the parent
                          -> SeedState
getNewEpochSeedState slot SeedState{..} = 
  let
    currentEpoch = theSlot $ slot `div` epochLength
  in
    SeedState{
      -- H(seed of predecessors epoch, epoch, block nonces in reverse order)
      currentSeed = hash (runPut $ do
        put currentSeed
        put currentEpoch
        mapM_ (put . proofToHash) revBlockNonces), 
      epochLength = epochLength , 
      epoch = currentEpoch,
      revBlockNonces = []
    }

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