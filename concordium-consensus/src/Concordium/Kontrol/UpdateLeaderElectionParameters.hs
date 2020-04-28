module Concordium.Kontrol.UpdateLeaderElectionParameters where

import Data.Serialize

import Concordium.GlobalState.Classes
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.SeedState
import Concordium.Types
import Concordium.Crypto.VRF
import Concordium.Crypto.SHA256

slotDependentBirkParameters :: BirkParametersOperations m => Slot -> BirkParameters m -> m (BirkParameters m)
slotDependentBirkParameters slot bps = do
  seedState <- getSeedState bps
  case slotDependentSeedState slot seedState of
    Just newSeedState ->
        -- if the slot is in a newer epoch, update the state of seed and bakers
        updateBirkParametersForNewEpoch newSeedState bps
    Nothing ->
        -- if the slot is in the same epoch as the predecessor, nothing changes
        return bps

-- If the slot is in a newer epoch returns the new seed state, otherwise returns Nothing
slotDependentSeedState :: Slot -> SeedState -> Maybe SeedState
slotDependentSeedState slot seedState =
    let currentEpoch = theSlot $ slot `div` epochLength seedState
        isInSameEpoch = currentEpoch == epoch seedState
    in if isInSameEpoch
       then Nothing
       else Just $ getNewEpochSeedState slot seedState

-- |Instantiate a seed state: leadership election nonce should be random, epoch length should be long, but not too long...
genesisSeedState :: LeadershipElectionNonce -> EpochLength -> SeedState
genesisSeedState nonce epochLength =
  SeedState nonce epochLength 0 Nothing

-- |Get the seed state of the new epoch
getNewEpochSeedState :: Slot -- ^The slot we need parameters for
                          -> SeedState -- ^The seed state of the parent
                          -> SeedState
getNewEpochSeedState slot state@SeedState{..} =
  let
    currentEpoch = theSlot $ slot `div` epochLength
    newSeed = hash (runPut $ do
                            mapM_ put blockNonceHash 
                            put currentEpoch)
  in
    state{
      currentSeed = newSeed,
      epoch = currentEpoch,
      blockNonceHash = Just newSeed
    }

-- |When a new block is added, it affects the resettable leaky beacon iff it is in the first 2/3 slots of the epoch
updateSeedState :: Slot -> BlockNonce -> SeedState -> SeedState
updateSeedState slot bn state@SeedState{..} =
  let 
    shouldContributeBlockNonce = 3 * (slot `rem` epochLength) < 2 * epochLength
  in
    if shouldContributeBlockNonce then
      -- less than 2/3 slots into the epoch, add the new block nonce
      state {blockNonceHash = Just $ hash $ runPut $ do
              mapM_ put blockNonceHash
              put $ proofToHash bn}
    else 
      -- more than 2/3 slots into the epoch, no update
      state