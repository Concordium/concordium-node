{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Concordium.GlobalState.SeedState where

import GHC.Generics
import Data.Word
import Data.Serialize

import Concordium.Types
import Concordium.Crypto.VRF
import Concordium.Crypto.SHA256

-- |State for computing the leadership election nonce.
data SeedState = SeedState {
  -- |Seed of the current epoch
  currentSeed :: LeadershipElectionNonce,
  -- |Number of slots in an epoch, probably stored in genesis data
  epochLength :: EpochLength,
  -- |Current epoch
  epoch :: Word64,
  -- |List of blocknonces from current epoch in reverse order
  revBlockNonces :: [BlockNonce]
} deriving (Eq, Generic, Show)
instance Serialize SeedState

-- |Instantiate a seed state: leadership election nonce should be random, epoch length should be long, but not too long...
genesisSeedState :: LeadershipElectionNonce -> EpochLength -> SeedState
genesisSeedState nonce epochLength =
  SeedState nonce epochLength 0 []

-- |Get the seed state for a slot and the seed state of the potential parent.
-- If slot is in the same epoch as the potential parent, then the seed state is shared
-- Else update the seed state, possibly iterating over empty epochs separating slot and potential parent
getSeedState :: Slot -> SeedState -> SeedState
getSeedState slot state@SeedState{..} = 
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
            put currentEpoch -- Todo what if several epochs?
            mapM_ (put . proofToHash) revBlockNonces), 
          epochLength = epochLength , 
          epoch = currentEpoch,
          revBlockNonces = []
        }
      else -- this only happens if one or several epochs are completely void of blocks:
        getSeedState slot $ getSeedState (slot - epochLength) state

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
            