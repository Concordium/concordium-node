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

-- |Instantiate a seed state: leadership elction nonce should be random, epoch length should be long, but not too long...
genesisSeedState :: LeadershipElectionNonce -> EpochLength -> SeedState
genesisSeedState nonce epochLength =
  SeedState nonce epochLength 0 []

getSeed :: SeedState -> LeadershipElectionNonce
getSeed state = currentSeed state

updateSeed :: Slot -> BlockNonce -> SeedState -> SeedState
updateSeed slot bn state@SeedState{..} =
  let 
    currentEpoch = theSlot $ slot `div` epochLength
    isFirstBlockOfEpoch = currentEpoch /= epoch
    shouldContributeBlockNonce = slot `rem` epochLength <= (2 * epochLength) `div` 3
  in
    if isFirstBlockOfEpoch then 
      SeedState{
        -- H(seed of last epoch, epoch, block nonces in reverse order)
        currentSeed = hash (runPut $ do
          put currentSeed
          put (epoch + 1)
          mapM_ (put . proofToHash) revBlockNonces), 
        epochLength = epochLength , 
        epoch = currentEpoch,
        revBlockNonces = if shouldContributeBlockNonce then [bn] else []
      }
    else if shouldContributeBlockNonce then
      -- less than 2/3 slots into the epoch, add the new block nonce
        state {revBlockNonces = bn : revBlockNonces}
    else 
      -- more than 2/3 slots into the epoch, no update
      state
