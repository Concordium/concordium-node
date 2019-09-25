{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Concordium.GlobalState.SeedState where

import GHC.Generics
import Data.Word
import Data.Serialize

import Concordium.Types

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