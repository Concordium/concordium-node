{-# LANGUAGE DeriveGeneric #-}

module Concordium.GlobalState.SeedState where

import GHC.Generics
import Data.Word
import Data.Serialize

import Concordium.Crypto.SHA256 (Hash)
import Concordium.Types

-- |State for computing the leadership election nonce.
data SeedState = SeedState {
  -- |Seed of the current epoch
  currentSeed :: LeadershipElectionNonce,
  -- |Number of slots in an epoch, probably stored in genesis data
  epochLength :: EpochLength,
  -- |Current epoch
  epoch :: Word64,
  -- |Hash of the block nonces in the current epoch up to block B,
  --  where B is either the last of the first 2/3 of the blocks in this epoch,
  --  or the current block, whichever comes first.
  --  blockNonceHash should be Nothing only for the genesis block.
  blockNonceHash :: Maybe Hash

} deriving (Eq, Generic, Show)
instance Serialize SeedState

-- |Instantiate a seed state: leadership election nonce should be random, epoch length should be long, but not too long...
genesisSeedState :: LeadershipElectionNonce -> EpochLength -> SeedState
genesisSeedState nonce epochLength =
  SeedState nonce epochLength 0 Nothing
