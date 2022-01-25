module Concordium.GlobalState.Basic.BlockState.PoolRewards where

import Data.Serialize
import qualified Data.Vector as Vec
import Data.Word

import Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMBT

-- TODO: Define and implement the hashing scheme.

data BakerPoolRewardDetails = BakerPoolRewardDetails
    { -- |The number of blocks baked by this baker in the reward period
      blockCount :: !Word64,
      -- |The total transaction fees accrued to this pool in the reward period
      transactionFeesAccrued :: !Amount,
      -- |Whether the pool contributed to a finalization proof in the reward period
      finalizationAwake :: !Bool
    }
data DelegatorCapital = DelegatorCapital
    { -- |'DelegatorId' of the delegator
      dcDelegatorId :: !DelegatorId,
      -- |'Amount' staked by the delegator
      dcDelegatorCapital :: !Amount
    }

instance Serialize DelegatorCapital where
    put DelegatorCapital{..} = do
        put dcDelegatorId
        put dcDelegatorCapital
    get = do
        dcDelegatorId <- get
        dcDelegatorCapital <- get
        return DelegatorCapital{..}

instance HashableTo Hash.Hash DelegatorCapital where
    getHash = Hash.hash . encode

data BakerCapital = BakerCapital
    { -- |'BakerId' of the pool owner
      bcBakerId :: !BakerId,
      -- |Equity capital of the pool owner
      bcBakerEquityCapital :: !Amount,
      -- |Capital of each baker delegated to this pool
      bcDelegatorCapital :: !(Vec.Vector DelegatorCapital)
    }

data CapitalDistribution = CapitalDistribution
    { -- |Capital associated with baker pools
      bakerPoolCapital :: !(Vec.Vector BakerCapital),
      -- |Capital associated with the L-pool
      lPoolCapital :: !(Vec.Vector DelegatorCapital)
    }

-- |Details of rewards accruing over the course of a reward period, and details about the capital
-- distribution for this reward period and (possibly) the next.
data PoolRewards = PoolRewards
    { -- |The capital distribution for the next reward period.
      -- This is updated the epoch before a payday.
      nextCapital :: !CapitalDistribution, -- TODO: Use Hashed

      -- |The capital distribution for the current reward period.
      currentCapital :: !CapitalDistribution, -- TODO: Use Hashed

      -- |The details of rewards accruing to baker pools.
      -- These are indexed by the index of the baker in the capital distribution (_not_ the BakerId).
      bakerPoolRewardDetails :: !(LFMBT.LFMBTree Word64 BakerPoolRewardDetails),
      -- |The transaction reward amount accruing to the L-pool.
      lPoolTransactionRewards :: !Amount,
      -- |The transaction reward fraction accruing to the foundation.
      foundationTransactionRewards :: !Amount,
      -- |The next payday occurs at the start of this epoch.
      nextPaydayEpoch :: !Epoch,
      -- |The rate at which tokens are minted for the current reward period.
      nextPaydayMintRate :: !MintRate
    }

bakerBlockCounts :: PoolRewards -> [(BakerId, Word64)]
bakerBlockCounts PoolRewards{..} = zipWith bc (Vec.toList (bakerPoolCapital currentCapital)) (LFMBT.toAscPairList bakerPoolRewardDetails)
    where
        bc BakerCapital{..} (_, BakerPoolRewardDetails{..}) = (bcBakerId, blockCount)