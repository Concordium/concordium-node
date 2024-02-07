{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.PoolRewards where

import Data.Serialize
import Data.Word

import Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization

-- | 'BakerPoolRewardDetails' tracks the rewards that have been earned by a baker pool in the current
--  reward period. These are used to pay out the rewards at the payday.
data BakerPoolRewardDetails = BakerPoolRewardDetails
    { -- | The number of blocks baked by this baker in the reward period
      blockCount :: !Word64,
      -- | The total transaction fees accrued to this pool in the reward period
      transactionFeesAccrued :: !Amount,
      -- | Whether the pool contributed to a finalization proof in the reward period
      finalizationAwake :: !Bool
    }
    deriving (Eq, Show)

instance Serialize BakerPoolRewardDetails where
    put BakerPoolRewardDetails{..} = do
        putWord64be blockCount
        put transactionFeesAccrued
        putBool finalizationAwake

    get = BakerPoolRewardDetails <$> getWord64be <*> get <*> getBool

instance HashableTo Hash.Hash BakerPoolRewardDetails where
    getHash = Hash.hash . encode

instance (Monad m) => MHashableTo m Hash.Hash BakerPoolRewardDetails

-- | Baker pool reward details with no rewards accrued to the baker.
emptyBakerPoolRewardDetails :: BakerPoolRewardDetails
emptyBakerPoolRewardDetails =
    BakerPoolRewardDetails
        { blockCount = 0,
          transactionFeesAccrued = 0,
          finalizationAwake = False
        }
