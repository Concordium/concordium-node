{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.PoolRewards where

import Data.Serialize
import Data.Word

import Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization

-- | 'BakerPoolRewardDetails' tracks the rewards that have been earned by a baker pool in the current
--  reward period. These are used to pay out the rewards at the payday.
data BakerPoolRewardDetails (av :: AccountVersion) = BakerPoolRewardDetails
    { -- | The number of blocks baked by this baker in the reward period
      blockCount :: !Word64,
      -- | The total transaction fees accrued to this pool in the reward period
      transactionFeesAccrued :: !Amount,
      -- | Whether the pool contributed to a finalization proof in the reward period
      finalizationAwake :: !Bool,
      -- | The number of missed rounds in the reward period
      missedRounds :: !(Conditionally (SupportsValidatorSuspension av) Word64)
    }
    deriving (Eq, Show)

instance forall av. (IsAccountVersion av) => Serialize (BakerPoolRewardDetails av) where
    put BakerPoolRewardDetails{..} = do
        putWord64be blockCount
        put transactionFeesAccrued
        putBool finalizationAwake
        mapM_ putWord64be missedRounds

    get =
        BakerPoolRewardDetails
            <$> getWord64be
            <*> get
            <*> getBool
            <*> conditionallyA (sSupportsValidatorSuspension (accountVersion @av)) get

instance forall av. (IsAccountVersion av) => HashableTo Hash.Hash (BakerPoolRewardDetails av) where
    getHash = Hash.hash . encode

instance forall m av. (IsAccountVersion av, Monad m) => MHashableTo m Hash.Hash (BakerPoolRewardDetails av)

-- | Baker pool reward details with no rewards accrued to the baker.
emptyBakerPoolRewardDetails :: forall av. (IsAccountVersion av) => BakerPoolRewardDetails av
emptyBakerPoolRewardDetails =
    BakerPoolRewardDetails
        { blockCount = 0,
          transactionFeesAccrued = 0,
          finalizationAwake = False,
          missedRounds = conditionally (sSupportsValidatorSuspension (accountVersion @av)) 0
        }

-- | Migrate BakerPoolRewardDetails with different account versions.
migrateBakerPoolRewardDetails ::
    forall av0 av1.
    (IsAccountVersion av1) =>
    BakerPoolRewardDetails av0 ->
    BakerPoolRewardDetails av1
migrateBakerPoolRewardDetails BakerPoolRewardDetails{..} =
    BakerPoolRewardDetails
        { missedRounds =
            conditionally
                (sSupportsValidatorSuspension (accountVersion @av1))
                (fromCondDef missedRounds 0),
          ..
        }
