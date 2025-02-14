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

-- | Information needed to determine whether to suspend a validator.
data SuspensionInfo = SuspensionInfo
    { -- | The number of missed rounds since the validator most recently
      --  became a (non-suspended) member of the validator committee.
      missedRounds :: !Word64,
      -- | Flag indicating that the validator should be suspended at the coming
      --  snapshot epoch, because its missed rounds have crossed the threshold
      --  given in the chain parameters in the previous reward period.
      primedForSuspension :: !Bool
    }
    deriving (Eq, Show)

emptySuspensionInfo :: SuspensionInfo
emptySuspensionInfo = SuspensionInfo{missedRounds = 0, primedForSuspension = False}

instance Serialize SuspensionInfo where
    put SuspensionInfo{..} = do
        putWord64be missedRounds
        putBool primedForSuspension
    get =
        SuspensionInfo <$> get <*> getBool

-- | 'BakerPoolRewardDetails' tracks the rewards that have been earned by a baker pool in the current
--  reward period. These are used to pay out the rewards at the payday.
data BakerPoolRewardDetails (av :: AccountVersion) = BakerPoolRewardDetails
    { -- | The number of blocks baked by this baker in the reward period
      blockCount :: !Word64,
      -- | The total transaction fees accrued to this pool in the reward period
      transactionFeesAccrued :: !Amount,
      -- | Whether the pool contributed to a finalization proof in the reward period
      finalizationAwake :: !Bool,
      -- | Information for deciding whether a validator will be suspended at the next snapshot epoch.
      suspensionInfo :: !(Conditionally (SupportsValidatorSuspension av) SuspensionInfo)
    }
    deriving (Eq, Show)

instance forall av. (IsAccountVersion av) => Serialize (BakerPoolRewardDetails av) where
    put BakerPoolRewardDetails{..} = do
        putWord64be blockCount
        put transactionFeesAccrued
        putBool finalizationAwake
        mapM_ put suspensionInfo

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
          suspensionInfo =
            conditionally (sSupportsValidatorSuspension (accountVersion @av)) emptySuspensionInfo
        }

-- | Migrate BakerPoolRewardDetails with different account versions.
migrateBakerPoolRewardDetails ::
    forall av0 av1.
    (IsAccountVersion av1) =>
    BakerPoolRewardDetails av0 ->
    BakerPoolRewardDetails av1
migrateBakerPoolRewardDetails BakerPoolRewardDetails{..} =
    BakerPoolRewardDetails
        { suspensionInfo =
            conditionally
                (sSupportsValidatorSuspension (accountVersion @av1))
                (fromCondDef suspensionInfo emptySuspensionInfo),
          ..
        }
