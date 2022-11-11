{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.BakerInfo where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch
import Data.Ratio
import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Execution (DelegationTarget, OpenStatus)

data FullBakerInfo = FullBakerInfo
    { _theBakerInfo :: !BakerInfo,
      _bakerStake :: !Amount
    }
    deriving (Eq, Show)

instance Serialize FullBakerInfo where
    put FullBakerInfo{..} = do
        put _theBakerInfo
        put _bakerStake
    get = do
        _theBakerInfo <- get
        _bakerStake <- get
        return FullBakerInfo{..}

makeClassy ''FullBakerInfo

instance HasBakerInfo FullBakerInfo where
    bakerInfo = theBakerInfo
instance HashableTo H.Hash FullBakerInfo where
    getHash = H.hash . encode

data FullBakers = FullBakers
    { -- |All bakers in ascending order of BakerId.
      fullBakerInfos :: !(Vec.Vector FullBakerInfo),
      -- |The total stake of all bakers.
      bakerTotalStake :: !Amount
    }
    deriving (Eq, Show)

-- |Look up a baker by its identifier.
-- This is implemented with binary search.
fullBaker :: FullBakers -> BakerId -> Maybe FullBakerInfo
fullBaker FullBakers{..} = binarySearch (_bakerIdentity . _theBakerInfo) fullBakerInfos

lotteryBaker :: FullBakers -> BakerId -> Maybe (BakerInfo, LotteryPower)
lotteryBaker fbs bid = lp <$> fullBaker fbs bid
  where
    lp fb = (fb ^. bakerInfo, fb ^. bakerStake % bakerTotalStake fbs)

-- |'FullBakerInfo' plus the baker's 'CommissionRates'.
data FullBakerInfoEx = FullBakerInfoEx
    { _exFullBakerInfo :: !FullBakerInfo,
      _bakerPoolCommissionRates :: !CommissionRates
    }
    deriving (Eq, Show)

makeLenses ''FullBakerInfoEx

instance HasFullBakerInfo FullBakerInfoEx where
    fullBakerInfo = exFullBakerInfo

instance HasBakerInfo FullBakerInfoEx where
    bakerInfo = exFullBakerInfo . theBakerInfo

-- |An extended version of 'FullBakers' that includes commission rates.
-- This is used for handling rewards in P4 onwards.
data FullBakersEx = FullBakersEx
    { -- |All bakers in ascending order of BakerId.
      bakerInfoExs :: !(Vec.Vector FullBakerInfoEx),
      -- |The total stake of all bakers.
      bakerPoolTotalStake :: !Amount
    }
    deriving (Eq, Show)

data BakerKeyUpdate = BakerKeyUpdate
    { -- |New public sign key
      bkuSignKey :: !BakerSignVerifyKey,
      -- |New public aggregation key
      bkuAggregationKey :: !BakerAggregationVerifyKey,
      -- |New public election key
      bkuElectionKey :: !BakerElectionVerifyKey
    }
    deriving (Eq, Ord, Show)

data BakerKeyUpdateResult
    = -- |The keys were updated successfully
      BKUSuccess !BakerId
    | -- |The account is not currently a baker
      BKUInvalidBaker
    | -- |The aggregation key is a duplicate
      BKUDuplicateAggregationKey
    deriving (Eq, Ord, Show)

bakerKeyUpdateToInfo :: BakerId -> BakerKeyUpdate -> BakerInfo
bakerKeyUpdateToInfo _bakerIdentity BakerKeyUpdate{..} =
    BakerInfo
        { _bakerSignatureVerifyKey = bkuSignKey,
          _bakerAggregationVerifyKey = bkuAggregationKey,
          _bakerElectionVerifyKey = bkuElectionKey,
          ..
        }

data BakerStakeUpdateResult
    = -- |The stake was increased. (Takes effect in epoch after next.)
      BSUStakeIncreased !BakerId
    | -- |The stake was reduced, effective from the given epoch.
      BSUStakeReduced !BakerId !Epoch
    | -- |The stake was not changed. (Either no change was specified, or the amount was identical.)
      BSUStakeUnchanged !BakerId
    | -- |The specified baker was not valid.
      BSUInvalidBaker
    | -- |A stake change is already pending, so the change could not be made.
      BSUChangePending !BakerId
    | -- |Tried to update the stake under the threshold specified in current chain parameters.
      BSUStakeUnderThreshold
    deriving (Eq, Ord, Show)

data BakerRestakeEarningsUpdateResult
    = -- |The flag was updated.
      BREUUpdated !BakerId
    | -- |The specified baker was not valid.
      BREUInvalidBaker
    deriving (Eq, Ord, Show)

data BakerAdd = BakerAdd
    { -- |The keys for the baker.
      baKeys :: !BakerKeyUpdate,
      -- |The initial stake.
      baStake :: !Amount,
      -- |Whether to restake GTU earned from rewards.
      baStakeEarnings :: !Bool
    }
    deriving (Eq, Ord, Show)

data BakerAddResult
    = -- |Adding baker successful.
      BASuccess !BakerId
    | -- |Account unknown.
      BAInvalidAccount
    | -- |The account is already registered as a baker.
      BAAlreadyBaker !BakerId
    | -- |The aggregation key already exists.
      BADuplicateAggregationKey
    | -- |The stake is below the required threshold dictated by current chain parameters.
      BAStakeUnderThreshold
    deriving (Eq, Ord, Show)

-- |Data structure used to add/remove/update baker.
data BakerConfigure
    = -- |Add a baker, all fields are required.
      BakerConfigureAdd
        { bcaKeys :: !BakerKeyUpdate,
          bcaCapital :: !Amount,
          bcaRestakeEarnings :: !Bool,
          bcaOpenForDelegation :: !OpenStatus,
          bcaMetadataURL :: !UrlText,
          bcaTransactionFeeCommission :: !AmountFraction,
          bcaBakingRewardCommission :: !AmountFraction,
          bcaFinalizationRewardCommission :: !AmountFraction
        }
    | -- |Update baker with optional fields.
      BakerConfigureUpdate
        { -- |The timestamp of the current slot (slot time).
          bcuSlotTimestamp :: !Timestamp,
          bcuKeys :: !(Maybe BakerKeyUpdate),
          bcuCapital :: !(Maybe Amount),
          bcuRestakeEarnings :: !(Maybe Bool),
          bcuOpenForDelegation :: !(Maybe OpenStatus),
          bcuMetadataURL :: !(Maybe UrlText),
          bcuTransactionFeeCommission :: !(Maybe AmountFraction),
          bcuBakingRewardCommission :: !(Maybe AmountFraction),
          bcuFinalizationRewardCommission :: !(Maybe AmountFraction)
        }
    deriving (Eq, Show)

-- |A baker update change result from configure baker. Used to indicate whether the configure will cause
-- any changes to the baker's stake, keys, etc.
data BakerConfigureUpdateChange
    = BakerConfigureStakeIncreased !Amount
    | BakerConfigureStakeReduced !Amount
    | BakerConfigureRestakeEarnings !Bool
    | BakerConfigureOpenForDelegation !OpenStatus
    | BakerConfigureUpdateKeys !BakerKeyUpdate
    | BakerConfigureMetadataURL !UrlText
    | BakerConfigureTransactionFeeCommission !AmountFraction
    | BakerConfigureBakingRewardCommission !AmountFraction
    | BakerConfigureFinalizationRewardCommission !AmountFraction
    deriving (Eq, Show)

-- |Result of configure baker.
data BakerConfigureResult
    = -- |Configure baker successful.
      BCSuccess ![BakerConfigureUpdateChange] !BakerId
    | -- |Account unknown.
      BCInvalidAccount
    | -- |The aggregation key already exists.
      BCDuplicateAggregationKey !BakerAggregationVerifyKey
    | -- |The stake is below the required threshold dictated by current chain parameters.
      BCStakeUnderThreshold
    | -- |The finalization reward commission is not in the allowed range.
      BCFinalizationRewardCommissionNotInRange
    | -- |The baking reward commission is not in the allowed range.
      BCBakingRewardCommissionNotInRange
    | -- |The transaction fee commission is not in the allowed range.
      BCTransactionFeeCommissionNotInRange
    | -- |A change is already pending on this baker.
      BCChangePending
    | -- |This is not a valid baker.
      BCInvalidBaker
    deriving (Eq, Show)

-- |Result of remove baker.
data BakerRemoveResult
    = -- |The baker was removed, effective from the given epoch.
      BRRemoved !BakerId !Epoch
    | -- |This is not a valid baker.
      BRInvalidBaker
    | -- |A change is already pending on this baker.
      BRChangePending !BakerId
    deriving (Eq, Ord, Show)

-- |Data structure used to add/remove/update delegator.
data DelegationConfigure
    = -- |Add a delegator, all fields are required.
      DelegationConfigureAdd
        { dcaCapital :: !Amount,
          dcaRestakeEarnings :: !Bool,
          dcaDelegationTarget :: !DelegationTarget
        }
    | -- |Update delegator with optional fields.
      DelegationConfigureUpdate
        { -- |The timestamp of the current slot (slot time of the block in which the update occurs).
          dcuSlotTimestamp :: !Timestamp,
          dcuCapital :: !(Maybe Amount),
          dcuRestakeEarnings :: !(Maybe Bool),
          dcuDelegationTarget :: !(Maybe DelegationTarget)
        }
    deriving (Eq, Show)

-- |A delegation update change result from configure delegation. Used to indicate whether the
-- configure will cause any changes to the delegator's stake, restake earnings flag, etc.
data DelegationConfigureUpdateChange
    = DelegationConfigureStakeIncreased !Amount
    | DelegationConfigureStakeReduced !Amount
    | DelegationConfigureRestakeEarnings !Bool
    | DelegationConfigureDelegationTarget !DelegationTarget
    deriving (Eq, Show)

-- |Result of configure delegator.
data DelegationConfigureResult
    = -- |Configure delegation successful.
      DCSuccess ![DelegationConfigureUpdateChange] !DelegatorId
    | -- |Account unknown.
      DCInvalidAccount
    | -- |A change is already pending on this delegator.
      DCChangePending
    | -- |This is not a valid delegator.
      DCInvalidDelegator
    | -- |Delegation target is not a valid baker.
      DCInvalidDelegationTarget !BakerId
    | -- |The pool is not open for delegators.
      DCPoolClosed
    | -- |The pool's total capital would become too large.
      DCPoolStakeOverThreshold
    | -- |The delegated capital would become too large in comparison with pool owner's equity capital.
      DCPoolOverDelegated
    deriving (Eq, Show)
