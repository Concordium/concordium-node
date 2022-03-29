{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.BakerInfo where

import Data.Ratio
import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.Accounts
import Concordium.Utils.BinarySearch

import Concordium.Types
import Concordium.Types.Execution (OpenStatus, DelegationTarget)

data FullBakerInfo = FullBakerInfo {
    _theBakerInfo :: !BakerInfo,
    _bakerStake :: !Amount
} deriving (Eq, Show)

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

data FullBakers = FullBakers {
    -- |All bakers in ascending order of BakerId.
    fullBakerInfos :: !(Vec.Vector FullBakerInfo),
    -- |The total stake of all bakers.
    bakerTotalStake :: !Amount
} deriving (Eq, Show)

-- |Look up a baker by its identifier.
-- This is implemented with binary search.
fullBaker :: FullBakers -> BakerId -> Maybe FullBakerInfo
fullBaker FullBakers{..} = binarySearch (_bakerIdentity . _theBakerInfo) fullBakerInfos

lotteryBaker :: FullBakers -> BakerId -> Maybe (BakerInfo, LotteryPower)
lotteryBaker fbs bid = lp <$> fullBaker fbs bid
    where
      lp fb = (fb ^. bakerInfo, fb ^. bakerStake % bakerTotalStake fbs)

-- |'FullBakerInfo' plus the baker's 'CommissionRates'.
data FullBakerInfoEx = FullBakerInfoEx {
    _exFullBakerInfo :: !FullBakerInfo,
    _bakerPoolCommissionRates :: !CommissionRates
  } deriving (Eq, Show)

makeLenses ''FullBakerInfoEx

instance HasFullBakerInfo FullBakerInfoEx where
  fullBakerInfo = exFullBakerInfo

instance HasBakerInfo FullBakerInfoEx where
  bakerInfo = exFullBakerInfo . theBakerInfo

-- |An extended version of 'FullBakers' that includes commission rates.
-- This is used for handling rewards in P4 onwards.
data FullBakersEx = FullBakersEx {
  -- |All bakers in ascending order of BakerId.
  bakerInfoExs :: !(Vec.Vector FullBakerInfoEx),
  -- |The total stake of all bakers.
  bakerPoolTotalStake :: !Amount
} deriving (Eq, Show)

data BakerKeyUpdate = BakerKeyUpdate {
  -- |New public sign key
  bkuSignKey :: !BakerSignVerifyKey,
  -- |New public aggregation key
  bkuAggregationKey :: !BakerAggregationVerifyKey,
  -- |New public election key
  bkuElectionKey :: !BakerElectionVerifyKey
} deriving (Eq, Ord, Show)

data BakerKeyUpdateResult
  = BKUSuccess !BakerId
  -- ^The keys were updated successfully
  | BKUInvalidBaker
  -- ^The account is not currently a baker
  | BKUDuplicateAggregationKey
  -- ^The aggregation key is a duplicate
  deriving (Eq, Ord, Show)

bakerKeyUpdateToInfo :: BakerId -> BakerKeyUpdate -> BakerInfo
bakerKeyUpdateToInfo _bakerIdentity BakerKeyUpdate{..} = BakerInfo {
      _bakerSignatureVerifyKey = bkuSignKey,
      _bakerAggregationVerifyKey = bkuAggregationKey,
      _bakerElectionVerifyKey = bkuElectionKey,
      ..
    }

data BakerStakeUpdateResult
  = BSUStakeIncreased !BakerId
  -- ^The stake was increased. (Takes effect in epoch after next.)
  | BSUStakeReduced !BakerId !Epoch
  -- ^The stake was reduced, effective from the given epoch.
  | BSUStakeUnchanged !BakerId
  -- ^The stake was not changed. (Either no change was specified, or the amount was identical.)
  | BSUInvalidBaker
  -- ^The specified baker was not valid.
  | BSUChangePending !BakerId
  -- ^A stake change is already pending, so the change could not be made.
  | BSUStakeUnderThreshold
  -- ^Tried to update the stake under the threshold specified in current chain parameters.
  deriving (Eq, Ord, Show)

data BakerRestakeEarningsUpdateResult
  = BREUUpdated !BakerId
  -- ^The flag was updated.
  | BREUInvalidBaker
  -- ^The specified baker was not valid.
  deriving (Eq, Ord, Show)

data BakerAdd = BakerAdd {
  -- |The keys for the baker.
  baKeys :: !BakerKeyUpdate,
  -- |The initial stake.
  baStake :: !Amount,
  -- |Whether to restake GTU earned from rewards.
  baStakeEarnings :: !Bool
} deriving (Eq, Ord, Show)

data BakerAddResult
  = BASuccess !BakerId
  -- ^Adding baker successful.
  | BAInvalidAccount
  -- ^Account unknown.
  | BAAlreadyBaker !BakerId
  -- ^The account is already registered as a baker.
  | BADuplicateAggregationKey
  -- ^The aggregation key already exists.
  | BAStakeUnderThreshold
  -- ^The stake is below the required threshold dictated by current chain parameters.
  deriving (Eq, Ord, Show)

-- |Data structure used to add/remove/update baker.
data BakerConfigure =
    -- |Add a baker, all fields are required.
    BakerConfigureAdd {
        bcaKeys :: !BakerKeyUpdate,
        bcaCapital :: !Amount,
        bcaRestakeEarnings :: !Bool,
        bcaOpenForDelegation :: !OpenStatus,
        bcaMetadataURL :: !UrlText,
        bcaTransactionFeeCommission :: !AmountFraction,
        bcaBakingRewardCommission :: !AmountFraction,
        bcaFinalizationRewardCommission :: !AmountFraction
    }
    -- |Update baker with optional fields.
  | BakerConfigureUpdate {
        -- |The timestamp of the current slot (slot time).
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
data BakerConfigureUpdateChange =
    BakerConfigureStakeIncreased !Amount
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
  = BCSuccess ![BakerConfigureUpdateChange] !BakerId
    -- ^Configure baker successful.
  | BCInvalidAccount
  -- ^Account unknown.
  | BCDuplicateAggregationKey !BakerAggregationVerifyKey
  -- ^The aggregation key already exists.
  | BCStakeUnderThreshold
  -- ^The stake is below the required threshold dictated by current chain parameters.
  | BCFinalizationRewardCommissionNotInRange
  -- ^The finalization reward commission is not in the allowed range.
  | BCBakingRewardCommissionNotInRange
  -- ^The baking reward commission is not in the allowed range.
  | BCTransactionFeeCommissionNotInRange
  -- ^The transaction fee commission is not in the allowed range.
  | BCChangePending
  -- ^A change is already pending on this baker.
  | BCInvalidBaker
  -- ^This is not a valid baker.
  deriving (Eq, Show)

-- |Result of remove baker.
data BakerRemoveResult
  = BRRemoved !BakerId !Epoch
  -- ^The baker was removed, effective from the given epoch.
  | BRInvalidBaker
  -- ^This is not a valid baker.
  | BRChangePending !BakerId
  -- ^A change is already pending on this baker.
  deriving (Eq, Ord, Show)

-- |Data structure used to add/remove/update delegator.
data DelegationConfigure =
    -- |Add a delegator, all fields are required.
    DelegationConfigureAdd {
      dcaCapital :: !Amount,
      dcaRestakeEarnings :: !Bool,
      dcaDelegationTarget :: !DelegationTarget
    }
    -- |Update delegator with optional fields.
  | DelegationConfigureUpdate {
      -- |The timestamp of the current slot (slot time).
      dcuSlotTimestamp :: !Timestamp,
      dcuCapital :: !(Maybe Amount),
      dcuRestakeEarnings :: !(Maybe Bool),
      dcuDelegationTarget :: !(Maybe DelegationTarget)
  }
  deriving (Eq, Show)

-- |A delegation update change result from configure delegation. Used to indicate whether the
-- configure will cause any changes to the delegator's stake, restake earnings flag, etc.
data DelegationConfigureUpdateChange =
    DelegationConfigureStakeIncreased !Amount
  | DelegationConfigureStakeReduced !Amount
  | DelegationConfigureRestakeEarnings !Bool
  | DelegationConfigureDelegationTarget !DelegationTarget
  deriving (Eq, Show)

-- |Result of configure delegator.
data DelegationConfigureResult
  = DCSuccess ![DelegationConfigureUpdateChange] !DelegatorId
    -- ^Configure delegation successful.
  | DCInvalidAccount
    -- ^Account unknown.
  | DCChangePending
  -- ^A change is already pending on this delegator.
  | DCInvalidDelegator
  -- ^This is not a valid delegator.
  | DCInvalidDelegationTarget !BakerId
  -- ^Delegation target is not a valid baker.
  | DCPoolClosed
  -- ^The pool is not open for delegators.
  | DCPoolStakeOverThreshold
  -- ^The pool's total capital would become too large.
  | DCPoolOverDelegated
  -- ^The delegated capital would become too large in comparison with pool owner's equity capital.
  deriving (Eq, Show)
