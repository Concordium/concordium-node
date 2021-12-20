{-# LANGUAGE BangPatterns #-}
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

makeLenses ''FullBakerInfo

instance HasBakerInfo FullBakerInfo where
  bakerInfo = theBakerInfo
instance HashableTo H.Hash FullBakerInfo where
  getHash = H.hash . encode

data FullBakers = FullBakers {
    -- |All bakers in ascending order of BakerId.
    fullBakerInfos :: !(Vec.Vector FullBakerInfo),
    -- |The total stake of all bakers.
    bakerTotalStake :: !Amount
} deriving Eq

-- |Look up a baker by its identifier.
-- This is implemented with binary search.
fullBaker :: FullBakers -> BakerId -> Maybe FullBakerInfo
fullBaker FullBakers{..} bid = binSearch 0 (Vec.length fullBakerInfos - 1)
    where
      binSearch lowIndex highIndex = case compare lowIndex highIndex of
          LT -> let
                  midIndex = lowIndex + (highIndex - lowIndex) `div` 2
                  bi = fullBakerInfos Vec.! midIndex
                in case compare bid (_bakerIdentity (_theBakerInfo bi)) of
                  LT -> binSearch lowIndex (midIndex - 1)
                  EQ -> Just bi
                  GT -> binSearch (midIndex + 1) highIndex
          EQ -> let bi = fullBakerInfos Vec.! lowIndex in
                if _bakerIdentity (_theBakerInfo bi) == bid then
                  Just bi
                else
                  Nothing
          GT -> Nothing

lotteryBaker :: FullBakers -> BakerId -> Maybe (BakerInfo, LotteryPower)
lotteryBaker fbs bid = lp <$> fullBaker fbs bid
    where
      lp fb = (fb ^. bakerInfo, fb ^. bakerStake % bakerTotalStake fbs)

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

-- TODO: Fix and Document
data BakerConfigure =
    BakerConfigureAdd {
        bcaKeys :: !BakerKeyUpdate,
        bcaCapital :: !Amount,
        bcaRestakeEarnings :: !Bool,
        bcaOpenForDelegation :: !OpenStatus,
        bcaMetadataURL :: !UrlText,
        bcaTransactionFeeCommission :: !RewardFraction,
        bcaBakingRewardCommission :: !RewardFraction,
        bcaFinalizationRewardCommission :: !RewardFraction
    }
  | BakerConfigureRemove {
    bcrTimestamp :: !Timestamp,
    bcrSlotDuration :: !Duration
  }
  | BakerConfigureUpdate {
        bcuTimestamp :: !Timestamp,
        bcuSlotDuration :: !Duration,
        bcuKeys :: !(Maybe BakerKeyUpdate),
        bcuCapital :: !(Maybe Amount),
        bcuRestakeEarnings :: !(Maybe Bool),
        bcuOpenForDelegation :: !(Maybe OpenStatus),
        bcuMetadataURL :: !(Maybe UrlText),
        bcuTransactionFeeCommission :: !(Maybe RewardFraction),
        bcuBakingRewardCommission :: !(Maybe RewardFraction),
        bcuFinalizationRewardCommission :: !(Maybe RewardFraction)
    }

-- |A baker update change result from configure baker. Used to indicate whether the configure will cause
-- any changes to the baker's stake, keys, etc.
data BakerConfigureUpdateChange =
    BakerConfigureStakeIncreased !Amount
  | BakerConfigureStakeReduced !Amount
  | BakerConfigureRestakeEarnings !Bool
  | BakerConfigureOpenForDelegation !OpenStatus
  | BakerConfigureUpdateKeys !BakerKeyUpdate
  | BakerConfigureMetadataURL !UrlText
  | BakerConfigureTransactionFeeCommission !RewardFraction
  | BakerConfigureBakingRewardCommission !RewardFraction
  | BakerConfigureFinalizationRewardCommission !RewardFraction
  deriving (Eq, Show)

-- TODO: Document
data BakerConfigureResult
  = BCSuccess ![BakerConfigureUpdateChange] !BakerId
    -- ^Configure baker successful.
  | BCInvalidAccount
  -- ^Account unknown.
  | BCAlreadyBaker !BakerId
  -- ^The account is already registered as a baker.
  | BCAlreadyDelegator !BakerId
  -- ^The account is already registered as a delegator.
  | BCDuplicateAggregationKey !BakerAggregationVerifyKey
  -- ^The aggregation key already exists.
  | BCStakeUnderThreshold
  -- ^The stake is below the required threshold dictated by current chain parameters.
  | BCCommissionNotInRange
  -- ^The commission is not in the allowed range.
  | BCChangePending
  -- ^A change is already pending on this baker.
  | BCInvalidBaker
  -- ^This is not a valid baker.
  deriving (Eq, Show)

data BakerRemoveResult
  = BRRemoved !BakerId !Epoch
  -- ^The baker was removed, effective from the given epoch.
  | BRInvalidBaker
  -- ^This is not a valid baker.
  | BRChangePending !BakerId
  -- ^A change is already pending on this baker.
  deriving (Eq, Ord, Show)

-- TODO: Fix and Document
data DelegationConfigure =
    DelegationConfigureAdd {
      dcaCapital :: !Amount,
      dcaRestakeEarnings :: !Bool,
      dcaDelegationTarget :: !DelegationTarget
    }
  | DelegationConfigureUpdate {
      dcuTimestamp :: !Timestamp,
      dcuSlotDuration :: !Duration,
      dcuCapital :: !(Maybe Amount),
      dcuRestakeEarnings :: !(Maybe Bool),
      dcuDelegationTarget :: !(Maybe DelegationTarget)
  }
  | DelegationConfigureRemove {
      dcrTimestamp :: !Timestamp,
      dcrSlotDuration :: !Duration
  }
  deriving (Eq, Show)

-- TODO: Fix and Document
data DelegationConfigureResult = DelegationConfigureResult
  deriving (Eq, Ord, Show)
