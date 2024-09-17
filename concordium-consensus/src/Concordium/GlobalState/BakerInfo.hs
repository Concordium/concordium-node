{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.BakerInfo where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Genesis.Data
import Concordium.Scheduler.Types
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization

import Data.Ratio
import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform

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
    { -- | All bakers in ascending order of BakerId.
      fullBakerInfos :: !(Vec.Vector FullBakerInfo),
      -- | The total stake of all bakers.
      bakerTotalStake :: !Amount
    }
    deriving (Eq, Show)

-- | Serialize 'FullBakers'. This is used in updating the leadership election nonce for a new epoch.
--  The serialization format is:
--
--  * Number of bakers (64 bit, big endian)
--  * For each baker in ascending order of baker ID:
--      * Baker ID
--      * Election (VRF) public key
--      * Signature public key
--      * Aggregate signature public key
--      * Effective stake amount of the baker
putFullBakers :: Putter FullBakers
putFullBakers FullBakers{..} = do
    putLength (Vec.length fullBakerInfos)
    mapM_ put fullBakerInfos

-- | Look up a baker by its identifier.
--  This is implemented with binary search.
fullBaker :: FullBakers -> BakerId -> Maybe FullBakerInfo
fullBaker FullBakers{..} = binarySearch (_bakerIdentity . _theBakerInfo) fullBakerInfos

lotteryBaker :: FullBakers -> BakerId -> Maybe (BakerInfo, LotteryPower)
lotteryBaker fbs bid = lp <$> fullBaker fbs bid
  where
    lp fb = (fb ^. bakerInfo, fb ^. bakerStake % bakerTotalStake fbs)

-- | 'FullBakerInfo' plus the baker's 'CommissionRates'.
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

-- | An extended version of 'FullBakers' that includes commission rates.
--  This is used for handling rewards in P4 onwards.
data FullBakersEx = FullBakersEx
    { -- | All bakers in ascending order of BakerId.
      bakerInfoExs :: !(Vec.Vector FullBakerInfoEx),
      -- | The total stake of all bakers.
      bakerPoolTotalStake :: !Amount
    }
    deriving (Eq, Show)

data BakerKeyUpdate = BakerKeyUpdate
    { -- | New public sign key
      bkuSignKey :: !BakerSignVerifyKey,
      -- | New public aggregation key
      bkuAggregationKey :: !BakerAggregationVerifyKey,
      -- | New public election key
      bkuElectionKey :: !BakerElectionVerifyKey
    }
    deriving (Eq, Ord, Show)

-- | Extract the 'BakerKeyUpdate' from a 'BakerKeysWithProofs'.
bakerKeysWithoutProofs :: BakerKeysWithProofs -> BakerKeyUpdate
bakerKeysWithoutProofs BakerKeysWithProofs{..} =
    BakerKeyUpdate
        { bkuSignKey = bkwpSignatureVerifyKey,
          bkuAggregationKey = bkwpAggregationVerifyKey,
          bkuElectionKey = bkwpElectionVerifyKey
        }

data BakerKeyUpdateResult
    = -- | The keys were updated successfully
      BKUSuccess !BakerId
    | -- | The account is not currently a baker
      BKUInvalidBaker
    | -- | The aggregation key is a duplicate
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
    = -- | The stake was increased. (Takes effect in epoch after next.)
      BSUStakeIncreased !BakerId
    | -- | The stake was reduced, effective from the given epoch.
      BSUStakeReduced !BakerId !Epoch
    | -- | The stake was not changed. (Either no change was specified, or the amount was identical.)
      BSUStakeUnchanged !BakerId
    | -- | The specified baker was not valid.
      BSUInvalidBaker
    | -- | A stake change is already pending, so the change could not be made.
      BSUChangePending !BakerId
    | -- | Tried to update the stake under the threshold specified in current chain parameters.
      BSUStakeUnderThreshold
    deriving (Eq, Ord, Show)

data BakerRestakeEarningsUpdateResult
    = -- | The flag was updated.
      BREUUpdated !BakerId
    | -- | The specified baker was not valid.
      BREUInvalidBaker
    deriving (Eq, Ord, Show)

data BakerAdd = BakerAdd
    { -- | The keys for the baker.
      baKeys :: !BakerKeyUpdate,
      -- | The initial stake.
      baStake :: !Amount,
      -- | Whether to restake GTU earned from rewards.
      baStakeEarnings :: !Bool
    }
    deriving (Eq, Ord, Show)

data BakerAddResult
    = -- | Adding baker successful.
      BASuccess !BakerId
    | -- | Account unknown.
      BAInvalidAccount
    | -- | The account is already registered as a baker.
      BAAlreadyBaker !BakerId
    | -- | The aggregation key already exists.
      BADuplicateAggregationKey
    | -- | The stake is below the required threshold dictated by current chain parameters.
      BAStakeUnderThreshold
    deriving (Eq, Ord, Show)

-- | Result of remove baker.
data BakerRemoveResult
    = -- | The baker was removed, effective from the given epoch.
      BRRemoved !BakerId !Epoch
    | -- | This is not a valid baker.
      BRInvalidBaker
    | -- | A change is already pending on this baker.
      BRChangePending !BakerId
    deriving (Eq, Ord, Show)

-- | Parameters for adding a validator.
data ValidatorAdd = ValidatorAdd
    { -- | The keys for the validator.
      vaKeys :: !BakerKeyUpdate,
      -- | The initial stake.
      vaCapital :: !Amount,
      -- | Whether to restake earned rewards
      vaRestakeEarnings :: !Bool,
      -- | Whether the validator pool is open for delegation.
      vaOpenForDelegation :: !OpenStatus,
      -- | The metadata URL for the validator.
      vaMetadataURL :: !UrlText,
      -- | The commission rates for the validator.
      vaCommissionRates :: !CommissionRates
    }
    deriving (Eq, Show)

-- | Parameters for updating an existing validator. Where a field is 'Nothing', the field is not
--  updated.
data ValidatorUpdate = ValidatorUpdate
    { -- | The new keys for the validator.
      vuKeys :: !(Maybe BakerKeyUpdate),
      -- | The new capital for the validator. If this is @Just 0@, the validator is removed.
      vuCapital :: !(Maybe Amount),
      -- | Whether to restake earned rewards.
      vuRestakeEarnings :: !(Maybe Bool),
      -- | Whether the validator pool is open for delegation.
      vuOpenForDelegation :: !(Maybe OpenStatus),
      -- | The new metadata URL for the validator.
      vuMetadataURL :: !(Maybe UrlText),
      -- | The new transaction fee commission for the validator.
      vuTransactionFeeCommission :: !(Maybe AmountFraction),
      -- | The new baking reward commission for the validator.
      vuBakingRewardCommission :: !(Maybe AmountFraction),
      -- | The new finalization reward commission for the validator.
      vuFinalizationRewardCommission :: !(Maybe AmountFraction)
    }
    deriving (Eq, Show)

-- | A 'ValidatorUpdate' that removes the validator.
validatorRemove :: ValidatorUpdate
validatorRemove =
    ValidatorUpdate
        { vuKeys = Nothing,
          vuCapital = Just 0,
          vuRestakeEarnings = Nothing,
          vuOpenForDelegation = Nothing,
          vuMetadataURL = Nothing,
          vuTransactionFeeCommission = Nothing,
          vuBakingRewardCommission = Nothing,
          vuFinalizationRewardCommission = Nothing
        }

-- | Failure modes when configuring a validator.
data ValidatorConfigureFailure
    = -- | The stake is below the required threshold dictated by current chain parameters.
      VCFStakeUnderThreshold
    | -- | The transaction fee commission is not in the allowed range.
      VCFTransactionFeeCommissionNotInRange
    | -- | The baking reward commission is not in the allowed range.
      VCFBakingRewardCommissionNotInRange
    | -- | The finalization reward commission is not in the allowed range.
      VCFFinalizationRewardCommissionNotInRange
    | -- | The aggregation key is already in use by another validator.
      VCFDuplicateAggregationKey !BakerAggregationVerifyKey
    | -- | A change is already pending on this validator.
      VCFChangePending
    deriving (Eq, Show)

-- | A baker update change result from configure baker. Used to indicate whether the configure will cause
--  any changes to the baker's stake, keys, etc.
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

-- | Parameters for adding a delegator.
data DelegatorAdd = DelegatorAdd
    { -- | The initial staked capital for the delegator.
      daCapital :: !Amount,
      -- | Whether to restake earnings.
      daRestakeEarnings :: !Bool,
      -- | The delegation target for the delegator.
      daDelegationTarget :: !DelegationTarget
    }
    deriving (Eq, Show)

-- | Parameters for updating an existing delegator. Where a field is 'Nothing', the field is not
--  updated.
data DelegatorUpdate = DelegatorUpdate
    { -- | The new capital for the delegator. If this is @Just 0@, the delegator is removed.
      duCapital :: !(Maybe Amount),
      -- | Whether to restake earnings.
      duRestakeEarnings :: !(Maybe Bool),
      -- | The new delegation target for the delegator.
      duDelegationTarget :: !(Maybe DelegationTarget)
    }
    deriving (Eq, Show)

-- | A 'DelegatorUpdate' that removes the delegator.
delegatorRemove :: DelegatorUpdate
delegatorRemove =
    DelegatorUpdate
        { duCapital = Just 0,
          duRestakeEarnings = Nothing,
          duDelegationTarget = Nothing
        }

-- | A delegation update change result from configure delegation. Used to indicate whether the
--  configure will cause any changes to the delegator's stake, restake earnings flag, etc.
data DelegationConfigureUpdateChange
    = DelegationConfigureStakeIncreased !Amount
    | DelegationConfigureStakeReduced !Amount
    | DelegationConfigureRestakeEarnings !Bool
    | DelegationConfigureDelegationTarget !DelegationTarget
    deriving (Eq, Show)

-- | Failure modes for configuring a delegator.
data DelegatorConfigureFailure
    = -- | The delegation target is not a valid baker.
      DCFInvalidDelegationTarget !BakerId
    | -- | The pool is not open for delegators.
      DCFPoolClosed
    | -- | The pool's total capital would become too large.
      DCFPoolStakeOverThreshold
    | -- | The delegated capital would become too large in comparison with pool owner's equity
      -- capital.
      DCFPoolOverDelegated
    | -- | A change is already pending on this delegator.
      DCFChangePending
    deriving (Eq, Show)

-- | Construct an 'AccountBaker' from a 'GenesisBaker'.
--  For 'P4', this creates the baker with the initial pool status being open for all, the
--  empty metadata URL and the maximum commission rates allowable under the chain parameters.
genesisBakerInfo :: forall pv. SProtocolVersion pv -> ChainParameters pv -> GenesisBaker -> AccountBaker (AccountVersionFor pv)
genesisBakerInfo spv cp baker@GenesisBaker{..} = AccountBaker{..}
  where
    _stakedAmount = gbStake
    _stakeEarnings = gbRestakeEarnings
    _accountBakerInfo = genesisBakerInfoEx spv cp baker
    _bakerPendingChange = NoChange

-- | Construct an 'BakerInfoEx' from a 'GenesisBaker'.
--  For 'P4', this creates the baker with the initial pool status being open for all, the
--  empty metadata URL and the maximum commission rates allowable under the chain parameters.
genesisBakerInfoEx :: forall pv. SProtocolVersion pv -> ChainParameters pv -> GenesisBaker -> BakerInfoEx (AccountVersionFor pv)
genesisBakerInfoEx spv cp GenesisBaker{..} = case spv of
    SP1 -> BakerInfoExV0 bkrInfo
    SP2 -> BakerInfoExV0 bkrInfo
    SP3 -> BakerInfoExV0 bkrInfo
    SP4 -> binfoV1
    SP5 -> binfoV1
    SP6 -> binfoV1
    SP7 -> binfoV1
    SP8 -> binfoV1
  where
    bkrInfo =
        BakerInfo
            { _bakerIdentity = gbBakerId,
              _bakerSignatureVerifyKey = gbSignatureVerifyKey,
              _bakerElectionVerifyKey = gbElectionVerifyKey,
              _bakerAggregationVerifyKey = gbAggregationVerifyKey
            }
    binfoV1 :: (PVSupportsDelegation pv, PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1) => BakerInfoEx (AccountVersionFor pv)
    binfoV1 =
        BakerInfoExV1
            bkrInfo
            BakerPoolInfo
                { _poolOpenStatus = OpenForAll,
                  _poolMetadataUrl = emptyUrlText,
                  _poolCommissionRates = cp ^. cpPoolParameters . ppCommissionBounds . to maximumCommissionRates
                }
