{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies #-}
module Concordium.GlobalState.BakerInfo where

import Data.Ratio
import Data.Serialize
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H

import Concordium.Types

data BakerInfo = BakerInfo {
    -- |Identity of the baker. This is actually the account index of
    -- the account controlling the baker.
    _bakerIdentity :: !BakerId,
    -- |The baker's public VRF key
    _bakerElectionVerifyKey :: !BakerElectionVerifyKey,
    -- |The baker's public signature key
    _bakerSignatureVerifyKey :: !BakerSignVerifyKey,
    -- |The baker's public key for finalization record aggregation
    _bakerAggregationVerifyKey :: !BakerAggregationVerifyKey
} deriving (Eq, Show)

instance Serialize BakerInfo where
  put BakerInfo{..} = do
    put _bakerIdentity
    put _bakerElectionVerifyKey
    put _bakerSignatureVerifyKey
    put _bakerAggregationVerifyKey
  get = do
    _bakerIdentity <- get
    _bakerElectionVerifyKey <- get
    _bakerSignatureVerifyKey <- get
    _bakerAggregationVerifyKey <- get
    return BakerInfo{..}

makeLenses ''BakerInfo

data FullBakerInfo = FullBakerInfo {
    _bakerInfo :: !BakerInfo,
    _bakerStake :: !Amount
} deriving (Eq, Show)

instance Serialize FullBakerInfo where
  put FullBakerInfo{..} = do
    put _bakerInfo
    put _bakerStake
  get = do
    _bakerInfo <- get
    _bakerStake <- get
    return FullBakerInfo{..}

makeLenses ''FullBakerInfo

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
                in case compare bid (_bakerIdentity (_bakerInfo bi)) of
                  LT -> binSearch lowIndex (midIndex - 1)
                  EQ -> Just bi
                  GT -> binSearch (midIndex + 1) highIndex
          EQ -> let bi = fullBakerInfos Vec.! lowIndex in
                if _bakerIdentity (_bakerInfo bi) == bid then
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
}

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
}

data BakerAddResult
  = BASuccess !BakerId
  -- ^Adding baker successful.
  | BAInvalidAccount
  -- ^Account unknown.
  | BAAlreadyBaker !BakerId
  -- ^The account is already registered as a baker.
  | BADuplicateAggregationKey
  -- ^The aggregation key already exists.
  deriving (Eq, Ord, Show)

data BakerRemoveResult
  = BRRemoved !BakerId !Epoch
  -- ^The baker was removed, effective from the given epoch.
  | BRInvalidBaker
  -- ^This is not a valid baker.
  | BRChangePending !BakerId
  -- ^A change is already pending on this baker.
  deriving (Eq, Ord, Show)