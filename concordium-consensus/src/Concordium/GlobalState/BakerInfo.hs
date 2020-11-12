{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies #-}
module Concordium.GlobalState.BakerInfo where

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

data BakerKeyUpdate = BakerKeyUpdate {
  -- |New public sign key
  bkuSignKey :: !BakerSignVerifyKey,
  -- |New public aggregation key
  bkuAggregationKey :: !BakerAggregationVerifyKey,
  -- |New public election key
  bkuElectionKey :: !BakerElectionVerifyKey
}

data BakerKeyUpdateResult
  = BKUSuccess
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

data BakerStakeUpdate = BakerStakeUpdate {
  -- |The new desired stake. 'Nothing' if no change required.
  bsuNewStake :: !(Maybe Amount),
  -- |Whether to restake any earnings. 'Nothing' if no change required.
  bsuStakeEarnings :: !(Maybe Bool)
}

data BakerStakeUpdateResult
  = BSUStakeIncreased
  -- ^The stake was increased. (Takes effect in epoch after next.)
  | BSUStakeReduced !Epoch
  -- ^The stake was reduced. (Takes effect 1 epoch after the given epoch.)
  | BSUStakeUnchanged
  -- ^The stake was not changed. (Either no change was specified, or the amount was identical.)
  | BSUInvalidBaker
  -- ^The specified baker was not valid.
  | BSUChangePending
  -- ^A stake change is already pending, so the change could not be made.
  | BSUInsufficientBalance
  -- ^The balance of the account is not sufficient for the new stake.
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
  | BAAlreadyBaker
  -- ^The account is already registered as a baker.
  | BAInsufficientBalance
  -- ^The balance was less than the specified stake.
  | BADuplicateAggregationKey
  -- ^The aggregation key already exists.
  deriving (Eq, Ord, Show)
data BakerRemoveResult
  = BRRemoved !Epoch
  -- ^The baker was removed, effective 1 epoch after the given epoch.
  | BRInvalidBaker
  -- ^This is not a valid baker.
  | BRChangePending
  -- ^A change is already pending on this baker.
  deriving (Eq, Ord, Show)
