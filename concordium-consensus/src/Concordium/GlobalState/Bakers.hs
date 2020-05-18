{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies #-}
module Concordium.GlobalState.Bakers where

import GHC.Generics
import Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.List as List
import Data.Serialize
import Data.Ratio
import Lens.Micro.Platform
import Concordium.Utils

import Concordium.Types

data BakerCreationInfo = BakerCreationInfo !BakerElectionVerifyKey !BakerSignVerifyKey !BakerAggregationVerifyKey !AccountAddress

data BakerError =
      DuplicateSignKey
    | DuplicateAggregationKey
  deriving (Eq, Show)

data BakerInfo = BakerInfo {
    -- |The baker's public VRF key
    _bakerElectionVerifyKey :: !BakerElectionVerifyKey,
    -- |The baker's public signature key
    _bakerSignatureVerifyKey :: !BakerSignVerifyKey,
    -- |The baker's public key for finalization record aggregation
    _bakerAggregationVerifyKey :: !BakerAggregationVerifyKey,
    -- |The stake delegated to the baker
    _bakerStake :: !Amount,
    -- |The account associated with the baker
    _bakerAccount :: !AccountAddress
} deriving (Eq, Generic, Show)
instance Serialize BakerInfo

makeLenses ''BakerInfo

data Bakers = Bakers {
    -- |The bakers, indexed by 'BakerId'
    _bakerMap :: !(Map BakerId BakerInfo),
    -- |The baker ids indexed by the keys
    _bakersByKey :: !(Map BakerSignVerifyKey BakerId),
    -- |The total stake delegated to all bakers
    _bakerTotalStake :: !Amount,
    -- |The next 'BakerId' to use for a new baker.
    -- 'BakerId's should not be reused when bakers are removed.
    _nextBakerId :: !BakerId,
    -- |Aggregation keys in use
    _aggregationKeys :: !(Set BakerAggregationVerifyKey)
} deriving (Eq, Generic, Show)

makeLenses ''Bakers

instance Serialize Bakers where
    put Bakers{..} = put _bakerMap >> put _nextBakerId
    get = do
        _bakerMap <- get
        _nextBakerId <- get
        let
            (_bakersByKey, _bakerTotalStake, _aggregationKeys) = Map.foldrWithKey deriv (Map.empty, 0, Set.empty) _bakerMap
            deriv bid BakerInfo{..} (m, t, aks) = (m & at' (_bakerSignatureVerifyKey) ?~ bid,
                                                t + _bakerStake,
                                                Set.insert _bakerAggregationVerifyKey aks)
        return Bakers{..}

emptyBakers :: Bakers
emptyBakers = Bakers Map.empty Map.empty 0 0 Set.empty

-- |Make bakers from a list and assign them sequential identities.
-- NB: Only the first baker with the given signing key is used.
-- NB: Likewise, only the first baker with the given aggregation key is used
-- The bakers which were not added are returned.
bakersFromList :: [BakerInfo] -> (Bakers, [BakerInfo])
bakersFromList bkrs = (
  Bakers {
      _bakerMap = Map.fromList bakerList,
        ..
      },
    duplicateBakers
  )
    where
      (_bakersByKey, bakerList, duplicateBakers, _nextBakerId, _bakerTotalStake, _aggregationKeys) =
        List.foldl' (\(known, bkrList, duplicate, nextId, totalStake, aggKeys) baker ->
                        let key = baker ^. bakerSignatureVerifyKey
                            aggKey = baker ^. bakerAggregationVerifyKey in
                          case Map.lookup key known of
                            Nothing -> -- new baker key
                              if Set.member aggKey aggKeys then
                                (known, bkrList, baker:duplicate, nextId, totalStake, aggKeys)
                              else
                                (Map.insert key nextId known, (nextId, baker):bkrList, duplicate, nextId+1, totalStake + baker ^. bakerStake, insert aggKey aggKeys)
                            Just _ -> (known, bkrList, baker:duplicate, nextId, totalStake, aggKeys)
                    )
                    (Map.empty, [], [], 0, 0, Set.empty)
                    bkrs

bakerData :: BakerId -> Bakers -> Maybe (BakerInfo, LotteryPower)
bakerData bid bkrs = (bkrs ^. bakerMap . at' bid) <&>
                        \bkr -> (bkr, (bkr ^. bakerStake) % (bkrs ^. bakerTotalStake))

-- |Add a baker to the set of known bakers.
-- If a baker with the given signing key already exists then return Right DuplicateSignKey,
-- If a baker with the given aggregation key already exists, return Left DuplicateAggregationKey,
-- otherwise assign it a fresh id and add it to the set of known bakers.a
createBaker :: BakerCreationInfo -> Bakers -> Either (BakerId, Bakers) BakerError
createBaker (BakerCreationInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAggregationVerifyKey _bakerAccount) bkrs =
  case bkrs ^. bakersByKey . at' _bakerSignatureVerifyKey of
    Nothing -> -- key does not yet exist
        if Set.member _bakerAggregationVerifyKey (bkrs ^. aggregationKeys) then
          Right DuplicateAggregationKey
        else -- aggregation keys is not already in use, so we insert baker
          Left (bid, bkrs
                     & bakerMap . at' bid ?~ BakerInfo{..}
                     & bakersByKey . at' _bakerSignatureVerifyKey ?~ bid
                     & nextBakerId .~ bid + 1
                     & aggregationKeys %~ insert _bakerAggregationVerifyKey)
            where
              _bakerStake = 0
              bid = _nextBakerId bkrs
    Just _ -> Right DuplicateSignKey

data BakerUpdate = BakerUpdate {
  -- |Identity of the baker to update.
  _buId :: !BakerId,
  -- |Optionally update the baker's reward account.
  _buAccount :: !(Maybe AccountAddress),
  -- |Optionally update the baker's public verification key.
  _buSignKey :: !(Maybe BakerSignVerifyKey),
  -- |Optionally update the baker's aggregation verification key
  _buAggregationKey :: !(Maybe BakerAggregationVerifyKey)
}

makeLenses ''BakerUpdate

emptyBakerUpdate :: BakerId -> BakerUpdate
emptyBakerUpdate bid = BakerUpdate bid Nothing Nothing Nothing

-- |Update a given baker.
-- If this would lead to duplicate baker signing keys return 'Nothing'.
-- If this would result in duplicate aggregation keys return 'Nothing'.
-- If the baker with the given id does not exist this function returns the
-- original 'Bakers' object.
updateBaker :: BakerUpdate -> Bakers -> Maybe Bakers
updateBaker !BakerUpdate{..} !bakers =
  case bakers ^? bakerMap . ix _buId of
    Nothing -> Just bakers
    Just binfo -> do
        (bakers', binfo') <- handleUpdateAggregationKey binfo _buAggregationKey bakers
        (bakers'', binfo'') <- handleUpdateSignKey binfo' _buSignKey bakers'
        handleUpdateBakerAccount binfo'' _buAccount bakers''
          where
            handleUpdateAggregationKey binfo aggKeyUpdate bs = case aggKeyUpdate of
              Nothing -> Just (bs, binfo)
              Just newAggKey ->
                if Set.member newAggKey (bakers ^. aggregationKeys) then
                  Nothing
                else
                  let oldAggKey = binfo ^. bakerAggregationVerifyKey
                      newBakerInfo = binfo & bakerAggregationVerifyKey .~ newAggKey
                  in Just
                      (bs
                        & aggregationKeys %~ Set.delete oldAggKey -- delete old aggregation key from pool of aggregation keys in use
                        & aggregationKeys %~ Set.insert newAggKey -- add new aggregation key to pool
                        & bakerMap %~ Map.insert _buId newBakerInfo, -- update the bakermap with the updated bakerinfo
                      newBakerInfo)
            handleUpdateSignKey binfo signKeyUpdate bs = case signKeyUpdate of
               Nothing -> -- don't update the sign key, no clash possible
                 Just (bs, binfo)
               Just newSignKey ->
                 -- new signing key, make sure it is new
                 case bs ^. bakersByKey . at' newSignKey of
                   Just _ -> -- existing signing key
                     Nothing
                   Nothing -> -- fresh signing key
                     let newBakerInfo = binfo & bakerSignatureVerifyKey .~ newSignKey
                     in Just (bs
                             & bakerMap %~ Map.insert _buId newBakerInfo
                             & bakersByKey . at' (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                             & bakersByKey . at' newSignKey .~ Just _buId, -- and add new identification
                          newBakerInfo)
            handleUpdateBakerAccount binfo accountUpdate bs = case accountUpdate of
                Nothing -> Just bs
                Just newBakerAccount ->
                  Just $ bs & bakerMap %~ Map.insert _buId (binfo & bakerAccount .~ newBakerAccount)

removeBaker :: BakerId -> Bakers -> (Bool, Bakers)
removeBaker bid !bakers =
    case bakers ^. bakerMap . at' bid of
        Nothing -> (False, bakers)
        Just bkr -> (True, bakers
                            & (bakerMap . at' bid .~ Nothing)
                            & bakersByKey . at' (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as wel.
                            & (bakerTotalStake %~ subtract (bkr ^. bakerStake))
                            & aggregationKeys %~ Set.delete (bkr ^. bakerAggregationVerifyKey))

modifyStake :: Maybe BakerId -> AmountDelta -> Bakers -> Bakers
modifyStake (Just bid) delta bakers = case bakers ^. bakerMap . at' bid of
        Nothing -> bakers
        Just _ -> bakers & bakerMap . ix bid . bakerStake %~ applyAmountDelta delta
                    & bakerTotalStake %~ applyAmountDelta delta
modifyStake _ _ bakers = bakers

addStake :: Maybe BakerId -> Amount -> Bakers -> Bakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: Maybe BakerId -> Amount -> Bakers -> Bakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)
