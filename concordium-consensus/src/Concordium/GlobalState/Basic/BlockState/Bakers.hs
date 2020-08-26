{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Basic.BlockState.Bakers where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Serialize
import Data.Ratio
import Lens.Micro.Platform
import Concordium.Utils
import Data.Maybe

import Concordium.GlobalState.BakerInfo
import Concordium.Types

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Basic.BlockState.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as L
import Concordium.Types.HashableTo

data Bakers = Bakers {
     -- | The bakers, indexed by 'BakerId'
     _bakerMap :: !(LFMBTree BakerId (Maybe FullBakerInfo)),
     -- | The baker ids indexed by the keys
     _bakersByKey :: !(Map BakerSignVerifyKey BakerId),
     -- | The total stake delegated to all bakers
     _bakerTotalStake :: !Amount,
     -- | Aggregation keys in use
     _aggregationKeys :: !(Set BakerAggregationVerifyKey)
} deriving (Eq, Show)

makeLenses ''Bakers

instance Serialize Bakers where
  put Bakers {..} = put _bakerMap
  get = do
    _bakerMap <- get
    let
        (_bakersByKey, _bakerTotalStake, _aggregationKeys) = foldr deriv (Map.empty, 0, Set.empty) [(i, v) | (i, Just v) <- L.toAscPairList _bakerMap]
        deriv (bid, FullBakerInfo {_bakerInfo = BakerInfo {..}, ..}) (m, t, aks) = ( m & at' _bakerSignatureVerifyKey ?~ bid,
                                               t + _bakerStake,
                                               Set.insert _bakerAggregationVerifyKey aks)
    return Bakers {..}

instance HashableTo H.Hash Bakers where
  getHash Bakers {..} = getHash _bakerMap

emptyBakers :: Bakers
emptyBakers = Bakers L.empty Map.empty 0 Set.empty

-- |Make bakers from a list and assign them sequential identities.
-- NB: Only the first baker with the given signing key is used.
-- NB: Likewise, only the first baker with the given aggregation key is used.
-- The bakers which were not added are returned.
bakersFromList :: [FullBakerInfo] -> (Bakers, [FullBakerInfo])
bakersFromList bkrs = (
  Bakers {
        _bakerMap = L.fromList $ map Just bakers,
        ..
      },
    duplicateBakers
  )
    where
      (_bakersByKey, duplicateBakers, _nextBakerId, _bakerTotalStake, _aggregationKeys, bakers) =
        List.foldl' (\(known, duplicate, nextId, totalStake, aggKeys, uniqueBakers) baker ->
                        let binfo = baker ^. bakerInfo
                            key = binfo ^. bakerSignatureVerifyKey
                            aggKey = binfo ^. bakerAggregationVerifyKey
                         in case Map.lookup key known of
                              Nothing ->
                                -- new baker key
                                -- if an aggregation key already exists skip the baker (mark it as duplicate)
                                if Set.member aggKey aggKeys then
                                  (known, baker : duplicate, nextId, totalStake, aggKeys, uniqueBakers)
                                else
                                  (Map.insert key nextId known, duplicate, nextId + 1, totalStake + baker ^. bakerStake, Set.insert aggKey aggKeys, baker : uniqueBakers)
                              Just _ -> (known, baker : duplicate, nextId, totalStake, aggKeys, uniqueBakers)
          )
          (Map.empty, [], 0, 0, Set.empty, [])
          bkrs

bakerData :: BakerId -> Bakers -> Maybe (BakerInfo, LotteryPower)
bakerData bid bkrs = L.lookupMaybe bid (bkrs ^. bakerMap) <&>
                                 \bkr -> (bkr ^. bakerInfo, (bkr ^. bakerStake) % (bkrs ^. bakerTotalStake))

-- |Add a baker to the set of known bakers.
-- If a baker with the given signing key already exists then return Right DuplicateSignKey,
-- If a baker with the given aggregation key already exists, return Left DuplicateAggregationKey,
-- otherwise assign it a fresh id and add it to the set of known bakers.a
createBaker :: BakerInfo -> Bakers -> Either BakerError (BakerId, Bakers)
createBaker BakerInfo{..} bkrs =
  case bkrs ^. bakersByKey . at' _bakerSignatureVerifyKey of
    Nothing -> -- key does not yet exist
      if Set.member _bakerAggregationVerifyKey (bkrs ^. aggregationKeys) then
        Left DuplicateAggregationKey
      else -- aggregation keys is not already in use, so we insert baker
        let (bid, t) = L.append (Just $ FullBakerInfo {_bakerInfo = BakerInfo {..}, ..}) (bkrs ^. bakerMap)
        in Right (bid,
                  bkrs
                     & bakerMap .~ t
                     & bakersByKey . at' _bakerSignatureVerifyKey ?~ bid
                     & aggregationKeys %~ Set.insert _bakerAggregationVerifyKey)
        where
          _bakerStake = 0
    Just _ -> Left DuplicateSignKey

data BakerUpdate = BakerUpdate {
  -- |Identity of the baker to update.
  _buId :: !BakerId,
  -- |Optionally update the baker's reward account.
  _buAccount :: !(Maybe AccountAddress),
  -- |Optionally update the baker's public verification key.
  _buSignKey :: !(Maybe BakerSignVerifyKey),
  -- |Optionally update the baker's aggregation verification key
  _buAggregationKey :: !(Maybe BakerAggregationVerifyKey),
  -- |Optionally update the baker's election verification key
  _buElectionKey :: !(Maybe BakerElectionVerifyKey)
}

makeLenses ''BakerUpdate

emptyBakerUpdate :: BakerId -> BakerUpdate
emptyBakerUpdate bid = BakerUpdate bid Nothing Nothing Nothing Nothing

-- |Update a given baker.
-- If this would lead to duplicate baker signing keys return 'Nothing'.
-- If this would result in duplicate aggregation keys return 'Nothing'.
-- If the baker with the given id does not exist this function returns the
-- original 'Bakers' object.
updateBaker :: BakerUpdate -> Bakers -> Maybe Bakers
updateBaker BakerUpdate{..} !bakers =
   case L.lookupMaybe _buId (bakers ^. bakerMap) of
    Nothing -> Just bakers
    Just bakerinfo -> do
        (bakers1, binfo1) <- handleUpdateAggregationKey bakerinfo _buAggregationKey bakers
        (bakers2, binfo2) <- handleUpdateSignKey binfo1 _buSignKey bakers1
        (bakers3, binfo3) <- handleUpdateElectionKey binfo2 _buElectionKey bakers2
        handleUpdateBakerAccount binfo3 _buAccount bakers3
          where
            handleUpdateAggregationKey binfo aggKeyUpdate bs = case aggKeyUpdate of
              Nothing -> Just (bs, Just binfo)
              Just newAggKey ->
                if Set.member newAggKey (bakers ^. aggregationKeys) then
                  Nothing
                else
                  let oldAggKey = binfo ^. bakerInfo . bakerAggregationVerifyKey
                      newBakerInfo = Just (binfo & bakerInfo . bakerAggregationVerifyKey .~ newAggKey)
                  in Just
                      (bs
                        & aggregationKeys %~ Set.delete oldAggKey -- delete old aggregation key from pool of aggregation keys in use
                        & aggregationKeys %~ Set.insert newAggKey -- add new aggregation key to pool
                        & bakerMap %~ (snd . fromJust . L.update (const ((), newBakerInfo)) _buId), -- update the bakermap with the updated bakerinfo
                      newBakerInfo)
            handleUpdateSignKey Nothing _ bs = Just (bs, Nothing)
            handleUpdateSignKey (Just binfo) signKeyUpdate bs = case signKeyUpdate of
              Nothing -> -- don't update the sign key, no clash possible
                Just (bs, Just binfo)
              Just newSignKey ->
                 -- new signing key, make sure it is new
                 case bs ^. bakersByKey . at' newSignKey of
                   Just _ -> -- existing signing key
                     Nothing
                   Nothing -> -- fresh signing key
                     let newBakerInfo = Just (binfo & bakerInfo . bakerSignatureVerifyKey .~ newSignKey)
                     in Just (bs
                             & bakerMap %~ (snd . fromJust . L.update (const ((), newBakerInfo)) _buId)
                             & bakersByKey . at' (binfo ^. bakerInfo . bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                             & bakersByKey . at' newSignKey .~ Just _buId, -- and add new identification
                          newBakerInfo)
            handleUpdateBakerAccount Nothing _ bs = Just bs
            handleUpdateBakerAccount (Just binfo) accountUpdate bs = case accountUpdate of
                Nothing -> Just bs
                Just newBakerAccount ->
                  Just $ bs & bakerMap %~ (snd . fromJust . L.update (const ((), (Just (binfo & bakerInfo . bakerAccount .~ newBakerAccount)))) _buId)
            handleUpdateElectionKey Nothing _ bs = Just (bs, Nothing)
            handleUpdateElectionKey (Just binfo) ekUpdate bs = case ekUpdate of
                Nothing -> Just (bs, Just binfo)
                Just newElectionKey ->
                  let newBakerInfo = Just $ binfo & bakerInfo . bakerElectionVerifyKey .~ newElectionKey
                   in Just (bs & bakerMap %~ (snd . fromJust . L.update (const ((), newBakerInfo)) _buId), newBakerInfo)

removeBaker :: BakerId -> Bakers -> (Bool, Bakers)
removeBaker bid !bakers =
    case L.lookupMaybe bid (bakers ^. bakerMap) of
        Nothing -> (False, bakers)
        Just bkr ->
          case L.delete bid (bakers ^. bakerMap) of
                Nothing -> (False, bakers)
                Just nbm ->
                  (True, bakers
                            & bakerMap .~ nbm
                            & bakersByKey . at' (bkr ^. bakerInfo . bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as wel.
                            & (bakerTotalStake %~ subtract (bkr ^. bakerStake))
                            & aggregationKeys %~ Set.delete (bkr ^. bakerInfo . bakerAggregationVerifyKey))

modifyStake :: Maybe BakerId -> AmountDelta -> Bakers -> Bakers
modifyStake (Just bid) delta bakers = case L.lookupMaybe bid (bakers ^. bakerMap) of
        Nothing -> bakers
        Just _ ->
          let bMap = bakers ^. bakerMap
              upd e = ((), (\f -> f {_bakerStake = applyAmountDelta delta (_bakerStake f)}) <$> e)
              mNewBakerMap = snd <$> L.update upd bid bMap
           in case mNewBakerMap of
                Just nbm ->
                  bakers & bakerMap .~ nbm
                    & bakerTotalStake %~ applyAmountDelta delta
                _ -> undefined
modifyStake _ _ bakers = bakers

addStake :: Maybe BakerId -> Amount -> Bakers -> Bakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: Maybe BakerId -> Amount -> Bakers -> Bakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)
