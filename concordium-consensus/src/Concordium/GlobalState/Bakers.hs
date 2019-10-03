{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies #-}
module Concordium.GlobalState.Bakers where

import GHC.Generics
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.List as List
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Types

data BakerCreationInfo = BakerCreationInfo !BakerElectionVerifyKey !BakerSignVerifyKey !AccountAddress

data BakerInfo = BakerInfo {
    -- |The baker's public VRF key
    _bakerElectionVerifyKey :: !BakerElectionVerifyKey,
    -- |The baker's public signature key
    _bakerSignatureVerifyKey :: !BakerSignVerifyKey,
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
    _nextBakerId :: !BakerId
} deriving (Eq, Generic, Show)

makeLenses ''Bakers

instance Serialize Bakers where
    put Bakers{..} = put _bakerMap >> put _nextBakerId
    get = do
        _bakerMap <- get
        _nextBakerId <- get
        let
            (_bakersByKey, _bakerTotalStake) = Map.foldrWithKey deriv (Map.empty, 0) _bakerMap
            deriv bid BakerInfo{..} (m, t) = (m & at (_bakerSignatureVerifyKey, _bakerElectionVerifyKey) . non [] %~ (bid:),
                                                t + _bakerStake)
        return Bakers{..}    

emptyBakers :: Bakers
emptyBakers = Bakers Map.empty Map.empty 0 0

-- |Make bakers from a list and assign them sequential identities.
-- NB: Only the first baker with the given signing key is used.
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
      (_bakersByKey, bakerList, duplicateBakers, _nextBakerId, _bakerTotalStake) =
        List.foldl' (\(known, bkrList, duplicate, nextId, totalStake) baker ->
                        let key = baker ^. bakerSignatureVerifyKey in
                          case Map.lookup key known of
                            Nothing -> -- new baker key
                              (Map.insert key nextId known, (nextId, baker):bkrList, duplicate, nextId+1, totalStake + baker ^. bakerStake)
                            Just _ -> (known, bkrList, baker:duplicate, nextId, totalStake)
                    )
                    (Map.empty, [], [], 0, 0)
                    bkrs
                                      

-- |Add a baker to the set of known bakers. If a baker with the given signing
-- key already exists then return 'Nothing', otherwise assign it a fresh id and add it to the set of known bakers.
createBaker :: BakerCreationInfo -> Bakers -> Maybe (BakerId, Bakers)
createBaker (BakerCreationInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAccount) bkrs =
  case bkrs ^. bakersByKey . at _bakerSignatureVerifyKey of
    Nothing -> -- key does not yet exist, insert it
        Just (bid, bkrs 
                   & bakerMap . at bid ?~ BakerInfo{..}
                   & bakersByKey . at _bakerSignatureVerifyKey ?~ bid
                   & nextBakerId .~ bid + 1)
      where
        _bakerStake = 0
        bid = _nextBakerId bkrs
    Just _ -> Nothing

data BakerUpdate = BakerUpdate {
  -- |Identity of the baker to update.
  _buId :: !BakerId,
  -- |Optionally update the baker's reward account.
  _buAccount :: !(Maybe AccountAddress),
  -- |Optionally update the baker's public verification key.
  _buSignKey :: !(Maybe BakerSignVerifyKey)
}

makeLenses ''BakerUpdate

emptyBakerUpdate :: BakerId -> BakerUpdate
emptyBakerUpdate bid = BakerUpdate bid Nothing Nothing

-- |Update a given baker. If this would lead to duplicate baker signing keys
-- return 'Nothing'. If the baker with the given id does not exist this function
-- returns the original 'Bakers' object.
updateBaker :: BakerUpdate -> Bakers -> Maybe Bakers
updateBaker !BakerUpdate{..} !bakers =
  case bakers ^? bakerMap . ix _buId of
    Nothing -> Just bakers
    Just binfo ->
      let bacc = _buAccount ^. non (binfo ^. bakerAccount)
      in case _buSignKey of
           Nothing -> -- don't update the sign key, no clash possible
             Just (bakers & bakerMap %~ Map.insert _buId (binfo & bakerAccount .~ bacc))
           Just newSignKey ->
             -- new signing key, make sure it is new
             case bakers ^. bakersByKey . at newSignKey of
               Just _ -> -- existing signing key
                 Nothing
               Nothing -> -- fresh signing key
                 Just (bakers
                       & bakerMap %~ Map.insert _buId (binfo & bakerSignatureVerifyKey .~ newSignKey)
                       & bakersByKey . at (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                       & bakersByKey . at newSignKey .~ Just _buId) -- and add new identification

removeBaker :: BakerId -> Bakers -> (Bool, Bakers)
removeBaker bid !bakers =
    case bakers ^. bakerMap . at bid of
        Nothing -> (False, bakers)
        Just bkr -> (True, bakers
                            & (bakerMap . at bid .~ Nothing)
                            & bakersByKey . at (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as wel.
                            & (bakerTotalStake %~ subtract (bkr ^. bakerStake)))

modifyStake :: Maybe BakerId -> AmountDelta -> Bakers -> Bakers
modifyStake (Just bid) delta bakers = case bakers ^. bakerMap . at bid of
        Nothing -> bakers
        Just _ -> bakers & bakerMap . ix bid . bakerStake %~ applyAmountDelta delta
                    & bakerTotalStake %~ applyAmountDelta delta
modifyStake _ _ bakers = bakers

addStake :: Maybe BakerId -> Amount -> Bakers -> Bakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: Maybe BakerId -> Amount -> Bakers -> Bakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)
