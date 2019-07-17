{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Concordium.GlobalState.Bakers where

import GHC.Generics
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
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
    _bakersByKey :: !(Map (BakerSignVerifyKey, BakerElectionVerifyKey) [BakerId]),
    -- |The total stake delegated to all bakers
    _bakerTotalStake :: !Amount,
    -- |The next 'BakerId' to use for a new baker.
    -- 'BakerId's should not be reused when bakers are removed.
    _nextBakerId :: !BakerId
} deriving (Eq, Generic, Show)
instance Serialize Bakers

makeLenses ''Bakers

emptyBakers :: Bakers
emptyBakers = Bakers Map.empty Map.empty 0 0

bakersFromList :: [BakerInfo] -> Bakers
bakersFromList bkrs = Bakers {
        _bakerMap = Map.fromList $ ibkrs,
        _bakersByKey = foldr (\(k,v) -> Map.insertWith (++) k v) Map.empty [((_bakerSignatureVerifyKey, _bakerElectionVerifyKey), [bid]) | (bid, BakerInfo{..}) <- ibkrs],
        _bakerTotalStake = sum $ _bakerStake <$> bkrs,
        _nextBakerId = fromIntegral $ length bkrs
    }
    where
        ibkrs = zip [0..] bkrs

createBaker :: BakerCreationInfo -> Bakers -> (BakerId, Bakers)
createBaker (BakerCreationInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAccount) bkrs =
        (bid, bkrs 
                & bakerMap . at bid ?~ BakerInfo{..}
                & bakersByKey . at (_bakerSignatureVerifyKey, _bakerElectionVerifyKey) . non [] %~ (bid :)
                & nextBakerId .~ bid + 1)
    where
        _bakerStake = 0
        bid = _nextBakerId bkrs

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

updateBaker :: BakerUpdate -> Bakers -> Bakers
updateBaker !BakerUpdate{..} !bakers = 
    bakers & bakerMap . ix _buId %~ 
        (maybe id (bakerAccount .~) _buAccount) . (maybe id (bakerSignatureVerifyKey .~) _buSignKey)

removeBaker :: BakerId -> Bakers -> (Bool, Bakers)
removeBaker bid !bakers =
    case bakers ^. bakerMap . at bid of
        Nothing -> (False, bakers)
        Just bkr -> (True, bakers
                            & (bakerMap . at bid .~ Nothing)
                            & bakersByKey . at (bkr ^. bakerSignatureVerifyKey, bkr ^. bakerElectionVerifyKey) . non [] %~ filter (/= bid)
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
