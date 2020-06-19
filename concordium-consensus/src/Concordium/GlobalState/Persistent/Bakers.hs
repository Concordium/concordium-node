{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.Persistent.Bakers where

import Data.Serialize
import qualified Data.Set as Set
import GHC.Generics
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Utils
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import Concordium.GlobalState.BakerInfo

-- |Representation of the set of bakers on the chain.
data PersistentBakers = PersistentBakers {
    -- |Baker information, indexed by 'BakerId'
    _bakerInfoMap :: !(Map.Map BakerId BakerInfo),
    -- |Baker stake, indexed by 'BakerId'
    _bakerStakeMap :: !(Map.Map BakerId Amount),
    -- |The baker ids indexed by the keys
    _bakersByKey :: !(Map.Map BakerSignVerifyKey BakerId),
    -- |The total stake delegated to all bakers
    _bakerTotalStake :: !Amount,
    -- |The next 'BakerId' to use for a new baker.
    -- 'BakerId's should not be reused when bakers are removed.
    _nextBakerId :: !BakerId,
    -- |Aggregation keys in use
    _aggregationKeys :: !(Set.Set BakerAggregationVerifyKey)
} deriving (Generic, Show)
instance Serialize PersistentBakers

makeLenses ''PersistentBakers

-- |Convert a (non-persistent) 'Transient.Accounts' to a (persistent) 'Accounts'.
-- The new object is not yet stored on disk.
makePersistentBakers :: Basic.Bakers -> PersistentBakers
makePersistentBakers Basic.Bakers{..} = PersistentBakers{..}
  where _bakerInfoMap = _bakerInfo <$> _bakerMap
        _bakerStakeMap = _bakerStake <$> _bakerMap

-- TODO (MRA) The following operations contain a lot of duplication with the same operations in Blockstate/Bakers.
-- Consider abstracting the Bakers type over the underline pure/persistent blockstate monad

createBaker :: BakerInfo -> PersistentBakers -> Either BakerError (BakerId, PersistentBakers)
createBaker (BakerInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAggregationVerifyKey _bakerAccount) bkrs =
    case bkrs ^. bakersByKey . at' _bakerSignatureVerifyKey of
        Nothing -> -- key does not yet exist
            if Set.member _bakerAggregationVerifyKey (bkrs ^. aggregationKeys) then
              Left DuplicateAggregationKey
            else -- aggregation keys is not already in use, so we insert baker
              Right (bid, bkrs
                         & bakerInfoMap . at' bid ?~ BakerInfo{..}
                         & bakerStakeMap . at' bid ?~ _bakerStake
                         & bakersByKey . at' _bakerSignatureVerifyKey ?~ bid
                         & nextBakerId .~ bid + 1
                         & aggregationKeys %~ Set.insert _bakerAggregationVerifyKey)
                where
                  _bakerStake = 0
                  bid = _nextBakerId bkrs
        Just _ -> Left DuplicateSignKey


-- TODO (MRA) Especially this function is practically identical to its pure version
-- |Update a given baker. If this would lead to duplicate baker signing keys
-- return 'Nothing'. If the baker with the given id does not exist this function
-- returns the original 'Bakers' object.

-- TODO (MRA) either move createBaker back here or add all these operations to the BakerOperations monad
updateBaker :: Basic.BakerUpdate -> PersistentBakers -> Maybe PersistentBakers
updateBaker !Basic.BakerUpdate{..} !bakers =
  case bakers ^? bakerInfoMap . ix _buId of
    Nothing -> Just bakers
    Just binfo ->
      let bacc = _buAccount ^. non (binfo ^. bakerAccount)
      in case _buSignKey of
           Nothing -> -- don't update the sign key, no clash possible
             Just (bakers & bakerInfoMap %~ Map.insert _buId (binfo & bakerAccount .~ bacc))
           Just newSignKey ->
             -- new signing key, make sure it is new
             case bakers ^. bakersByKey . at' newSignKey of
               Just _ -> -- existing signing key
                 Nothing
               Nothing -> -- fresh signing key
                 Just (bakers
                       & bakerInfoMap %~ Map.insert _buId (binfo & bakerSignatureVerifyKey .~ newSignKey)
                       & bakersByKey . at' (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                       & bakersByKey . at' newSignKey .~ Just _buId) -- and add new identification

removeBaker :: BakerId -> PersistentBakers -> (Bool, PersistentBakers)
removeBaker bid !bakers =
    case (bakers ^. bakerInfoMap . at' bid, bakers ^. bakerStakeMap . at' bid) of
        (Nothing, Nothing) -> (False, bakers)
        (Just bkr, Just bakerStake) -> (True, bakers
                            & (bakerInfoMap . at' bid .~ Nothing)
                            & bakersByKey . at' (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as well.
                            & (bakerStakeMap . at' bid .~ Nothing)
                            & (bakerTotalStake %~ subtract bakerStake))
        _ -> error $ "bakerInfoMap and bakerStakeMap must have the same domain, but they differ for key " ++ show bid

modifyStake :: Maybe BakerId -> AmountDelta -> PersistentBakers -> PersistentBakers
modifyStake (Just bid) delta bakers = case bakers ^. bakerStakeMap . at' bid of
        Nothing -> bakers
        Just _ -> bakers & bakerStakeMap . ix bid %~ applyAmountDelta delta
                    & bakerTotalStake %~ applyAmountDelta delta
modifyStake _ _ bakers = bakers

addStake :: Maybe BakerId -> Amount -> PersistentBakers -> PersistentBakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: Maybe BakerId -> Amount -> PersistentBakers -> PersistentBakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)