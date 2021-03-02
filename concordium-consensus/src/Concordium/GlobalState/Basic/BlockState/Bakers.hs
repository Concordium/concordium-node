{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Basic.BlockState.Bakers where

import Control.Exception
import Data.Set(Set)
import qualified Data.Vector as Vec
import Data.Serialize
import Lens.Micro.Platform

import Concordium.GlobalState.BakerInfo
import Concordium.Types
import Concordium.Utils.Serialization

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

-- |The set of bakers that are eligible to bake in a particular epoch.
--
-- The hashing scheme separately hashes the baker info and baker stakes.
--
-- Since this in memory implementation is indended more to serve as a specification
-- than to be used in practice, it is not optimised for time and space usage.
data EpochBakers = EpochBakers {
    -- |The 'BakerInfo' for each baker, ordered by the 'BakerId'.
    _bakerInfos :: !(Vec.Vector BakerInfo),
    -- |The stake associated with each baker. This vector corresponds
    -- with the '_bakerInfos' vector.
    _bakerStakes :: !(Vec.Vector Amount),
    -- |Total stake of all bakers.
    _bakerTotalStake :: !Amount
} deriving (Eq, Show)

-- |Look up a baker by its identifier.
-- This is implemented as a binary search.
epochBaker :: BakerId -> EpochBakers -> Maybe (BakerInfo, Amount)
epochBaker bid EpochBakers{..} = binSearch 0 (Vec.length _bakerInfos - 1)
    where
      binSearch lowIndex highIndex = case compare lowIndex highIndex of
          LT -> let
                  midIndex = lowIndex + (highIndex - lowIndex) `div` 2
                  bi = _bakerInfos Vec.! midIndex 
                in case compare bid (_bakerIdentity bi) of                
                  LT -> binSearch lowIndex (midIndex - 1)
                  EQ -> Just (bi, _bakerStakes Vec.! midIndex)
                  GT -> binSearch (midIndex + 1) highIndex      
          EQ -> let bi = _bakerInfos Vec.! lowIndex in
                if _bakerIdentity bi == bid then Just (bi, _bakerStakes Vec.! lowIndex) else Nothing
          GT -> Nothing

instance HashableTo H.Hash EpochBakers where
    getHash EpochBakers{..} = H.hashOfHashes (hashVec _bakerInfos) (hashVec _bakerStakes)
      where
        hashVec v = H.hash $ runPut $ mapM_ put v

-- |Serialize 'EpochBakers' in V0 format.
putEpochBakers :: Putter EpochBakers
putEpochBakers EpochBakers{..} = do
    assert (Vec.length _bakerInfos == Vec.length _bakerStakes) $
        putLength (Vec.length _bakerInfos)
    mapM_ put _bakerInfos
    mapM_ put _bakerStakes

-- |Deserialize 'EpochBakers' in V0 format.
getEpochBakers :: Get EpochBakers
getEpochBakers = do
    bakers <- getLength
    _bakerInfos <- Vec.replicateM bakers get
    _bakerStakes <- Vec.replicateM bakers get
    let _bakerTotalStake = Vec.sum _bakerStakes
    return EpochBakers{..}

-- |Convert an 'EpochBakers' to a 'FullBakers'.
epochToFullBakers :: EpochBakers -> FullBakers
epochToFullBakers EpochBakers{..} = FullBakers{
        fullBakerInfos = Vec.zipWith FullBakerInfo _bakerInfos _bakerStakes,
        bakerTotalStake = _bakerTotalStake
    }

-- |The set of accounts that are currently registered as bakers.
data ActiveBakers = ActiveBakers {
    _activeBakers :: !(Set BakerId),
    _aggregationKeys :: !(Set BakerAggregationVerifyKey)
} deriving (Eq, Show)

makeLenses ''ActiveBakers