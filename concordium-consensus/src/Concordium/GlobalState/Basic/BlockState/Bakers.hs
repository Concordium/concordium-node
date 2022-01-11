{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Basic.BlockState.Bakers where

import Control.Exception
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Vector as Vec
import Data.Serialize
import Lens.Micro.Platform
import Concordium.Types.Accounts

import Concordium.GlobalState.BakerInfo
import Concordium.Types
import Concordium.Utils.Serialization

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

-- |The set of bakers that are eligible to bake in a particular epoch.
--
-- The hashing scheme separately hashes the baker info and baker stakes.
--
-- Since this in memory implementation is intended more to serve as a specification
-- than to be used in practice, it is not optimised for time and space usage.
data EpochBakers (av :: AccountVersion) = EpochBakers {
    -- |The 'BakerInfo' for each baker, ordered by the 'BakerId'.
    _bakerInfos :: !(Vec.Vector BakerInfo),
    -- |The stake associated with each baker. This vector corresponds
    -- with the '_bakerInfos' vector.
    _bakerStakes :: !(Vec.Vector (BakerStake av)),
    -- |Total stake of all bakers.
    _bakerTotalStake :: !Amount
} deriving (Eq, Show)

-- |Look up a baker by its identifier.
-- This is implemented as a binary search.
epochBaker :: BakerId -> EpochBakers av -> Maybe (BakerInfo, Amount)
epochBaker bid EpochBakers{..} = binSearch 0 (Vec.length _bakerInfos - 1)
    where
      binSearch lowIndex highIndex = case compare lowIndex highIndex of
          LT -> let
                  midIndex = lowIndex + (highIndex - lowIndex) `div` 2
                  bi = _bakerInfos Vec.! midIndex 
                in case compare bid (_bakerIdentity bi) of                
                  LT -> binSearch lowIndex (midIndex - 1)
                  EQ -> Just (bi, bakerStakeAmount $ _bakerStakes Vec.! midIndex)
                  GT -> binSearch (midIndex + 1) highIndex      
          EQ -> let bi = _bakerInfos Vec.! lowIndex in
                if _bakerIdentity bi == bid
                    then Just (bi, bakerStakeAmount $ _bakerStakes Vec.! lowIndex)
                    else Nothing
          GT -> Nothing

instance HashableTo H.Hash (EpochBakers av) where
    getHash EpochBakers{..} =
        H.hashOfHashes
            (hashVec put _bakerInfos)
            (hashVec putBakerStake _bakerStakes)
      where
        hashVec p v = H.hash $ runPut $ mapM_ p v

-- |Serialize 'EpochBakers'.
putEpochBakers :: Putter (EpochBakers av)
putEpochBakers EpochBakers{..} = do
    assert (Vec.length _bakerInfos == Vec.length _bakerStakes) $
        putLength (Vec.length _bakerInfos)
    mapM_ put _bakerInfos
    mapM_ putBakerStake _bakerStakes

-- |Deserialize 'EpochBakers'.
getEpochBakers :: SAccountVersion av -> Get (EpochBakers av)
getEpochBakers sav = do
    bakers <- getLength
    _bakerInfos <- Vec.replicateM bakers get
    _bakerStakes <- Vec.replicateM bakers (getBakerStake sav)
    let _bakerTotalStake = Vec.sum (bakerStakeAmount <$> _bakerStakes)
    return EpochBakers{..}

-- |Convert an 'EpochBakers' to a 'FullBakers'.
epochToFullBakers :: EpochBakers av -> FullBakers
epochToFullBakers EpochBakers{..} = FullBakers{
        fullBakerInfos = Vec.zipWith mkFullBakerInfo _bakerInfos _bakerStakes,
        bakerTotalStake = _bakerTotalStake
    }
    where
        mkFullBakerInfo bi bs = FullBakerInfo bi (bakerStakeAmount bs)

-- |The set of accounts that are currently registered as bakers.
data ActiveBakers = ActiveBakers {
    _activeBakers :: !(Map BakerId (Set DelegatorId)),
    _aggregationKeys :: !(Set BakerAggregationVerifyKey)
} deriving (Eq, Show)

makeLenses ''ActiveBakers