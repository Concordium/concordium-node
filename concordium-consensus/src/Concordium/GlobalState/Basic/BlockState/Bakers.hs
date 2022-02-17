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
import Concordium.Utils.BinarySearch

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

-- |The set of bakers that are eligible to bake in a particular epoch.
--
-- The hashing scheme separately hashes the baker info and baker stakes.
--
-- Since this in memory implementation is intended more to serve as a specification
-- than to be used in practice, it is not optimised for time and space usage.
data EpochBakers = EpochBakers {
    -- |The 'BakerInfo' for each baker, ordered by the 'BakerId'.
    _bakerInfos :: !(Vec.Vector BakerInfo),
    -- |The stake associated with each baker pool. This vector corresponds
    -- with the '_bakerInfos' vector.
    _bakerStakes :: !(Vec.Vector Amount),
    -- |Total stake of all baker pools.
    _bakerTotalStake :: !Amount
} deriving (Eq, Show)

-- |Look up a baker by its identifier.
-- This is implemented as a binary search.
epochBaker :: BakerId -> EpochBakers -> Maybe (BakerInfo, Amount)
epochBaker bid EpochBakers{..} = do
    (idx, binfo) <- binarySearchI _bakerIdentity _bakerInfos bid
    return (binfo, _bakerStakes Vec.! idx)

instance HashableTo H.Hash EpochBakers where
    getHash EpochBakers{..} =
        H.hashOfHashes
            (hashVec put _bakerInfos)
            (hashVec put _bakerStakes)
      where
        hashVec p v = H.hash $ runPut $ mapM_ p v

-- |Serialize 'EpochBakers'.
putEpochBakers :: Putter EpochBakers
putEpochBakers EpochBakers{..} = do
    assert (Vec.length _bakerInfos == Vec.length _bakerStakes) $
        putLength (Vec.length _bakerInfos)
    mapM_ put _bakerInfos
    mapM_ put _bakerStakes

-- |Deserialize 'EpochBakers'.
getEpochBakers :: Get EpochBakers
getEpochBakers = do
    bakers <- getLength
    _bakerInfos <- Vec.replicateM bakers get
    _bakerStakes <- Vec.replicateM bakers get
    let _bakerTotalStake = Vec.sum _bakerStakes
    return EpochBakers{..}

-- |Construct an 'EpochBakers' from a list of pairs of 'BakerInfo' and the baker stake 'Amount'.
-- The list must be in ascending order by 'BakerId', with no duplicates.
makeHashedEpochBakers :: [(BakerInfo, Amount)] -> Hashed EpochBakers
makeHashedEpochBakers bakers = makeHashed EpochBakers{..}
    where
        bkrs = Vec.fromList bakers
        _bakerInfos = fst <$> bkrs
        _bakerStakes = snd <$> bkrs
        _bakerTotalStake = Vec.sum _bakerStakes

-- |Convert an 'EpochBakers' to a 'FullBakers'.
epochToFullBakers :: EpochBakers -> FullBakers
epochToFullBakers EpochBakers{..} = FullBakers{
        fullBakerInfos = Vec.zipWith mkFullBakerInfo _bakerInfos _bakerStakes,
        bakerTotalStake = _bakerTotalStake
    }
    where
        mkFullBakerInfo bi bs = FullBakerInfo bi bs

-- |Covert an 'EpochBakers' to a list of pairs of 'BakerId' and stake 'Amount'.
-- The list is in ascending order of 'BakerId'.
epochToBakerStakes :: EpochBakers -> Vec.Vector (BakerId, Amount)
epochToBakerStakes EpochBakers{..} = Vec.zipWith mkBakerStake _bakerInfos _bakerStakes
    where
        mkBakerStake bi bs = (_bakerIdentity bi, bs)

-- |The set of accounts that are currently registered as bakers.
data ActiveBakers = ActiveBakers {
    _activeBakers :: !(Map BakerId (Set DelegatorId)),
    _aggregationKeys :: !(Set BakerAggregationVerifyKey),
    _lPoolDelegators :: !(Set DelegatorId)
} deriving (Eq, Show)

makeLenses ''ActiveBakers