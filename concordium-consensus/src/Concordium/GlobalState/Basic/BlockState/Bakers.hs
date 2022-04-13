{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Basic.BlockState.Bakers where

import Control.Exception
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Serialize
import Lens.Micro.Platform
import Concordium.Types.Accounts

import Concordium.Genesis.Data (StateMigrationParameters(..))
import Concordium.GlobalState.BakerInfo
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Utils.Serialization
import Concordium.Utils.BinarySearch

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo
import qualified Concordium.Genesis.Data.P4 as P4
import Concordium.GlobalState.CapitalDistribution

-- |The set of bakers that are eligible to bake in a particular epoch.
--
-- The hashing scheme separately hashes the baker info and baker stakes.
--
-- Since this in memory implementation is intended more to serve as a specification
-- than to be used in practice, it is not optimised for time and space usage.
data EpochBakers (av :: AccountVersion) = EpochBakers {
    -- |The 'BakerInfo' for each baker, ordered by the 'BakerId'.
    _bakerInfos :: !(Vec.Vector (BakerInfoEx av)),
    -- |The stake associated with each baker pool. This vector corresponds
    -- with the '_bakerInfos' vector.
    _bakerStakes :: !(Vec.Vector Amount),
    -- |Total stake of all baker pools.
    _bakerTotalStake :: !Amount
} deriving (Eq, Show)

-- |Look up a baker by its identifier.
-- This is implemented as a binary search.
epochBaker :: BakerId -> EpochBakers av -> Maybe (BakerInfo, Amount)
epochBaker bid EpochBakers{..} = do
    (idx, binfo) <- binarySearchI _bakerIdentity (view bakerInfo <$> _bakerInfos) bid
    return (binfo, _bakerStakes Vec.! idx)

instance IsAccountVersion av => HashableTo H.Hash (EpochBakers av) where
    getHash EpochBakers{..} =
        H.hashOfHashes
            (hashVec _bakerInfos)
            (hashVec _bakerStakes)
      where
        hashVec v = H.hash $ runPut $ mapM_ put v

-- |Serialize 'EpochBakers'.
putEpochBakers :: IsAccountVersion av => Putter (EpochBakers av)
putEpochBakers EpochBakers{..} = do
    assert (Vec.length _bakerInfos == Vec.length _bakerStakes) $
        putLength (Vec.length _bakerInfos)
    mapM_ put _bakerInfos
    mapM_ put _bakerStakes

-- |Deserialize 'EpochBakers'.
getEpochBakers :: IsAccountVersion av => Get (EpochBakers av)
getEpochBakers = do
    bakers <- getLength
    _bakerInfos <- Vec.replicateM bakers get
    _bakerStakes <- Vec.replicateM bakers get
    let _bakerTotalStake = Vec.sum _bakerStakes
    return EpochBakers{..}

-- |Construct an 'EpochBakers' from a list of pairs of 'BakerInfo' and the baker stake 'Amount'.
-- The list must be in ascending order by 'BakerId', with no duplicates.
makeHashedEpochBakers :: IsAccountVersion av => [(BakerInfoEx av, Amount)] -> Hashed (EpochBakers av)
makeHashedEpochBakers bakers = makeHashed EpochBakers{..}
    where
        bkrs = Vec.fromList bakers
        _bakerInfos = fst <$> bkrs
        _bakerStakes = snd <$> bkrs
        _bakerTotalStake = Vec.sum _bakerStakes

-- |Convert an 'EpochBakers' to a 'FullBakers'.
epochToFullBakers :: EpochBakers av -> FullBakers
epochToFullBakers EpochBakers{..} = FullBakers{
        fullBakerInfos = Vec.zipWith mkFullBakerInfo _bakerInfos _bakerStakes,
        bakerTotalStake = _bakerTotalStake
    }
    where
        mkFullBakerInfo bi bs = FullBakerInfo (bi ^. bakerInfo) bs

-- |Convert an 'EpochBakers' to a 'FullBakersEx'.
epochToFullBakersEx :: (av ~ 'AccountV1) => EpochBakers av -> FullBakersEx
epochToFullBakersEx EpochBakers{..} = FullBakersEx {
        bakerInfoExs = Vec.zipWith mkFullBakerInfoEx _bakerInfos _bakerStakes,
        bakerPoolTotalStake = _bakerTotalStake
    }
    where
        mkFullBakerInfoEx bi bs =
            FullBakerInfoEx
                (FullBakerInfo (bi ^. bakerInfo) bs)
                (bi ^. poolCommissionRates)

-- |Covert an 'EpochBakers' to a vector of pairs of 'BakerId' and stake 'Amount'.
-- The vector is in ascending order of 'BakerId'.
epochToBakerStakes :: EpochBakers av -> Vec.Vector (BakerId, Amount)
epochToBakerStakes EpochBakers{..} = Vec.zipWith mkBakerStake _bakerInfos _bakerStakes
    where
        mkBakerStake bi bs = (bi ^. bakerIdentity, bs)

-- |Migrate 'EpochBakers' from one version to another.
-- For 'StateMigrationParametersTrivial', no conversion is performed.
-- For 'StateMigrationParametersP3ToP4', the bakers are extended using 'P4.defaultBakerPoolInfo'.
migrateEpochBakers ::
    StateMigrationParameters oldpv pv ->
    EpochBakers (AccountVersionFor oldpv) ->
    EpochBakers (AccountVersionFor pv)
migrateEpochBakers StateMigrationParametersTrivial eb = eb
migrateEpochBakers (StateMigrationParametersP3ToP4 migration) EpochBakers{..} =
    EpochBakers
        { _bakerInfos = migrateBakerInfo <$> _bakerInfos,
          ..
        }
    where
        migrateBakerInfo (BakerInfoExV0 bi) = BakerInfoExV1 bi (P4.defaultBakerPoolInfo migration)


-- |The delegators and total stake of an active pool.
data ActivePool = ActivePool {
    -- |The set of delegators to this pool.
    _apDelegators :: !(Set DelegatorId),
    -- |The total capital staked by delegators to this pool.
    _apDelegatorTotalCapital :: !Amount
} deriving (Eq, Show)
makeLenses ''ActivePool

-- |Active pool with no delegators.
emptyActivePool :: ActivePool
emptyActivePool = ActivePool mempty 0

-- |Make an active pool from a vector of the 'DelegatorCapital's.
makeActivePool :: Vec.Vector DelegatorCapital -> ActivePool
makeActivePool = Vec.foldr' accum emptyActivePool
  where
    accum DelegatorCapital{..} =
        (apDelegators %~ Set.insert dcDelegatorId)
            . (apDelegatorTotalCapital +~ dcDelegatorCapital)

-- |The semigroup on 'ActivePool' takes the union of delegators and sum of capitals.
-- If the delegators are not disjoint, the result may not be the desired one.
instance Semigroup ActivePool where
    ap1 <> ap2 =
        ActivePool
            (_apDelegators ap1 <> _apDelegators ap2)
            (_apDelegatorTotalCapital ap1 + _apDelegatorTotalCapital ap2)

-- |Remove a delegator from an 'ActivePool'. It is assumed that the delegator
-- belongs to the pool, and the 'Amount' correctly represents the delegator's contribution.
removeDelegator :: DelegatorId -> Amount -> ActivePool -> ActivePool
removeDelegator delId delAmt =
    (apDelegators %~ Set.delete delId)
        . (apDelegatorTotalCapital -~ delAmt)

-- |Add a delegator to an 'ActivePool'. It is assumed that the delegator does not
-- already belong to the pool.
addDelegator :: DelegatorId -> Amount -> ActivePool -> ActivePool
addDelegator delId delAmt =
    (apDelegators %~ Set.insert delId)
        . (apDelegatorTotalCapital +~ delAmt)

-- |An index of the baker and delegator accounts.  Every account that has a baker or delegator
-- record on it should have a corresponding entry here.
-- (See $Concordium.GlobalState.BlockState.ActiveCurrentNext.)
data ActiveBakers = ActiveBakers {
    -- |A map from each baker to its pool of delegators.
    _activeBakers :: !(Map BakerId ActivePool),
    -- |The set of public aggregation keys used by bakers.
    _aggregationKeys :: !(Set BakerAggregationVerifyKey),
    -- |The pool of L-pool delegators.
    _lPoolDelegators :: !ActivePool,
    -- |The total capital of active bakers and delegators.
    _totalActiveCapital :: !Amount
} deriving (Eq, Show)

makeLenses ''ActiveBakers

-- |The pool for a specific delegation target.
pool :: DelegationTarget -> Traversal' ActiveBakers ActivePool
pool DelegateToLPool = lPoolDelegators
pool (DelegateToBaker bid) = activeBakers . ix bid

-- |An empty 'ActiveBakers' structure.
emptyActiveBakers :: ActiveBakers
emptyActiveBakers = ActiveBakers mempty mempty emptyActivePool 0

-- |Transfer all delegators from a baker to the L-pool in the 'ActiveBakers'. This does
-- not affect the total stake, and does not remove the baker itself. This returns the list of
-- affected delegators.  (This will have no effect if the baker is not actually a baker, although
-- this function should not be used in that case.)
transferDelegatorsToLPool :: BakerId -> ActiveBakers -> ([DelegatorId], ActiveBakers)
transferDelegatorsToLPool bid ab = case ab ^? activeBakers . ix bid of
    Nothing -> ([], ab)
    Just oldPool ->
        ( oldPool ^. apDelegators . to Set.toAscList,
          ab
            & activeBakers . ix bid .~ emptyActivePool
            & lPoolDelegators
                %~ ( (apDelegators %~ Set.union (oldPool ^. apDelegators))
                        . (apDelegatorTotalCapital +~ (oldPool ^. apDelegatorTotalCapital))
                   )
        )