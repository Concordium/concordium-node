{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.PoolRewards (
    BakerPoolRewardDetails (..),
    CapitalDistributionRef,
    PoolRewards (..),
    emptyPoolRewards,
    putPoolRewards,
    bakerBlockCounts,
    rotateCapitalDistribution,
    setNextCapitalDistribution,
    currentPassiveDelegationCapital,
    lookupBakerCapitalAndRewardDetails,
    migratePoolRewardsP1,
    migratePoolRewards,
    migratePoolRewardsP6,
) where

import Control.Exception (assert)
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Vector as Vec
import Data.Word

import Concordium.Crypto.SHA256 as Hash

import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization.Put (MonadPut (..))

import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT
import Concordium.GlobalState.PoolRewards
import Concordium.GlobalState.Rewards

type CapitalDistributionRef (bhv :: BlockHashVersion) =
    HashedBufferedRef' (CapitalDistributionHash' bhv) CapitalDistribution

-- | Details of rewards accruing over the course of a reward period, and details about the capital
--  distribution for this reward period and (possibly) the next. Note, 'currentCapital' and
--  'nextCapital' are the same except in the epoch before a payday, where 'nextCapital' is updated
--  to record the capital distribution for the next reward period.
data PoolRewards (bhv :: BlockHashVersion) (av :: AccountVersion) = PoolRewards
    { -- | The capital distribution for the next reward period.
      --  This is updated the epoch before a payday.
      nextCapital :: !(CapitalDistributionRef bhv),
      -- | The capital distribution for the current reward period.
      currentCapital :: !(CapitalDistributionRef bhv),
      -- | The details of rewards accruing to baker pools.
      --  These are indexed by the index of the baker in the capital distribution (_not_ the BakerId).
      bakerPoolRewardDetails :: !(LFMBT.LFMBTree Word64 HashedBufferedRef (BakerPoolRewardDetails av)),
      -- | The transaction reward amount accruing to the passive delegators.
      passiveDelegationTransactionRewards :: !Amount,
      -- | The transaction reward fraction accruing to the foundation.
      foundationTransactionRewards :: !Amount,
      -- | The next payday occurs at the start of this epoch.
      nextPaydayEpoch :: !Epoch,
      -- | The rate at which tokens are minted for the current reward period.
      nextPaydayMintRate :: !MintRate
    }
    deriving (Show)

-- | Migrate pool rewards from @m@ to the new backing store @t m@.
--  This takes the new next payday epoch as a parameter, since this should always be updated on
--  a protocol update. The hashes for the capital distributions are recomputed, as they schema
--  may change between versions.
migratePoolRewards ::
    (SupportMigration m t, IsBlockHashVersion bhv1, IsAccountVersion av0, IsAccountVersion av1) =>
    Epoch ->
    PoolRewards bhv0 av0 ->
    t m (PoolRewards bhv1 av1)
migratePoolRewards newNextPayday PoolRewards{..} = do
    nextCapital' <- migrateHashedBufferedRef return nextCapital
    currentCapital' <- migrateHashedBufferedRef return currentCapital
    bakerPoolRewardDetails' <- LFMBT.migrateLFMBTree (migrateReference (return . migrateBakerPoolRewardDetails)) bakerPoolRewardDetails
    -- the remaining fields are flat, so migration is copying
    return
        PoolRewards
            { nextCapital = nextCapital',
              currentCapital = currentCapital',
              bakerPoolRewardDetails = bakerPoolRewardDetails',
              nextPaydayEpoch = newNextPayday,
              ..
            }

-- | Migrate pool rewards from @m@ to the new backing store @t m@, for use with consensus version 1.
--  This takes the pre-migration epoch number and reward period length as parameters, and sets the
--  next payday epoch to be the the number of epochs that were remaining until the next payday
--  at the time of the migration, or the length of the reward period if that is smaller.
migratePoolRewardsP6 ::
    (SupportMigration m t, IsBlockHashVersion bhv1, IsAccountVersion av0, IsAccountVersion av1) =>
    -- | The epoch number before the migration.
    Epoch ->
    -- | The length of the reward period.
    RewardPeriodLength ->
    PoolRewards bhv0 av0 ->
    t m (PoolRewards bhv1 av1)
migratePoolRewardsP6 oldEpoch rpLength pr = migratePoolRewards newNextPayday pr
  where
    oldPaydayEpoch = nextPaydayEpoch pr
    newNextPayday = max 1 (min (rewardPeriodEpochs rpLength) (oldPaydayEpoch - oldEpoch))

-- | Migrate pool rewards from the format before delegation to the P4 format.
migratePoolRewardsP1 ::
    forall m bhv av.
    (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) =>
    -- | Current epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- | Next epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- | Mapping of baker ids to the number of blocks baked in the epoch.
    Map.Map BakerId Word64 ->
    -- | Epoch of next payday
    Epoch ->
    -- | Mint rate for the next payday
    MintRate ->
    m (PoolRewards bhv av)
migratePoolRewardsP1 curBakers nextBakers blockCounts npEpoch npMintRate = do
    (nextCapital, _) <- refFlush =<< bufferHashed (makeCD nextBakers)
    (currentCapital, _) <- refFlush =<< bufferHashed (makeCD curBakers)
    bakerPoolRewardDetails' <- LFMBT.fromAscListV =<< mapM makePRD curBakers
    (_, bakerPoolRewardDetails) <- storeUpdateRef bakerPoolRewardDetails'
    let passiveDelegationTransactionRewards = 0
        foundationTransactionRewards = 0
        nextPaydayEpoch = npEpoch
        nextPaydayMintRate = npMintRate
    return PoolRewards{..}
  where
    makeCD bkrs =
        makeHashed $
            CapitalDistribution
                { bakerPoolCapital = Vec.fromList (makeBakerCapital <$> bkrs),
                  passiveDelegatorsCapital = Vec.empty
                }
    makeBakerCapital (bid, amt) = BakerCapital bid amt Vec.empty
    makePRD :: (BakerId, a) -> m (HashedBufferedRef (BakerPoolRewardDetails av))
    makePRD (bid, _) = do
        let bprd =
                BakerPoolRewardDetails
                    { blockCount = Map.findWithDefault 0 bid blockCounts,
                      transactionFeesAccrued = 0,
                      finalizationAwake = False,
                      suspensionInfo = conditionally hasValidatorSuspension emptySuspensionInfo
                    }
        (!newRef, _) <- refFlush =<< refMake bprd
        return newRef
    hasValidatorSuspension = sSupportsValidatorSuspension (accountVersion @av)

-- | Look up the baker capital and reward details for a baker ID.
lookupBakerCapitalAndRewardDetails ::
    (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) =>
    BakerId ->
    PoolRewards bhv av ->
    m (Maybe (BakerCapital, BakerPoolRewardDetails av))
lookupBakerCapitalAndRewardDetails bid PoolRewards{..} = do
    cdistr <- refLoad currentCapital
    case binarySearchI bcBakerId (bakerPoolCapital cdistr) bid of
        Nothing -> return Nothing
        Just (index, capital) ->
            fmap (capital,) <$> LFMBT.lookup (fromIntegral index) bakerPoolRewardDetails

instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (PoolRewards bhv av) where
    storeUpdate pr0 = do
        (pNextCapital, nextCapital) <- storeUpdate (nextCapital pr0)
        (pCurrentCapital, currentCapital) <- storeUpdate (currentCapital pr0)
        (pBakerPoolRewardDetails, bakerPoolRewardDetails) <-
            storeUpdate (bakerPoolRewardDetails pr0)
        (pPassiveDelegationTransactionRewards, passiveDelegationTransactionRewards) <-
            storeUpdate (passiveDelegationTransactionRewards pr0)
        (pFoundationTransactionRewards, foundationTransactionRewards) <-
            storeUpdate (foundationTransactionRewards pr0)
        (pNextPaydayEpoch, nextPaydayEpoch) <- storeUpdate (nextPaydayEpoch pr0)
        (pNextPaydayMintRate, nextPaydayMintRate) <- storeUpdate (nextPaydayMintRate pr0)
        let p = do
                pNextCapital
                pCurrentCapital
                pBakerPoolRewardDetails
                pPassiveDelegationTransactionRewards
                pFoundationTransactionRewards
                pNextPaydayEpoch
                pNextPaydayMintRate
        return (p, PoolRewards{..})
    load = do
        mNextCapital <- label "Next Capital" load
        mCurrentCapital <- label "Current Capital" load
        mBakerPoolRewardDetails <- label "Baker Pool Reward Details" load
        mPassiveDelegationTransactionRewards <- label "Passive Delegation Transaction Rewards" load
        mFoundationTransactionRewards <- label "Foundation Transaction Rewards" load
        mNextPaydayEpoch <- label "Next Payday Epoch" load
        mNextPaydayMintRate <- label "Next Payday Mint Rate" load
        return $! do
            nextCapital <- mNextCapital
            currentCapital <- mCurrentCapital
            bakerPoolRewardDetails <- mBakerPoolRewardDetails
            passiveDelegationTransactionRewards <- mPassiveDelegationTransactionRewards
            foundationTransactionRewards <- mFoundationTransactionRewards
            nextPaydayEpoch <- mNextPaydayEpoch
            nextPaydayMintRate <- mNextPaydayMintRate
            return $! PoolRewards{..}

-- | Serialize 'PoolRewards'.
--  The 'bakerPoolRewardDetails' is serialized as a flat list, with the length implied by the
--  length of 'bakerPoolCapital' of 'currentCapital'.
putPoolRewards :: (MonadBlobStore m, MonadPut m, IsBlockHashVersion bhv, IsAccountVersion av) => PoolRewards bhv av -> m ()
putPoolRewards PoolRewards{..} = do
    nxtCapital <- refLoad nextCapital
    curCapital <- refLoad currentCapital
    bprdList <- LFMBT.toAscList bakerPoolRewardDetails
    assert (Vec.length (bakerPoolCapital curCapital) == length bprdList) $
        liftPut $ do
            put nxtCapital
            put curCapital
            mapM_ put bprdList
            put passiveDelegationTransactionRewards
            put foundationTransactionRewards
            put nextPaydayEpoch
            put nextPaydayMintRate

instance (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) => MHashableTo m (PoolRewardsHash bhv) (PoolRewards bhv av) where
    getHashM PoolRewards{..} = do
        hNextCapital <- getHashM nextCapital
        hCurrentCapital <- getHashM currentCapital
        hBakerPoolRewardDetails <- getHashM bakerPoolRewardDetails
        return $!
            PoolRewardsHash . Hash.hashOfHashes (theCapitalDistributionHash @bhv hNextCapital) $
                Hash.hashOfHashes (theCapitalDistributionHash @bhv hCurrentCapital) $
                    Hash.hashOfHashes (LFMBT.theLFMBTreeHash @bhv hBakerPoolRewardDetails) $
                        getHash $
                            runPut $
                                put passiveDelegationTransactionRewards
                                    <> put foundationTransactionRewards
                                    <> put nextPaydayEpoch
                                    <> put nextPaydayMintRate

instance (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) => Cacheable m (PoolRewards bhv av) where
    cache pr@PoolRewards{nextPaydayEpoch = nextPaydayEpoch, nextPaydayMintRate = nextPaydayMintRate} = do
        nextCapital <- cache (nextCapital pr)
        currentCapital <- cache (currentCapital pr)
        bakerPoolRewardDetails <- cache (bakerPoolRewardDetails pr)
        passiveDelegationTransactionRewards <- cache (passiveDelegationTransactionRewards pr)
        foundationTransactionRewards <- cache (foundationTransactionRewards pr)
        return PoolRewards{..}

-- | The empty 'PoolRewards'.
emptyPoolRewards :: (MonadBlobStore m, IsBlockHashVersion bhv) => m (PoolRewards bhv av)
emptyPoolRewards = do
    emptyCDRef <- refMake emptyCapitalDistribution
    return
        PoolRewards
            { nextCapital = emptyCDRef,
              currentCapital = emptyCDRef,
              bakerPoolRewardDetails = LFMBT.empty,
              passiveDelegationTransactionRewards = 0,
              foundationTransactionRewards = 0,
              nextPaydayEpoch = 0,
              nextPaydayMintRate = MintRate 0 0
            }

-- | List of baker and number of blocks baked by this baker in the reward period.
bakerBlockCounts :: (MonadBlobStore m, IsBlockHashVersion bhv, IsAccountVersion av) => PoolRewards bhv av -> m [(BakerId, Word64)]
bakerBlockCounts PoolRewards{..} = do
    cc <- refLoad currentCapital
    rds <- LFMBT.toAscPairList bakerPoolRewardDetails
    return $! zipToBlockCounts (bakerPoolCapital cc) rds
  where
    zipToBlockCounts _ [] = []
    zipToBlockCounts bpc ((_, BakerPoolRewardDetails{..}) : rds) =
        if Vec.null bpc
            then []
            else (bcBakerId (Vec.head bpc), blockCount) : zipToBlockCounts (Vec.tail bpc) rds

-- | Rotate the capital distribution, so that the current capital distribution
--  is replaced by the next one, and set up empty pool rewards, except that
--  missed rounds are carried over from the old pool rewards.
rotateCapitalDistribution ::
    forall av ref m bhv.
    (MonadBlobStore m, Reference m ref (PoolRewards bhv av), IsBlockHashVersion bhv, IsAccountVersion av) =>
    ref (PoolRewards bhv av) ->
    m (ref (PoolRewards bhv av))
rotateCapitalDistribution oldPoolRewards = do
    pr <- refLoad oldPoolRewards
    nextCap <- refLoad (nextCapital pr)
    oldBakerIds <- fmap bcBakerId . Vec.toList . bakerPoolCapital <$> refLoad (currentCapital pr)
    let newBakerIds = bcBakerId <$> Vec.toList (bakerPoolCapital nextCap)
    oldRewardDetails <- LFMBT.toAscList (bakerPoolRewardDetails pr)
    let buildRewardDetails [] _ _ = []
        buildRewardDetails newIds [] [] = emptyBakerPoolRewardDetails <$ newIds
        buildRewardDetails (newId : newIds) (oldId : oldIds) (r : rs) = case compare newId oldId of
            LT -> emptyBakerPoolRewardDetails : buildRewardDetails newIds (oldId : oldIds) (r : rs)
            EQ ->
                (emptyBakerPoolRewardDetails @av){suspensionInfo = suspensionInfo r}
                    : buildRewardDetails newIds oldIds rs
            GT -> buildRewardDetails (newId : newIds) oldIds rs
        buildRewardDetails _ _ _ =
            error "rotateCapitalDistribution: length mismatch between capital distribution and reward details"
    rewardDetails <- LFMBT.fromAscList $ buildRewardDetails newBakerIds oldBakerIds oldRewardDetails
    refMake $
        pr
            { currentCapital = nextCapital pr,
              bakerPoolRewardDetails = rewardDetails
            }

setNextCapitalDistribution ::
    (MonadBlobStore m, Reference m ref (PoolRewards bhv av), IsBlockHashVersion bhv) =>
    [(BakerId, Amount, [(DelegatorId, Amount)])] ->
    [(DelegatorId, Amount)] ->
    ref (PoolRewards bhv av) ->
    m (ref (PoolRewards bhv av))
setNextCapitalDistribution bakers passive oldPoolRewards = do
    let bakerPoolCapital = Vec.fromList $ map mkBakCap bakers
    let passiveDelegatorsCapital = Vec.fromList $ map mkDelCap passive
    capDist <- refMake $ CapitalDistribution{..}
    pr <- refLoad oldPoolRewards
    refMake pr{nextCapital = capDist}
  where
    mkBakCap (bcBakerId, bcBakerEquityCapital, dels) =
        let bcDelegatorCapital = Vec.fromList $ map mkDelCap dels
        in  BakerCapital{..}
    mkDelCap (dcDelegatorId, dcDelegatorCapital) =
        DelegatorCapital{..}

-- | The total capital passively delegated in the current reward period capital distribution.
currentPassiveDelegationCapital ::
    (MonadBlobStore m, IsBlockHashVersion bhv) =>
    PoolRewards bhv av ->
    m Amount
currentPassiveDelegationCapital PoolRewards{..} =
    Vec.sum . fmap dcDelegatorCapital . passiveDelegatorsCapital <$> refLoad currentCapital
