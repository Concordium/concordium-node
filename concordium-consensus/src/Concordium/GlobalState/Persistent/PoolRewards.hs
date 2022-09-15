{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.Persistent.PoolRewards (
    module Concordium.GlobalState.Basic.BlockState.PoolRewards,
    PoolRewards (..),
    emptyPoolRewards,
    makerPersistentPoolRewards,
    putPoolRewards,
    bakerBlockCounts,
    rotateCapitalDistribution,
    setNextCapitalDistribution,
    currentPassiveDelegationCapital,
    lookupBakerCapitalAndRewardDetails,
    migratePoolRewards
) where

import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Vector as Vec
import Data.Word
import Control.Exception (assert)

import Concordium.Crypto.SHA256 as Hash

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as BasicLFMBT
import Concordium.GlobalState.Rewards

import Concordium.GlobalState.Basic.BlockState.PoolRewards (
    BakerPoolRewardDetails (..),
 )
import qualified Concordium.GlobalState.Basic.BlockState.PoolRewards as BasicPoolRewards
import Concordium.GlobalState.CapitalDistribution

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT

import Concordium.Utils.Serialization.Put

-- |Details of rewards accruing over the course of a reward period, and details about the capital
-- distribution for this reward period and (possibly) the next. Note, 'currentCapital' and
-- 'nextCapital' are the same except in the epoch before a payday, where 'nextCapital' is updated
-- to record the capital distribution for the next reward period.
data PoolRewards = PoolRewards
    { -- |The capital distribution for the next reward period.
      -- This is updated the epoch before a payday.
      nextCapital :: !(HashedBufferedRef CapitalDistribution),
      -- |The capital distribution for the current reward period.
      currentCapital :: !(HashedBufferedRef CapitalDistribution),
      -- |The details of rewards accruing to baker pools.
      -- These are indexed by the index of the baker in the capital distribution (_not_ the BakerId).
      bakerPoolRewardDetails :: !(LFMBT.LFMBTree Word64 BufferedRef BakerPoolRewardDetails),
      -- |The transaction reward amount accruing to the passive delegators.
      passiveDelegationTransactionRewards :: !Amount,
      -- |The transaction reward fraction accruing to the foundation.
      foundationTransactionRewards :: !Amount,
      -- |The next payday occurs at the start of this epoch.
      nextPaydayEpoch :: !Epoch,
      -- |The rate at which tokens are minted for the current reward period.
      nextPaydayMintRate :: !MintRate
    }
    deriving (Show)

migratePoolRewards :: forall m . (MonadBlobStore m) => 
    -- |Current epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- |Next epoch bakers and stakes, in ascending order of 'BakerId'.
    [(BakerId, Amount)] ->
    -- |Mapping of baker ids to the number of blocks baked in the epoch.
    Map.Map BakerId Word64 ->
    -- |Epoch of next payday
    Epoch ->
    -- |Mint rate for the next payday
    MintRate ->
    m PoolRewards 
migratePoolRewards curBakers nextBakers blockCounts npEpoch npMintRate = do
  (nextCapital, _) <- refFlush =<< bufferHashed (makeCD nextBakers)
  (currentCapital, _) <- refFlush =<< bufferHashed (makeCD curBakers)
  bakerPoolRewardDetails' <- LFMBT.fromAscListV =<< mapM makePRD curBakers
  (_, bakerPoolRewardDetails) <- storeUpdateRef bakerPoolRewardDetails'
  let passiveDelegationTransactionRewards = 0
      foundationTransactionRewards = 0
      nextPaydayEpoch = npEpoch
      nextPaydayMintRate = npMintRate
  return PoolRewards{..}

  where makeCD bkrs = makeHashed $
            CapitalDistribution
                { bakerPoolCapital = Vec.fromList (makeBakerCapital <$> bkrs),
                  passiveDelegatorsCapital = Vec.empty
                }
        makeBakerCapital (bid, amt) = BakerCapital bid amt Vec.empty
        makePRD :: (BakerId, a) -> m (BufferedRef BakerPoolRewardDetails)
        makePRD (bid, _) = do
          let bprd = BakerPoolRewardDetails
                  { blockCount = Map.findWithDefault 0 bid blockCounts,
                    transactionFeesAccrued = 0,
                    finalizationAwake = False
                  }
          (!newRef, _) <- refFlush =<< refMake bprd
          return newRef

-- |Look up the baker capital and reward details for a baker ID.
lookupBakerCapitalAndRewardDetails ::
    (MonadBlobStore m) =>
    BakerId ->
    PoolRewards ->
    m (Maybe (BakerCapital, BakerPoolRewardDetails))
lookupBakerCapitalAndRewardDetails bid PoolRewards{..} = do
    cdistr <- refLoad currentCapital
    case binarySearchI bcBakerId (bakerPoolCapital cdistr) bid of
        Nothing -> return Nothing
        Just (index, capital) ->
            fmap (capital,) <$> LFMBT.lookup (fromIntegral index) bakerPoolRewardDetails

instance MonadBlobStore m => BlobStorable m PoolRewards where
    storeUpdate pr0 = do
        (pNextCapital, nextCapital) <- storeUpdate (nextCapital pr0)
        (pCurrentCapital, currentCapital) <- storeUpdate (currentCapital pr0)
        (pBakerPoolRewardDetails, bakerPoolRewardDetails)
            <- storeUpdate (bakerPoolRewardDetails pr0)
        (pPassiveDelegationTransactionRewards, passiveDelegationTransactionRewards)
            <- storeUpdate (passiveDelegationTransactionRewards pr0)
        (pFoundationTransactionRewards, foundationTransactionRewards)
            <- storeUpdate (foundationTransactionRewards pr0)
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

-- |Serialize 'PoolRewards'.
-- The 'bakerPoolRewardDetails' is serialized as a flat list, with the length implied by the
-- length of 'bakerPoolCapital' of 'currentCapital'.
putPoolRewards :: (MonadBlobStore m, MonadPut m) => PoolRewards -> m ()
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

instance MonadBlobStore m => MHashableTo m PoolRewardsHash PoolRewards where
    getHashM PoolRewards{..} = do
        hNextCapital <- getHashM nextCapital
        hCurrentCapital <- getHashM currentCapital
        hBakerPoolRewardDetails <- getHashM bakerPoolRewardDetails
        return
            $! PoolRewardsHash . Hash.hashOfHashes hNextCapital
            $ Hash.hashOfHashes hCurrentCapital $
                Hash.hashOfHashes hBakerPoolRewardDetails $
                    getHash $
                        runPut $
                            put passiveDelegationTransactionRewards
                                <> put foundationTransactionRewards
                                <> put nextPaydayEpoch
                                <> put nextPaydayMintRate

instance MonadBlobStore m => Cacheable m PoolRewards where
    cache pr@PoolRewards{nextPaydayEpoch = nextPaydayEpoch, nextPaydayMintRate = nextPaydayMintRate} = do
        nextCapital <- cache (nextCapital pr)
        currentCapital <- cache (currentCapital pr)
        bakerPoolRewardDetails <- cache (bakerPoolRewardDetails pr)
        passiveDelegationTransactionRewards <- cache (passiveDelegationTransactionRewards pr)
        foundationTransactionRewards <- cache (foundationTransactionRewards pr)
        return PoolRewards{..}

makerPersistentPoolRewards :: MonadBlobStore m => BasicPoolRewards.PoolRewards -> m PoolRewards
makerPersistentPoolRewards bpr = do
    nc <- refMake (_unhashed (BasicPoolRewards.nextCapital bpr))
    cc <- refMake (_unhashed (BasicPoolRewards.currentCapital bpr))
    bprd <- LFMBT.fromAscList $ BasicLFMBT.toAscList $ BasicPoolRewards.bakerPoolRewardDetails bpr
    return
        PoolRewards
            { nextCapital = nc,
              currentCapital = cc,
              bakerPoolRewardDetails = bprd,
              passiveDelegationTransactionRewards = BasicPoolRewards.passiveDelegationTransactionRewards bpr,
              foundationTransactionRewards = BasicPoolRewards.foundationTransactionRewards bpr,
              nextPaydayEpoch = BasicPoolRewards.nextPaydayEpoch bpr,
              nextPaydayMintRate = BasicPoolRewards.nextPaydayMintRate bpr
            }

-- |The empty 'PoolRewards'.
emptyPoolRewards :: MonadBlobStore m => m PoolRewards
emptyPoolRewards = makerPersistentPoolRewards BasicPoolRewards.emptyPoolRewards

-- |List of baker and number of blocks baked by this baker in the reward period.
bakerBlockCounts :: MonadBlobStore m => PoolRewards -> m [(BakerId, Word64)]
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

-- |Rotate the capital distribution, so that the current capital distribution is replaced by the
-- next one, and set up empty pool rewards.
rotateCapitalDistribution ::
    (MonadBlobStore m, Reference m ref PoolRewards) =>
    ref PoolRewards ->
    m (ref PoolRewards)
rotateCapitalDistribution oldPoolRewards = do
    pr <- refLoad oldPoolRewards
    nextCap <- refLoad (nextCapital pr)
    rewardDetails <-
        LFMBT.fromAscList $
            replicate
                (Vec.length (bakerPoolCapital nextCap))
                BasicPoolRewards.emptyBakerPoolRewardDetails
    refMake $
        pr
            { currentCapital = nextCapital pr,
              bakerPoolRewardDetails = rewardDetails
            }

setNextCapitalDistribution ::
    (MonadBlobStore m, Reference m ref PoolRewards) =>
    [(BakerId, Amount, [(DelegatorId, Amount)])] ->
    [(DelegatorId, Amount)] ->
    ref PoolRewards ->
    m (ref PoolRewards)
setNextCapitalDistribution bakers passive oldPoolRewards = do
    let bakerPoolCapital = Vec.fromList $ map mkBakCap bakers
    let passiveDelegatorsCapital = Vec.fromList $ map mkDelCap passive
    capDist <- refMake $ CapitalDistribution{..}
    pr <- refLoad oldPoolRewards
    refMake pr{nextCapital = capDist}
  where
    mkBakCap (bcBakerId, bcBakerEquityCapital, dels) =
        let bcDelegatorCapital = Vec.fromList $ map mkDelCap dels
         in BakerCapital{..}
    mkDelCap (dcDelegatorId, dcDelegatorCapital) =
        DelegatorCapital{..}

-- |The total capital passively delegated in the current reward period capital distribution.
currentPassiveDelegationCapital ::
    (MonadBlobStore m) =>
    PoolRewards ->
    m Amount
currentPassiveDelegationCapital PoolRewards{..} =
    Vec.sum . fmap dcDelegatorCapital . passiveDelegatorsCapital <$> refLoad currentCapital
