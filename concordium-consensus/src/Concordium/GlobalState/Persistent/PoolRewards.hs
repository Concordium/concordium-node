module Concordium.GlobalState.Persistent.PoolRewards (
    module Concordium.GlobalState.Basic.BlockState.PoolRewards,
    PoolRewards (..),
    emptyPoolRewards,
    makePoolRewards,
    putPoolRewards,
    bakerBlockCounts,
    setNextCapitalDistribution,
    currentLPoolDelegatedCapital,
    lookupBakerCapitalAndRewardDetails,
) where

import Data.Serialize
import qualified Data.Vector as Vec
import Data.Word

import Concordium.Crypto.SHA256 as Hash

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as BasicLFMBT
import Concordium.GlobalState.Rewards

import Concordium.GlobalState.Basic.BlockState.PoolRewards (
    BakerCapital (..),
    BakerPoolRewardDetails (..),
    CapitalDistribution (..),
    DelegatorCapital (..),
    bcTotalDelegatorCapital,
    emptyCapitalDistribution,
 )
import qualified Concordium.GlobalState.Basic.BlockState.PoolRewards as BasicPoolRewards

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBT

import Concordium.Utils.Serialization.Put

-- |Details of rewards accruing over the course of a reward period, and details about the capital
-- distribution for this reward period and (possibly) the next.
data PoolRewards = PoolRewards
    { -- |The capital distribution for the next reward period.
      -- This is updated the epoch before a payday.
      nextCapital :: !(HashedBufferedRef CapitalDistribution),
      -- |The capital distribution for the current reward period.
      currentCapital :: !(HashedBufferedRef CapitalDistribution),
      -- |The details of rewards accruing to baker pools.
      -- These are indexed by the index of the baker in the capital distribution (_not_ the BakerId).
      bakerPoolRewardDetails :: !(LFMBT.LFMBTree Word64 BufferedRef BakerPoolRewardDetails),
      -- |The transaction reward amount accruing to the L-pool.
      lPoolTransactionRewards :: !Amount,
      -- |The transaction reward fraction accruing to the foundation.
      foundationTransactionRewards :: !Amount,
      -- |The next payday occurs at the start of this epoch.
      nextPaydayEpoch :: !Epoch,
      -- |The rate at which tokens are minted for the current reward period.
      nextPaydayMintRate :: !MintRate
    }
    deriving (Show)

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
        (pBakerPoolRewardDetails, bakerPoolRewardDetails) <- storeUpdate (bakerPoolRewardDetails pr0)
        (pLPoolTransactionRewards, lPoolTransactionRewards) <- storeUpdate (lPoolTransactionRewards pr0)
        (pFoundationTransactionRewards, foundationTransactionRewards) <- storeUpdate (foundationTransactionRewards pr0)
        (pNextPaydayEpoch, nextPaydayEpoch) <- storeUpdate (nextPaydayEpoch pr0)
        (pNextPaydayMintRate, nextPaydayMintRate) <- storeUpdate (nextPaydayMintRate pr0)
        let p = do
                pNextCapital
                pCurrentCapital
                pBakerPoolRewardDetails
                pLPoolTransactionRewards
                pFoundationTransactionRewards
                pNextPaydayEpoch
                pNextPaydayMintRate
        return (p, PoolRewards{..})
    store bsp = fst <$> storeUpdate bsp
    load = do
        mNextCapital <- label "Next Capital" load
        mCurrentCapital <- label "Current Capital" load
        mBakerPoolRewardDetails <- label "Baker Pool Reward Details" load
        mLPoolTransactionRewards <- label "L-Pool Transaction Rewards" load
        mFoundationTransactionRewards <- label "Foundation Transaction Rewards" load
        mNextPaydayEpoch <- label "Next Payday Epoch" load
        mNextPaydayMintRate <- label "Next Payday Mint Rate" load
        return $! do
            nextCapital <- mNextCapital
            currentCapital <- mCurrentCapital
            bakerPoolRewardDetails <- mBakerPoolRewardDetails
            lPoolTransactionRewards <- mLPoolTransactionRewards
            foundationTransactionRewards <- mFoundationTransactionRewards
            nextPaydayEpoch <- mNextPaydayEpoch
            nextPaydayMintRate <- mNextPaydayMintRate
            return $! PoolRewards{..}

putPoolRewards :: (MonadBlobStore m, MonadPut m) => PoolRewards -> m ()
putPoolRewards PoolRewards{..} = do
    liftPut . put =<< refLoad nextCapital
    liftPut . put =<< refLoad currentCapital
    liftPut =<< store bakerPoolRewardDetails
    liftPut $ do
        put lPoolTransactionRewards
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
                            put lPoolTransactionRewards
                                <> put foundationTransactionRewards
                                <> put nextPaydayEpoch
                                <> put nextPaydayMintRate

instance MonadBlobStore m => Cacheable m PoolRewards where
    cache pr@PoolRewards{nextPaydayEpoch = nextPaydayEpoch, nextPaydayMintRate = nextPaydayMintRate} = do
        nextCapital <- cache (nextCapital pr)
        currentCapital <- cache (currentCapital pr)
        bakerPoolRewardDetails <- cache (bakerPoolRewardDetails pr)
        lPoolTransactionRewards <- cache (lPoolTransactionRewards pr)
        foundationTransactionRewards <- cache (foundationTransactionRewards pr)
        return PoolRewards{..}

makePoolRewards :: MonadBlobStore m => BasicPoolRewards.PoolRewards -> m PoolRewards
makePoolRewards bpr = do
    nc <- refMake (_unhashed (BasicPoolRewards.nextCapital bpr))
    cc <- refMake (_unhashed (BasicPoolRewards.currentCapital bpr))
    bprd <- LFMBT.fromAscList $ BasicLFMBT.toAscList $ BasicPoolRewards.bakerPoolRewardDetails bpr
    return
        PoolRewards
            { nextCapital = nc,
              currentCapital = cc,
              bakerPoolRewardDetails = bprd,
              lPoolTransactionRewards = BasicPoolRewards.lPoolTransactionRewards bpr,
              foundationTransactionRewards = BasicPoolRewards.foundationTransactionRewards bpr,
              nextPaydayEpoch = BasicPoolRewards.nextPaydayEpoch bpr,
              nextPaydayMintRate = BasicPoolRewards.nextPaydayMintRate bpr
            }

-- |The empty 'PoolRewards'.
emptyPoolRewards :: MonadBlobStore m => m PoolRewards
emptyPoolRewards = makePoolRewards BasicPoolRewards.emptyPoolRewards

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

setNextCapitalDistribution ::
    (MonadBlobStore m, Reference m ref PoolRewards) =>
    [(BakerId, Amount, [(DelegatorId, Amount)])] ->
    [(DelegatorId, Amount)] ->
    ref PoolRewards ->
    m (ref PoolRewards)
setNextCapitalDistribution bakers lpool oldPoolRewards = do
    let bakerPoolCapital = Vec.fromList $ map mkBakCap bakers
    let lPoolCapital = Vec.fromList $ map mkDelCap lpool
    capDist <- refMake $ CapitalDistribution{..}
    pr <- refLoad oldPoolRewards
    refMake $ (pr{nextCapital = capDist})
  where
    mkBakCap (bcBakerId, bcBakerEquityCapital, dels) =
        let bcDelegatorCapital = Vec.fromList $ map mkDelCap dels
         in BakerCapital{..}
    mkDelCap (dcDelegatorId, dcDelegatorCapital) =
        DelegatorCapital{..}

-- |The total capital delegated to the L-Pool in the current reward period capital distribution.
currentLPoolDelegatedCapital ::
    (MonadBlobStore m) =>
    PoolRewards ->
    m Amount
currentLPoolDelegatedCapital PoolRewards{..} =
    Vec.sum . fmap dcDelegatorCapital . lPoolCapital <$> refLoad currentCapital