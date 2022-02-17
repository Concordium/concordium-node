{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Basic.BlockState.PoolRewards where

import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Vector as Vec
import Data.Word

import Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMBT
import Concordium.Utils
import Lens.Micro.Platform

data BakerPoolRewardDetails = BakerPoolRewardDetails
    { -- |The number of blocks baked by this baker in the reward period
      blockCount :: !Word64,
      -- |The total transaction fees accrued to this pool in the reward period
      transactionFeesAccrued :: !Amount,
      -- |Whether the pool contributed to a finalization proof in the reward period
      finalizationAwake :: !Bool
    }
    deriving (Show)

instance Serialize BakerPoolRewardDetails where
    put BakerPoolRewardDetails{..} = do
        put blockCount
        put transactionFeesAccrued
        put finalizationAwake

    get = BakerPoolRewardDetails <$> get <*> get <*> get

instance HashableTo Hash.Hash BakerPoolRewardDetails where
    getHash = Hash.hash . encode

-- |Baker pool reward details with no rewards accrued to the baker.
emptyBakerPoolRewardDetails :: BakerPoolRewardDetails
emptyBakerPoolRewardDetails =
    BakerPoolRewardDetails
        { blockCount = 0,
          transactionFeesAccrued = 0,
          finalizationAwake = False
        }

instance Monad m => MHashableTo m Hash.Hash BakerPoolRewardDetails

data DelegatorCapital = DelegatorCapital
    { -- |'DelegatorId' of the delegator
      dcDelegatorId :: !DelegatorId,
      -- |'Amount' staked by the delegator
      dcDelegatorCapital :: !Amount
    }
    deriving (Show)

instance Serialize DelegatorCapital where
    put DelegatorCapital{..} = do
        put dcDelegatorId
        put dcDelegatorCapital
    get = do
        dcDelegatorId <- get
        dcDelegatorCapital <- get
        return DelegatorCapital{..}

instance HashableTo Hash.Hash DelegatorCapital where
    getHash = Hash.hash . encode

instance Monad m => MHashableTo m Hash.Hash DelegatorCapital

data BakerCapital = BakerCapital
    { -- |'BakerId' of the pool owner
      bcBakerId :: !BakerId,
      -- |Equity capital of the pool owner
      bcBakerEquityCapital :: !Amount,
      -- |Capital of each baker delegated to this pool
      bcDelegatorCapital :: !(Vec.Vector DelegatorCapital)
    }
    deriving (Show)

instance Serialize BakerCapital where
    put BakerCapital{..} = do
        put bcBakerId
        put bcBakerEquityCapital
        put (Vec.length bcDelegatorCapital) <> mapM_ put bcDelegatorCapital

    get = do
        bcBakerId <- get
        bcBakerEquityCapital <- get
        bcDelegatorCapital <- flip Vec.generateM (const get) =<< get
        return BakerCapital{..}

instance HashableTo Hash.Hash BakerCapital where
    getHash BakerCapital{..} = Hash.hash $
        runPut $ do
            put bcBakerId
            put bcBakerEquityCapital
            put $ getHash @Hash.Hash $ LFMBT.fromFoldable @Word64 bcDelegatorCapital

instance Monad m => MHashableTo m Hash.Hash BakerCapital

-- |The total capital delegated to the baker.
bcTotalDelegatorCapital :: BakerCapital -> Amount
bcTotalDelegatorCapital = sum . fmap dcDelegatorCapital . bcDelegatorCapital

data CapitalDistribution = CapitalDistribution
    { -- |Capital associated with baker pools
      bakerPoolCapital :: !(Vec.Vector BakerCapital),
      -- |Capital associated with the L-pool
      lPoolCapital :: !(Vec.Vector DelegatorCapital)
    }
    deriving (Show)

instance Serialize CapitalDistribution where
    put CapitalDistribution{..} = do
        put (Vec.length bakerPoolCapital) <> mapM_ put bakerPoolCapital
        put (Vec.length lPoolCapital) <> mapM_ put lPoolCapital

    get = do
        bakerPoolCapital <- flip Vec.generateM (const get) =<< get
        lPoolCapital <- flip Vec.generateM (const get) =<< get
        return CapitalDistribution{..}

instance HashableTo Hash.Hash CapitalDistribution where
    getHash CapitalDistribution{..} =
        Hash.hashOfHashes
            (getHash (LFMBT.fromFoldable @Word64 bakerPoolCapital))
            (getHash (LFMBT.fromFoldable @Word64 lPoolCapital))

instance Monad m => MHashableTo m Hash.Hash CapitalDistribution

-- |The empty 'CapitalDistribution'.
emptyCapitalDistribution :: CapitalDistribution
emptyCapitalDistribution = CapitalDistribution Vec.empty Vec.empty

-- |Construct a 'CapitalDistribution' from a sorted list of bakers, with their equity capital and
-- delegator capitals, and the L-Pool delegator capitals.
makeCapitalDistribution :: [(BakerId, Amount, [(DelegatorId, Amount)])] -> [(DelegatorId, Amount)] -> CapitalDistribution
makeCapitalDistribution bakers lpool =
    CapitalDistribution
        { bakerPoolCapital = Vec.fromList $ mkBakerCapital <$> bakers,
          lPoolCapital = Vec.fromList $ mkDelegatorCapital <$> lpool
        }
  where
    mkBakerCapital (bcBakerId, bcBakerEquityCapital, dels) =
        let bcDelegatorCapital = Vec.fromList $ mkDelegatorCapital <$> dels
         in BakerCapital{..}
    mkDelegatorCapital (dcDelegatorId, dcDelegatorCapital) = DelegatorCapital{..}

-- |Details of rewards accruing over the course of a reward period, and details about the capital
-- distribution for this reward period and (possibly) the next.
data PoolRewards = PoolRewards
    { -- |The capital distribution for the next reward period.
      -- This is updated the epoch before a payday.
      nextCapital :: !(Hashed CapitalDistribution),
      -- |The capital distribution for the current reward period.
      currentCapital :: !(Hashed CapitalDistribution),
      -- |The details of rewards accruing to baker pools.
      -- These are indexed by the index of the baker in the capital distribution (_not_ the BakerId).
      bakerPoolRewardDetails :: !(LFMBT.LFMBTree Word64 BakerPoolRewardDetails),
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

-- |Traversal for accessing the reward details for a particular baker ID.
rewardDetails :: BakerId -> Traversal' PoolRewards BakerPoolRewardDetails
rewardDetails bid f pr
    | Just (index, _) <- mindex =
        (\bprd -> pr{bakerPoolRewardDetails = bprd})
            <$> ix (fromIntegral index) f (bakerPoolRewardDetails pr)
    | otherwise = pure pr
  where
    mindex = binarySearchI bcBakerId (bakerPoolCapital $ _unhashed $ currentCapital pr) bid

-- |Look up the baker capital and reward details for a baker ID.
lookupBakerCapitalAndRewardDetails :: BakerId -> PoolRewards -> Maybe (BakerCapital, BakerPoolRewardDetails)
lookupBakerCapitalAndRewardDetails bid PoolRewards{..} = do
    (index, capital) <- binarySearchI bcBakerId (bakerPoolCapital $ _unhashed currentCapital) bid
    rds <- bakerPoolRewardDetails ^? ix (fromIntegral index)
    return (capital, rds)

instance HashableTo Hash.Hash PoolRewards where
    getHash PoolRewards{..} =
        Hash.hashOfHashes (getHash nextCapital) $
            Hash.hashOfHashes (getHash currentCapital) $
                Hash.hashOfHashes (getHash bakerPoolRewardDetails) $
                    getHash $
                        runPut $
                            put lPoolTransactionRewards
                                <> put foundationTransactionRewards
                                <> put nextPaydayEpoch
                                <> put nextPaydayMintRate

-- |The empty 'PoolRewards'.
emptyPoolRewards :: PoolRewards
emptyPoolRewards =
    PoolRewards
        { nextCapital = makeHashed emptyCapitalDistribution,
          currentCapital = makeHashed emptyCapitalDistribution,
          bakerPoolRewardDetails = LFMBT.empty,
          lPoolTransactionRewards = 0,
          foundationTransactionRewards = 0,
          nextPaydayEpoch = 0,
          nextPaydayMintRate = MintRate 0 0
        }

-- |A 'Putter' for 'PoolRewards'.
putPoolRewards :: Putter PoolRewards
putPoolRewards PoolRewards{..} = do
    put (_unhashed nextCapital)
    put (_unhashed currentCapital)
    put bakerPoolRewardDetails
    put lPoolTransactionRewards
    put foundationTransactionRewards
    put nextPaydayEpoch
    put nextPaydayMintRate

getPoolRewards :: Get PoolRewards
getPoolRewards = do
    nextCapital <- makeHashed <$> get
    currentCapital <- makeHashed <$> get
    bakerPoolRewardDetails <- get
    lPoolTransactionRewards <- get
    foundationTransactionRewards <- get
    nextPaydayEpoch <- get
    nextPaydayMintRate <- get
    return PoolRewards{..}

-- |List of baker and number of blocks baked by this baker in the reward period.
bakerBlockCounts :: PoolRewards -> [(BakerId, Word64)]
bakerBlockCounts PoolRewards{..} =
    zipWith
        bc
        (Vec.toList (bakerPoolCapital (_unhashed currentCapital)))
        (LFMBT.toAscPairList bakerPoolRewardDetails)
  where
    bc BakerCapital{..} (_, BakerPoolRewardDetails{..}) = (bcBakerId, blockCount)

-- |Rotate the capital distribution and set up empty pool rewards.
rotateCapitalDistribution :: PoolRewards -> PoolRewards
rotateCapitalDistribution pr =
    pr
        { currentCapital = nextCapital pr,
          bakerPoolRewardDetails =
            LFMBT.fromList $
                replicate
                    (Vec.length (bakerPoolCapital (_unhashed (nextCapital pr))))
                    emptyBakerPoolRewardDetails
        }

-- |Set the next 'CapitalDistribution' given a sorted list of bakers, with their equity capital and
-- delegator capitals, and the L-Pool delegator capitals.
setNextCapitalDistribution ::
    [(BakerId, Amount, [(DelegatorId, Amount)])] ->
    [(DelegatorId, Amount)] ->
    PoolRewards ->
    PoolRewards
setNextCapitalDistribution bakers lpool pr =
    pr{nextCapital = makeHashed $ makeCapitalDistribution bakers lpool}

makePoolRewardsForMigration ::
    -- |Current epoch bakers and stakes, in ascending order of 'BakerId'.
    Vec.Vector (BakerId, Amount) ->
    -- |Next epoch bakers and stakes, in ascending order of 'BakerId'.
    Vec.Vector (BakerId, Amount) ->
    -- |'BakerId's of baked blocks
    [BakerId] ->
    -- |Epoch of next payday
    Epoch ->
    -- |Mint rate for the next payday
    MintRate ->
    PoolRewards
makePoolRewardsForMigration curBakers nextBakers bakedBlocks npEpoch npMintRate =
    PoolRewards
        { nextCapital = makeCD nextBakers,
          currentCapital = makeCD curBakers,
          bakerPoolRewardDetails = LFMBT.fromFoldable (makePRD <$> curBakers),
          lPoolTransactionRewards = 0,
          foundationTransactionRewards = 0,
          nextPaydayEpoch = npEpoch,
          nextPaydayMintRate = npMintRate
        }
  where
    makeCD bkrs =
        makeHashed $
            CapitalDistribution
                { bakerPoolCapital = makeBakerCapital <$> bkrs,
                  lPoolCapital = Vec.empty
                }
    makeBakerCapital (bid, amt) = BakerCapital bid amt Vec.empty
    blockCounts = foldr (\bid -> at' bid . non 0 %~ (+ 1)) Map.empty bakedBlocks
    makePRD (bid, _) =
        BakerPoolRewardDetails
            { blockCount = Map.findWithDefault 0 bid blockCounts,
              transactionFeesAccrued = 0,
              finalizationAwake = False
            }

-- |Make initial pool rewards for a genesis block state.
makeInitialPoolRewards ::
    -- |Genesis bakers and delegators
    [(BakerId, Amount, [(DelegatorId, Amount)])] ->
    -- |Genesis L-Pool delegators
    [(DelegatorId, Amount)] ->
    -- |Epoch of next payday
    Epoch ->
    -- |Mint rate
    MintRate ->
    PoolRewards
makeInitialPoolRewards bakers lpool npEpoch npMintRate =
    PoolRewards
        { nextCapital = initCD,
          currentCapital = initCD,
          bakerPoolRewardDetails = LFMBT.fromList (emptyBakerPoolRewardDetails <$ bakers),
          lPoolTransactionRewards = 0,
          foundationTransactionRewards = 0,
          nextPaydayEpoch = npEpoch,
          nextPaydayMintRate = npMintRate
        }
  where
    initCD = makeHashed $ makeCapitalDistribution bakers lpool

-- |The total capital delegated to the L-Pool in the current reward period capital distribution.
currentLPoolDelegatedCapital :: PoolRewards -> Amount
currentLPoolDelegatedCapital =
    Vec.sum . fmap dcDelegatorCapital . lPoolCapital . _unhashed . currentCapital