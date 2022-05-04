{-# LANGUAGE RankNTypes #-}

module Concordium.GlobalState.Basic.BlockState.PoolRewards where

import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform

import Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMBT
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Rewards
import Concordium.Utils

-- |'BakerPoolRewardDetails' tracks the rewards that have been earned by a baker pool in the current
-- reward period. These are used to pay out the rewards at the payday.
data BakerPoolRewardDetails = BakerPoolRewardDetails
    { -- |The number of blocks baked by this baker in the reward period
      blockCount :: !Word64,
      -- |The total transaction fees accrued to this pool in the reward period
      transactionFeesAccrued :: !Amount,
      -- |Whether the pool contributed to a finalization proof in the reward period
      finalizationAwake :: !Bool
    }
    deriving (Eq, Show)

instance Serialize BakerPoolRewardDetails where
    put BakerPoolRewardDetails{..} = do
        putWord64be blockCount
        put transactionFeesAccrued
        putBool finalizationAwake

    get = BakerPoolRewardDetails <$> getWord64be <*> get <*> getBool

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
      -- There must be an entry for each baker in 'currentCapital'.
      bakerPoolRewardDetails :: !(LFMBT.LFMBTree Word64 BakerPoolRewardDetails),
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

instance HashableTo PoolRewardsHash PoolRewards where
    getHash PoolRewards{..} =
        PoolRewardsHash . Hash.hashOfHashes (getHash nextCapital) $
            Hash.hashOfHashes (getHash currentCapital) $
                Hash.hashOfHashes (getHash bakerPoolRewardDetails) $
                    getHash $
                        runPut $
                            put passiveDelegationTransactionRewards
                                <> put foundationTransactionRewards
                                <> put nextPaydayEpoch
                                <> put nextPaydayMintRate

-- |The empty 'PoolRewards', where there are no bakers, delegators or rewards.
-- This is generally not used except as a dummy value for testing.
emptyPoolRewards :: PoolRewards
emptyPoolRewards =
    PoolRewards
        { nextCapital = makeHashed emptyCapitalDistribution,
          currentCapital = makeHashed emptyCapitalDistribution,
          bakerPoolRewardDetails = LFMBT.empty,
          passiveDelegationTransactionRewards = 0,
          foundationTransactionRewards = 0,
          nextPaydayEpoch = 0,
          nextPaydayMintRate = MintRate 0 0
        }

-- |A 'Putter' for 'PoolRewards'.
-- The 'bakerPoolRewardDetails' is serialized as a flat list, with the length implied by the
-- length of @bakerPoolCapital (_unhashed currentCapital)@.
putPoolRewards :: Putter PoolRewards
putPoolRewards PoolRewards{..} = do
    put (_unhashed nextCapital)
    put (_unhashed currentCapital)
    let bprdList = LFMBT.toAscList bakerPoolRewardDetails
    assert (Vec.length (bakerPoolCapital (_unhashed currentCapital)) == length bprdList) $
        mapM_ put $ LFMBT.toAscList bakerPoolRewardDetails
    put passiveDelegationTransactionRewards
    put foundationTransactionRewards
    put nextPaydayEpoch
    put nextPaydayMintRate

-- |Deserialize 'PoolRewards'.
-- The 'bakerPoolRewardDetails' is serialized as a flat list, with the length implied by the
-- length of @bakerPoolCapital (_unhashed currentCapital)@.
getPoolRewards :: Get PoolRewards
getPoolRewards = do
    nextCapital <- makeHashed <$> get
    currentCapital <- makeHashed <$> get
    bakerPoolRewardDetails <- LFMBT.fromList <$> replicateM
            (Vec.length (bakerPoolCapital (_unhashed currentCapital)))
            get
    passiveDelegationTransactionRewards <- get
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

-- |Rotate the capital distribution, so that the current capital distribution is replaced by the
-- next one, and set up empty pool rewards.
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

-- |Set the next 'CapitalDistribution'.
setNextCapitalDistribution ::
    CapitalDistribution ->
    PoolRewards ->
    PoolRewards
setNextCapitalDistribution cd pr =
    pr{nextCapital = makeHashed cd}

-- |Construct 'PoolRewards' for migrating from 'P3' to 'P4'.
-- This is used to construct the state of the genesis block.
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
          passiveDelegationTransactionRewards = 0,
          foundationTransactionRewards = 0,
          nextPaydayEpoch = npEpoch,
          nextPaydayMintRate = npMintRate
        }
  where
    makeCD bkrs =
        makeHashed $
            CapitalDistribution
                { bakerPoolCapital = makeBakerCapital <$> bkrs,
                  passiveDelegatorsCapital = Vec.empty
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
    -- |Genesis capital distribution
    CapitalDistribution ->
    -- |Epoch of next payday
    Epoch ->
    -- |Mint rate
    MintRate ->
    PoolRewards
makeInitialPoolRewards cdist npEpoch npMintRate =
    PoolRewards
        { nextCapital = initCD,
          currentCapital = initCD,
          bakerPoolRewardDetails = bprd,
          passiveDelegationTransactionRewards = 0,
          foundationTransactionRewards = 0,
          nextPaydayEpoch = npEpoch,
          nextPaydayMintRate = npMintRate
        }
  where
    initCD = makeHashed cdist
    bprd = LFMBT.fromList (replicate (length (bakerPoolCapital cdist)) emptyBakerPoolRewardDetails)

-- |The total capital passively delegated in the current reward period's capital distribution.
currentPassiveDelegationCapital :: PoolRewards -> Amount
currentPassiveDelegationCapital =
    Vec.sum . fmap dcDelegatorCapital . passiveDelegatorsCapital . _unhashed . currentCapital