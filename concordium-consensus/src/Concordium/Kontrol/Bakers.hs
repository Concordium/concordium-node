{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |Functionality for handling baker changes based on epoch boundaries.
module Concordium.Kontrol.Bakers where

import Control.Arrow
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.UpdateQueues

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState
import Concordium.Types.SeedState

-- |Compute the cap on the amount that may be delegated to a baker.
-- It is assumed that the total capital is at least the baker equity capital plus the baker
-- delegated capital.
delegatedCapitalCap ::
    -- |Pool parameters
    PoolParameters 'ChainParametersV1 ->
    -- |Current total capital
    Amount ->
    -- |Baker equity capital
    Amount ->
    -- |Baker delegated capital
    Amount ->
    Amount
delegatedCapitalCap poolParams totalCap bakerCap delCap = min leverageCap boundCap
  where
    capBound = poolParams ^. ppCapitalBound
    leverageFactor = poolParams ^. ppLeverageBound
    leverageCap
        | leverageFactor >= 1 = applyLeverageFactor leverageFactor bakerCap - bakerCap
        | otherwise = 0
    capBoundR = fractionToRational capBound
    preBoundCap = capBoundR * toRational (totalCap - delCap) - toRational bakerCap
    boundCap
        | preBoundCap > 0 = truncate (preBoundCap / (1 - capBoundR))
        | otherwise = 0

-- |Transition the bakers
transitionEpochBakersP4 ::
    (BlockStateOperations m, AccountVersionFor (MPV m) ~ 'AccountV1) =>
    UpdatableBlockState m ->
    -- |Whether we have transitioned over the pre-payday epoch boundary
    Bool ->
    -- |Whether we have transitioned over the payday epoch boundary
    Bool ->
    m (UpdatableBlockState m)
transitionEpochBakersP4 bs0 prePayday payday = do
    bs1 <- bsoRotateCurrentEpochBakers bs0
    undefined

-- |Process a set of bakers and delegators to apply pending changes that are effective.
applyPendingChanges ::
    (PendingChangeEffective 'AccountV1 -> Bool) ->
    ([ActiveBakerInfo' bakerInfoRef], [ActiveDelegatorInfo]) ->
    ([ActiveBakerInfo' bakerInfoRef], [ActiveDelegatorInfo])
applyPendingChanges isEffective (bakers0, lpool0) =
    foldr
        processBaker
        ([], processDelegators lpool0)
        bakers0
  where
    -- Apply effective pending changes to a list of delegators
    processDelegators [] = []
    processDelegators (d@ActiveDelegatorInfo{..} : ds) = case activeDelegatorPendingChange of
        RemoveStake et | isEffective et -> processDelegators ds
        ReduceStake amt et
            | isEffective et ->
                d{activeDelegatorStake = amt, activeDelegatorPendingChange = NoChange} :
                processDelegators ds
        _ -> d : processDelegators ds
    -- Merge two disjoint ordered lists of delegators
    mergeDelegators [] l = l
    mergeDelegators l [] = l
    mergeDelegators (d1 : ds1) (d2 : ds2)
        | activeDelegatorId d1 <= activeDelegatorId d2 = d1 : mergeDelegators ds1 (d2 : ds2)
        | otherwise = d2 : mergeDelegators (d1 : ds1) ds2
    -- Process a baker, adding it to the list of bakers if it is still a baker, and otherwise
    -- adding its delegators to the lpool.
    processBaker baker@ActiveBakerInfo{..} (bakers, lpool) = case activeBakerPendingChange of
        RemoveStake et | isEffective et -> (bakers, mergeDelegators pDelegators lpool)
        ReduceStake amt et
            | isEffective et ->
                ( baker
                    { activeBakerEquityCapital = amt,
                      activeBakerPendingChange = NoChange,
                      activeBakerDelegators = pDelegators
                    } :
                  bakers,
                  lpool
                )
        _ -> (baker : bakers, lpool)
      where
        pDelegators = processDelegators activeBakerDelegators

-- |Compute the timestamp of the start of an epoch based on the genesis data.
epochTimestamp :: (BasicGenesisData gd) => gd -> Epoch -> Timestamp
epochTimestamp gd epoch =
    addDuration
        (gdGenesisTime gd)
        (fromIntegral epoch * fromIntegral (gdEpochLength gd) * gdSlotDuration gd)

-- |Determine the test for whether a pending change is effective at a payday based
-- on the epoch of the payday.
effectiveTest ::
    (TreeStateMonad m) =>
    -- |Payday epoch
    Epoch ->
    m (PendingChangeEffective 'AccountV1 -> Bool)
effectiveTest paydayEpoch = do
    genData <- getGenesisData
    let paydayEpochTime = epochTimestamp genData paydayEpoch
    return (\(PendingChangeEffectiveV1 t) -> t <= paydayEpochTime)

-- |A helper datatype for computing the stake and capital distribution.
-- This is intentionally lazy, as the caller may not wish to evaluate all of the fields, but
-- constructing them together can avoid unnecessary duplication of work.
data BakerStakesAndCapital m = BakerStakesAndCapital
    { -- |The baker info and stake for each baker.
      bakerStakes :: [(BakerInfoRef m, Amount)],
      -- |Determine the capital distribution for bakers.
      bakerCapitalsM :: (AccountOperations m) => m [(BakerId, Amount, [(DelegatorId, Amount)])],
      -- |The capital distribution for the L-pool.
      lpoolCapital :: [(DelegatorId, Amount)]
    }

-- |Compute the baker stakes and capital distribution.
computeBakerStakesAndCapital ::
    forall m.
    PoolParameters 'ChainParametersV1 ->
    [ActiveBakerInfo m] ->
    [ActiveDelegatorInfo] ->
    BakerStakesAndCapital m
computeBakerStakesAndCapital poolParams activeBakers lpoolDelegators = BakerStakesAndCapital{..}
  where
    leverage = poolParams ^. ppLeverageBound
    capitalBound = poolParams ^. ppCapitalBound
    poolCapital ActiveBakerInfo{..} = activeBakerEquityCapital + sum (activeDelegatorStake <$> activeBakerDelegators)
    poolCapitals = poolCapital <$> activeBakers
    totalCapital = sum poolCapitals + sum (activeDelegatorStake <$> lpoolDelegators)
    capLimit = takeFraction capitalBound totalCapital
    makeBakerStake ActiveBakerInfo{..} poolCap =
        ( activeBakerInfoRef,
          minimum
            [ poolCap,
              applyLeverageFactor leverage activeBakerEquityCapital,
              capLimit
            ]
        )
    bakerStakes = zipWith makeBakerStake activeBakers poolCapitals
    delegatorCapital ActiveDelegatorInfo{..} = (activeDelegatorId, activeDelegatorStake)
    bakerCapital ActiveBakerInfo{..} = do
        bid <- _bakerIdentity <$> derefBakerInfo activeBakerInfoRef
        return (bid, activeBakerEquityCapital, delegatorCapital <$> activeBakerDelegators)
    bakerCapitalsM :: AccountOperations m => m [(BakerId, Amount, [(DelegatorId, Amount)])]
    bakerCapitalsM = mapM bakerCapital activeBakers
    lpoolCapital = (activeDelegatorId &&& activeDelegatorStake) <$> lpoolDelegators

-- |Generate and set the next epoch bakers and next capital based on the current active bakers.
generateNextBakers ::
    ( TreeStateMonad m,
      AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1
    ) =>
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
generateNextBakers bs0 = do
    paydayEpoch <- bsoGetPaydayEpoch bs0
    isEffective <- effectiveTest paydayEpoch
    -- Determine the bakers and delegators for the next reward period, accounting for any
    -- stake reductions that are currently pending on active bakers with effective time at
    -- or before the next payday.
    (activeBakers, lpoolDelegators) <-
        applyPendingChanges isEffective
            <$> bsoGetActiveBakersAndDelegators bs0
    cps <- bsoGetChainParameters bs0
    let BakerStakesAndCapital{..} =
            computeBakerStakesAndCapital
                (cps ^. cpPoolParameters)
                activeBakers
                lpoolDelegators
    bs1 <- bsoSetNextEpochBakers bs0 bakerStakes
    bakerCapitals <- bakerCapitalsM
    bsoSetNextCapitalDistribution bs1 bakerCapitals lpoolCapital

-- |Compute the epoch of the last payday at or before the given epoch.
-- This accounts for changes to the reward period.
--
-- PRECONDITION: The target epoch must be at least the next payday epoch.
-- TODO: Add tests
paydayEpochBefore ::
    -- |Current length of a reward period
    RewardPeriodLength ->
    -- |Pending updates to the reward period length
    [(Timestamp, RewardPeriodLength)] ->
    -- |Genesis timestamp
    Timestamp ->
    -- |Epoch duration
    Duration ->
    -- |Next payday epoch
    Epoch ->
    -- |The epoch to compute the last payday before
    Epoch ->
    Epoch
paydayEpochBefore rewardLength pendingLengths genesis epochDuration nextPayday targetEpoch = lastPayday
  where
    epochToTimestamp epoch = addDuration genesis (fromIntegral epoch * epochDuration)
    targetTime = epochToTimestamp targetEpoch
    -- Find the first payday (starting from startEpoch) that is no sooner than the given timestamp.
    -- This is a very naive implementation that is easy to see is correct, and is likely fast
    -- enough in practice. TODO: improve this
    paydayAfterTimestamp startEpoch pdLen ts
        | epochToTimestamp startEpoch >= ts = startEpoch
        | otherwise = paydayAfterTimestamp (startEpoch + pdLen) pdLen ts
    go fromEpoch curPLen ((changeTime, nextPLen) : changes)
        | changeTime < targetTime,
          let pac = paydayAfterTimestamp fromEpoch (rewardPeriodEpochs curPLen) changeTime,
          pac < targetEpoch =
            go pac nextPLen changes
    go fromEpoch curPLen _ = targetEpoch - ((targetEpoch - fromEpoch) `mod` rewardPeriodEpochs curPLen)
    lastPayday = go nextPayday rewardLength pendingLengths

getSlotBakersV1 ::
    forall m.
    ( BlockStateOperations m,
      AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      TreeStateMonad m
    ) =>
    BlockState m ->
    Slot ->
    m FullBakers
getSlotBakersV1 bs slot = do
    SeedState{epochLength, epoch = blockEpoch} <- getSeedState bs
    let slotEpoch = fromIntegral $ slot `quot` epochLength
    nextPayday <- getPaydayEpoch bs

    if slotEpoch < nextPayday
        then getCurrentEpochBakers bs
        else do
            chainParams <- _currentParameters <$> getUpdates bs
            let rewardPeriodLen = chainParams ^. cpTimeParameters . tpRewardPeriodLength
            let paydayLength = rewardPeriodEpochs rewardPeriodLen
            if blockEpoch + 1 == nextPayday && slotEpoch < nextPayday + paydayLength
                then getNextEpochBakers bs
                else do
                    -- The slot time is at least the next payday
                    -- First we compute the epoch of last payday that is no later than the given slot.
                    pendingLengths <- (each . _2 %~ _tpRewardPeriodLength) <$> getPendingTimeParameters bs
                    gd <- getGenesisData
                    let epochDuration = gdSlotDuration gd * fromIntegral (gdEpochLength gd)
                        latestPayday =
                            paydayEpochBefore
                                rewardPeriodLen
                                pendingLengths
                                (gdGenesisTime gd)
                                epochDuration
                                nextPayday
                                blockEpoch

                    -- From this we can determine which cooldowns have elapsed.
                    isEffective <- effectiveTest latestPayday

                    -- Determine the bakers and delegators for the next reward period, accounting for any
                    -- stake reductions that are currently pending on active bakers with effective time at
                    -- or before the next payday.
                    (activeBakers, lpoolDelegators) <-
                        applyPendingChanges isEffective
                            <$> getActiveBakersAndDelegators bs
                    -- Determine the pool parameters that would be effective the epoch before the payday
                    pendingPoolParams <- getPendingPoolParameters bs
                    let latestPaydayTime = epochTimestamp gd latestPayday
                        ePoolParams _ ((et, pp') : updates)
                            | addDuration et epochDuration <= latestPaydayTime =
                                ePoolParams pp' updates
                        ePoolParams pp _ = pp
                        effectivePoolParameters = ePoolParams (chainParams ^. cpPoolParameters) pendingPoolParams
                        bsc = computeBakerStakesAndCapital @m effectivePoolParameters activeBakers lpoolDelegators
                    let mkFullBaker (biRef, _bakerStake) = do
                            _theBakerInfo <- derefBakerInfo biRef
                            return FullBakerInfo{..}
                    fullBakerInfos <- mapM mkFullBaker (Vec.fromList $ bakerStakes bsc)
                    let bakerTotalStake = sum $ _bakerStake <$> fullBakerInfos
                    return FullBakers{..}
