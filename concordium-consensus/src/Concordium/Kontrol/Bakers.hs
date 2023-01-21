{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |Functionality for handling baker changes based on epoch boundaries.
module Concordium.Kontrol.Bakers where

import Data.Maybe
import Data.Monoid
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.SeedState
import Concordium.Types.UpdateQueues hiding (getUpdates)

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState

-- |Caps on the the stake that may be delegated to a baking pool.
-- N.B. The fields are not strict and should generally not be retained.
data PoolCaps = PoolCaps
    { -- |The (leverage bound minus 1) times the equity capital of the baker.
      leverageCap :: Amount,
      -- |The capital bound minus the baker's capital.
      boundCap :: Amount
    }

-- |Compute the caps on the amount that may be delegated to a baker.
-- It is assumed that the total capital is at least the baker equity capital plus the baker
-- delegated capital.
delegatedCapitalCaps ::
    -- |Pool parameters
    PoolParameters' 'PoolParametersVersion1 ->
    -- |Current total capital
    Amount ->
    -- |Baker equity capital
    Amount ->
    -- |Baker delegated capital
    Amount ->
    PoolCaps
delegatedCapitalCaps poolParams totalCap bakerCap delCap = PoolCaps{..}
  where
    capBound = poolParams ^. ppCapitalBound
    leverageFactor = poolParams ^. ppLeverageBound
    leverageCap
        | leverageFactor >= 1 = applyLeverageFactor leverageFactor bakerCap - bakerCap
        | otherwise = 0
    capBoundR = fractionToRational (theCapitalBound capBound)
    preBoundCap = capBoundR * toRational (totalCap - delCap) - toRational bakerCap
    boundCap
        | capBoundR >= 1 = maxBound
        | preBoundCap > 0 = truncate (preBoundCap / (1 - capBoundR))
        | otherwise = 0

-- |Compute the cap on the amount that may be delegated to a baker.
-- It is assumed that the total capital is at least the baker equity capital plus the baker
-- delegated capital.
delegatedCapitalCap ::
    -- |Pool parameters
    PoolParameters' 'PoolParametersVersion1 ->
    -- |Current total capital
    Amount ->
    -- |Baker equity capital
    Amount ->
    -- |Baker delegated capital
    Amount ->
    Amount
delegatedCapitalCap poolParams totalCap bakerCap delCap = min leverageCap boundCap
  where
    PoolCaps{..} = delegatedCapitalCaps poolParams totalCap bakerCap delCap

-- |Process a set of bakers and delegators to apply pending changes that are effective.
applyPendingChanges ::
    (Timestamp -> Bool) ->
    ([ActiveBakerInfo' bakerInfoRef], [ActiveDelegatorInfo]) ->
    ([ActiveBakerInfo' bakerInfoRef], [ActiveDelegatorInfo])
applyPendingChanges isEffective (bakers0, passive0) =
    foldr
        processBaker
        ([], processDelegators passive0)
        bakers0
  where
    -- Apply effective pending changes to a list of delegators
    processDelegators [] = []
    processDelegators (d@ActiveDelegatorInfo{..} : ds) = case activeDelegatorPendingChange of
        RemoveStake et | isEffective et -> processDelegators ds
        ReduceStake amt et
            | isEffective et ->
                d{activeDelegatorStake = amt, activeDelegatorPendingChange = NoChange}
                    : processDelegators ds
        _ -> d : processDelegators ds
    -- Merge two disjoint ordered lists of delegators
    mergeDelegators [] l = l
    mergeDelegators l [] = l
    mergeDelegators (d1 : ds1) (d2 : ds2)
        | activeDelegatorId d1 <= activeDelegatorId d2 = d1 : mergeDelegators ds1 (d2 : ds2)
        | otherwise = d2 : mergeDelegators (d1 : ds1) ds2
    -- Process a baker, adding it to the list of bakers if it is still a baker, and otherwise
    -- adding its delegators to passive delegation.
    processBaker baker@ActiveBakerInfo{..} (bakers, passive) = case activeBakerPendingChange of
        RemoveStake et | isEffective et -> (bakers, mergeDelegators pDelegators passive)
        ReduceStake amt et
            | isEffective et ->
                ( baker
                    { activeBakerEquityCapital = amt,
                      activeBakerPendingChange = NoChange,
                      activeBakerDelegators = pDelegators
                    }
                    : bakers,
                  passive
                )
        _ -> (baker{activeBakerDelegators = pDelegators} : bakers, passive)
      where
        pDelegators = processDelegators activeBakerDelegators

-- |Compute the timestamp of the start of an epoch based on the genesis data.
epochTimestamp :: GenesisConfiguration -> Epoch -> Timestamp
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
    m (Timestamp -> Bool)
effectiveTest paydayEpoch = do
    genData <- getGenesisData
    return (effectiveTest' genData paydayEpoch)

-- |Determine whether a pending change is effective at a payday based
-- on the epoch of the payday.
effectiveTest' :: GenesisConfiguration -> Epoch -> Timestamp -> Bool
effectiveTest' genData paydayEpoch = (<= paydayEpochTime)
  where
    paydayEpochTime = epochTimestamp genData paydayEpoch

-- |A helper datatype for computing the stake and capital distribution.
-- This is intentionally lazy, as the caller may not wish to evaluate all of the fields, but
-- constructing them together can avoid unnecessary duplication of work.
data BakerStakesAndCapital m = BakerStakesAndCapital
    { -- |The baker info and stake for each baker.
      bakerStakes :: [(BakerInfoRef m, Amount)],
      -- |Determine the capital distribution.
      capitalDistributionM :: m CapitalDistribution
    }

-- |Compute the baker stakes and capital distribution.
computeBakerStakesAndCapital ::
    forall m.
    (AccountOperations m) =>
    PoolParameters' 'PoolParametersVersion1 ->
    [ActiveBakerInfo m] ->
    [ActiveDelegatorInfo] ->
    BakerStakesAndCapital m
computeBakerStakesAndCapital poolParams activeBakers passiveDelegators = BakerStakesAndCapital{..}
  where
    leverage = poolParams ^. ppLeverageBound
    capitalBound = poolParams ^. ppCapitalBound
    poolCapital ActiveBakerInfo{..} = activeBakerEquityCapital + sum (activeDelegatorStake <$> activeBakerDelegators)
    poolCapitals = poolCapital <$> activeBakers
    totalCapital = sum poolCapitals + sum (activeDelegatorStake <$> passiveDelegators)
    capLimit = takeFraction (theCapitalBound capitalBound) totalCapital
    makeBakerStake ActiveBakerInfo{..} poolCap =
        ( activeBakerInfoRef,
          minimum
            [ poolCap,
              applyLeverageFactor leverage activeBakerEquityCapital,
              capLimit
            ]
        )
    bakerStakes = zipWith makeBakerStake activeBakers poolCapitals
    delegatorCapital ActiveDelegatorInfo{..} = DelegatorCapital activeDelegatorId activeDelegatorStake
    bakerCapital ActiveBakerInfo{..} = do
        bid <- _bakerIdentity <$> derefBakerInfo activeBakerInfoRef
        return
            BakerCapital
                { bcBakerId = bid,
                  bcBakerEquityCapital = activeBakerEquityCapital,
                  bcDelegatorCapital = Vec.fromList $ delegatorCapital <$> activeBakerDelegators
                }
    capitalDistributionM = do
        bakerPoolCapital <- Vec.fromList <$> mapM bakerCapital activeBakers
        let passiveDelegatorsCapital = Vec.fromList $ delegatorCapital <$> passiveDelegators
        return CapitalDistribution{..}

-- |Generate and set the next epoch bakers and next capital based on the current active bakers.
generateNextBakers ::
    ( TreeStateMonad m,
      PVSupportsDelegation (MPV m),
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1
    ) =>
    -- |The payday epoch
    Epoch ->
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
generateNextBakers paydayEpoch bs0 = do
    isEffective <- effectiveTest paydayEpoch
    -- Determine the bakers and delegators for the next reward period, accounting for any
    -- stake reductions that are currently pending on active bakers with effective time at
    -- or before the next payday.
    (activeBakers, passiveDelegators) <-
        applyPendingChanges isEffective
            <$> bsoGetActiveBakersAndDelegators bs0
    -- Note that we use the current value of the pool parameters as of this block.
    -- This should account for any updates that are effective at or before this block.
    -- However, this can include updates that are effective AFTER the epoch boundary but BEFORE
    -- the block. Thus the timing of the first block in the epoch before a payday can make a
    -- difference to the stake calculation for the next reward period. (Note also that if there
    -- are no blocks in this epoch, 'getSlotBakersP4' does not apply any updates.)
    cps <- bsoGetChainParameters bs0
    let BakerStakesAndCapital{..} =
            computeBakerStakesAndCapital
                (cps ^. cpPoolParameters)
                activeBakers
                passiveDelegators
    bs1 <- bsoSetNextEpochBakers bs0 bakerStakes
    capDist <- capitalDistributionM
    bsoSetNextCapitalDistribution bs1 capDist

-- |Compute the epoch of the last payday at or before the given epoch.
-- This accounts for changes to the reward period length.
--
-- PRECONDITION: The target epoch must be at least the next payday epoch.
-- TODO: Add tests
paydayEpochBefore ::
    -- |Current time parameters
    TimeParameters ->
    -- |Pending updates to the time parameters
    [(Slot, TimeParameters)] ->
    -- |Epoch length
    Slot ->
    -- |Next payday epoch
    Epoch ->
    -- |The slot to compute the last payday before
    Slot ->
    Epoch
paydayEpochBefore initialTimeParameters pendingTimeParameters epochLength nextPayday targetSlot = lastPayday
  where
    epochToSlot e = epochLength * fromIntegral e
    targetEpoch = fromIntegral $ targetSlot `div` epochLength
    -- Find the first payday (starting from startPaydayEpoch) that is no sooner than the given slot.
    -- This is a very naive implementation that is easy to see is correct, and is likely fast
    -- enough in practice. TODO: improve this
    paydayAfterSlot startPaydayEpoch pdLen slot
        | epochToSlot startPaydayEpoch >= slot = startPaydayEpoch
        | otherwise = paydayAfterSlot (startPaydayEpoch + pdLen) pdLen slot
    go fromPaydayEpoch curTPs ((changeSlot, nextTPs) : changes)
        | changeSlot < targetSlot,
          let pac =
                paydayAfterSlot
                    fromPaydayEpoch
                    (rewardPeriodEpochs (curTPs ^. tpRewardPeriodLength))
                    changeSlot,
          epochToSlot pac < targetSlot =
            go pac nextTPs changes
    go fromPaydayEpoch curTPs _ =
        targetEpoch
            - ((targetEpoch - fromPaydayEpoch) `mod` rewardPeriodEpochs (curTPs ^. tpRewardPeriodLength))
    lastPayday = go nextPayday initialTimeParameters pendingTimeParameters

-- |Get the updated value of the time parameters at a given slot, given the original time
-- parameters and the elapsed updates.
timeParametersAtSlot ::
    -- |Target slot
    Slot ->
    -- |Original time parameters
    TimeParameters ->
    -- |Updates to the time parameters in ascending order of slot time
    [(Slot, TimeParameters)] ->
    TimeParameters
timeParametersAtSlot targetSlot tp0 upds =
    fromMaybe tp0 $
        getLast $
            mconcat
                [Last (Just tp) | (slot, tp) <- upds, slot <= targetSlot]

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- This implementation is used for protocol version P4 and later.
-- The assumption is that there are no blocks between the block and the future slot; i.e. this
-- is used to determine the lottery participants that will try to bake a block with the block as the
-- parent.
--
-- If the slot is in the same payday as the given block, use the current epoch bakers.
-- If the slot is in the next payday, and the given block is in the last epoch of the prior payday,
-- use the next epoch bakers.
-- If the slot is in the more distant future, then the bakers for that slot are calculated based on
-- the active bakers and accounting for cooldowns.
-- (If the slot is in the past, the current epoch bakers are returned, but the function should
-- never be called for a historical slot.)
--
-- Note that it is very important that getSlotBakers should return the same set of bakers as will
-- be determined in the execution of the block. This is because it must be possible to reward the
-- baker of the block, which is not possible if the baker is not considered to be a baker during
-- the block's execution.
getSlotBakersP4 ::
    forall m.
    ( BlockStateQuery m,
      PVSupportsDelegation (MPV m),
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1
    ) =>
    GenesisConfiguration ->
    BlockState m ->
    Slot ->
    m FullBakers
getSlotBakersP4 genData bs slot = do
    SeedState{epochLength, epoch = blockEpoch} <- getSeedState bs
    let epochToSlot :: Epoch -> Slot
        epochToSlot e = fromIntegral e * epochLength
    nextPayday <- getPaydayEpoch bs
    let nextPaydaySlot = epochToSlot nextPayday

    if slot < nextPaydaySlot
        then getCurrentEpochBakers bs
        else do
            chainParams <- _currentParameters <$> getUpdates bs
            let blockTimeParameters = chainParams ^. cpTimeParameters
            pendingTimeParameters <-
                fmap (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                    <$> getPendingTimeParameters bs
            let paydayTimeParameters =
                    timeParametersAtSlot
                        nextPaydaySlot
                        (unOParam blockTimeParameters)
                        pendingTimeParameters
            let nextPaydayLength =
                    paydayTimeParameters
                        ^. tpRewardPeriodLength
                            . to (epochToSlot . rewardPeriodEpochs)
            if blockEpoch + 1 == nextPayday && slot < nextPaydaySlot + nextPaydayLength
                then getNextEpochBakers bs
                else do
                    -- The slot time is at least the next payday
                    -- First we compute the epoch of last payday that is no later than the given slot.
                    let latestPayday =
                            paydayEpochBefore
                                (unOParam blockTimeParameters)
                                pendingTimeParameters
                                (gdEpochLength genData)
                                nextPayday
                                slot

                    -- From this we can determine which cooldowns have elapsed.
                    let isEffective = effectiveTest' genData latestPayday

                    -- Determine the bakers and delegators for the next reward period, accounting for any
                    -- stake reductions that are currently pending on active bakers with effective time at
                    -- or before the next payday.
                    (activeBakers, passiveDelegators) <-
                        applyPendingChanges isEffective
                            <$> getActiveBakersAndDelegators bs
                    -- Determine the pool parameters that would be effective the epoch before the payday
                    pendingPoolParams <- getPendingPoolParameters bs
                    let latestPaydayTime = epochTimestamp genData latestPayday
                        epochDuration = gdSlotDuration genData * fromIntegral (gdEpochLength genData)
                        ePoolParams _ ((et, pp') : updates)
                            | addDuration (transactionTimeToTimestamp et) epochDuration <= latestPaydayTime =
                                ePoolParams pp' updates
                        ePoolParams pp _ = pp
                        effectivePoolParameters = ePoolParams (chainParams ^. cpPoolParameters) pendingPoolParams
                        bsc = computeBakerStakesAndCapital @m effectivePoolParameters activeBakers passiveDelegators
                    let mkFullBaker (biRef, _bakerStake) = do
                            _theBakerInfo <- derefBakerInfo biRef
                            return FullBakerInfo{..}
                    fullBakerInfos <- mapM mkFullBaker (Vec.fromList $ bakerStakes bsc)
                    let bakerTotalStake = sum $ _bakerStake <$> fullBakerInfos
                    return FullBakers{..}

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- The assumption is that there are no blocks between the block and the future slot; i.e. this
-- is used to determine the lottery participants that will try to bake a block with the block as the
-- parent.
--
-- The given slot should never be earlier than the slot of the given block.
getSlotBakers ::
    forall m.
    (IsProtocolVersion (MPV m), BlockStateQuery m) =>
    GenesisConfiguration ->
    BlockState m ->
    Slot ->
    m FullBakers
getSlotBakers genData = case protocolVersion @(MPV m) of
    SP1 -> getSlotBakersP1
    SP2 -> getSlotBakersP1
    SP3 -> getSlotBakersP1
    SP4 -> getSlotBakersP4 genData
    SP5 -> getSlotBakersP4 genData
    SP6 -> undefined -- FIXME

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- This will return 'Nothing' if the projected bakers could change before then (depending on
-- additional blocks), but will return the actual bakers if it is certain they will be correct.
-- This implementation is used for protocol versions P1-P3.
--
-- The given slot should never be earlier than the slot of the given block.
--
-- In P1, the bakers are fixed for the current epoch and the next epoch.
-- If the slot is in an epoch further in the future, this returns 'Nothing'.
-- (If the slot is in the past, the current epoch bakers will be returned, but the function should
-- not be called with a historical slot.)
getDefiniteSlotBakersP1 ::
    forall m.
    ( BlockStateQuery m,
      AccountVersionFor (MPV m) ~ 'AccountV0
    ) =>
    BlockState m ->
    Slot ->
    m (Maybe FullBakers)
getDefiniteSlotBakersP1 bs slot = do
    SeedState{..} <- getSeedState bs
    let slotEpoch = fromIntegral $ slot `quot` epochLength
    if slotEpoch <= epoch + 1
        then Just <$> getSlotBakersP1 bs slot
        else return Nothing

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- This will return 'Nothing' if the projected bakers could change before then (depending on
-- additional blocks), but will return the actual bakers if it is certain they will be correct.
-- This implementation is used for protocol version P4 and later.
--
-- The given slot should never be earlier than the slot of the given block.
--
-- If the slot is in the same payday as the given block, use the current epoch bakers.
-- If the slot is in the next payday, and the given block is in the last epoch of the prior payday,
-- use the next epoch bakers.
-- If the slot is further in the future, return 'Nothing'.
-- (If the slot is in the past, the current epoch bakers are returned, but the function should
-- never be called for a historical slot.)
getDefiniteSlotBakersP4 ::
    forall m.
    ( BlockStateQuery m,
      PVSupportsDelegation (MPV m),
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1
    ) =>
    GenesisConfiguration ->
    BlockState m ->
    Slot ->
    m (Maybe FullBakers)
getDefiniteSlotBakersP4 genData bs slot = do
    SeedState{epochLength, epoch = blockEpoch} <- getSeedState bs
    let epochToSlot :: Epoch -> Slot
        epochToSlot e = fromIntegral e * epochLength
    nextPayday <- getPaydayEpoch bs
    let nextPaydaySlot = epochToSlot nextPayday

    if slot < nextPaydaySlot
        then Just <$> getCurrentEpochBakers bs
        else do
            chainParams <- _currentParameters <$> getUpdates bs
            let blockTimeParameters = chainParams ^. cpTimeParameters
            pendingTimeParameters <-
                fmap (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                    <$> getPendingTimeParameters bs
            let paydayTimeParameters =
                    timeParametersAtSlot
                        nextPaydaySlot
                        (unOParam blockTimeParameters)
                        pendingTimeParameters
            let nextPaydayLength =
                    paydayTimeParameters
                        ^. tpRewardPeriodLength
                            . to (epochToSlot . rewardPeriodEpochs)
            if blockEpoch + 1 == nextPayday && slot < nextPaydaySlot + nextPaydayLength
                then Just <$> getNextEpochBakers bs
                else return Nothing

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- This will return 'Nothing' if the projected bakers could change before then (depending on
-- additional blocks), but will return the actual bakers if it is certain they will be correct.
--
-- The given slot should never be earlier than the slot of the given block.
getDefiniteSlotBakers ::
    forall m.
    (IsProtocolVersion (MPV m), BlockStateQuery m) =>
    GenesisConfiguration ->
    BlockState m ->
    Slot ->
    m (Maybe FullBakers)
getDefiniteSlotBakers genData = case protocolVersion @(MPV m) of
    SP1 -> getDefiniteSlotBakersP1
    SP2 -> getDefiniteSlotBakersP1
    SP3 -> getDefiniteSlotBakersP1
    SP4 -> getDefiniteSlotBakersP4 genData
    SP5 -> getDefiniteSlotBakersP4 genData
    SP6 -> undefined -- FIXME
