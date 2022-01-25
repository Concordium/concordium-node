{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- |Functionality for handling baker changes based on epoch boundaries.
module Concordium.Kontrol.Bakers where

import Control.Arrow
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Accounts

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState
import Concordium.Types.SeedState

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

-- |Generate and set the next epoch bakers and next capital based on the current active bakers.
-- TODO: Also account for pending changes.
generateNextBakers ::
    ( BlockStateOperations m,
      AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1
    ) =>
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
generateNextBakers bs0 = do
    (activeBakers, lpoolDelegators) <- bsoGetActiveBakersAndDelegators bs0
    cps <- bsoGetChainParameters bs0
    let leverage = cps ^. cpPoolParameters . ppLeverageBound
        capitalBound = cps ^. cpPoolParameters . ppCapitalBound
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
        lpoolCapital = (activeDelegatorId &&& activeDelegatorStake) <$> lpoolDelegators

    bs1 <- bsoSetNextEpochBakers bs0 bakerStakes
    bakerCapitals <- mapM bakerCapital activeBakers
    bsoSetNextCapitalDistribution bs1 bakerCapitals lpoolCapital

getSlotBakersV1 ::
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
            chainParams <- getChainParameters bs
            let paydayLength = chainParams ^. cpTimeParameters . tpRewardPeriodLength
            if blockEpoch + 1 == nextPayday && slotEpoch < nextPayday + paydayLength then
                getNextEpochBakers bs
            else do
                gd <- getGenesisData
                let genesisTime = gdGenesisTime gd
                    slotDuration = gdSlotDuration gd

                undefined
