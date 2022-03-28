{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |Functionality for handling baker changes based on epoch boundaries.
module Concordium.Kontrol.Bakers where

import qualified Data.Vector as Vec
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Accounts

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState
import Concordium.Types.SeedState

-- |Caps on the the stake that may be delegated to a baking pool.
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
    PoolParameters 'ChainParametersV1 ->
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
    PoolCaps{..} = delegatedCapitalCaps poolParams totalCap bakerCap delCap

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
    -- adding its delegators to the L-pool.
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
        _ -> (baker{activeBakerDelegators = pDelegators} : bakers, lpool)
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
      -- |Determine the capital distribution.
      capitalDistributionM :: m CapitalDistribution
    }

-- |Compute the baker stakes and capital distribution.
computeBakerStakesAndCapital ::
    forall m.
    (AccountOperations m) =>
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
        let lPoolCapital = Vec.fromList $ delegatorCapital <$> lpoolDelegators
        return CapitalDistribution{..}

-- |Generate and set the next epoch bakers and next capital based on the current active bakers.
generateNextBakers ::
    ( TreeStateMonad m,
      AccountVersionFor (MPV m) ~ 'AccountV1,
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
    (activeBakers, lpoolDelegators) <-
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
                lpoolDelegators
    bs1 <- bsoSetNextEpochBakers bs0 bakerStakes
    capDist <- capitalDistributionM
    bsoSetNextCapitalDistribution bs1 capDist

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- This implementation is used for protocol version P4 and later.
-- The assumption is that there are no blocks between the block and the future slot; i.e. this
-- is used to determine the lottery participants that will try to bake a block with the block as the
-- parent.
--
-- If the slot is in the same epoch as the given block, use the current epoch bakers.
-- If the slot is in a later epoch, use the next epoch bakers.
-- (This should not be called with a slot earlier than the block's slot. However, if it is,
-- it will return the current epoch bakers.)
--
-- Note, the next epoch bakers are updated in the first block in the epoch before a payday.
-- (Or, if there is no block in that epoch, the first block later than the epoch before the payday.)
-- This means that if an entire epoch is skipped, the computed bakers may not be the same as if
-- a block had existed in that epoch.  This state of affairs is not particularly bad, as it just
-- means that potential changes affecting the bakers (such as stake changes) are not accounted for
-- in the first block after at least one epoch has elapsed with no blocks (already an unlikely
-- circumstance).
getSlotBakersP4 ::
    forall m.
    ( BlockStateQuery m
    ) =>
    -- |State of parent block
    BlockState m ->
    -- |Slot to compute bakers for
    Slot ->
    m FullBakers
getSlotBakersP4 bs slot = do
    SeedState{epochLength, epoch = oldEpoch} <- getSeedState bs
    let slotEpoch = fromIntegral $ slot `quot` epochLength
    if slotEpoch <= oldEpoch
        then getCurrentEpochBakers bs
        else getNextEpochBakers bs

-- |Determine the bakers that apply to a future slot, given the state at a particular block.
-- The assumption is that there are no blocks between the block and the future slot; i.e. this
-- is used to determine the lottery participants that will try to bake a block with the block as the
-- parent. (Therefore, it is expected that the given slot will be later than the
-- slot of the given block.)
--
-- Prior to P4, if this function was called on an epoch beyond the next one, the bakers would be
-- derived from the active bakers under, with cooldowns applied. However, from P4 the next epoch
-- bakers will be returned instead. (This change in the protocol was made because it is even more
-- complex to compute the bakers, but it does not add security.)
getSlotBakers ::
    forall m.
    (MonadProtocolVersion m, BlockStateQuery m) =>
    BlockState m ->
    Slot ->
    m FullBakers
getSlotBakers = case protocolVersion @(MPV m) of
    SP1 -> getSlotBakersP1
    SP2 -> getSlotBakersP1
    SP3 -> getSlotBakersP1
    SP4 -> getSlotBakersP4
