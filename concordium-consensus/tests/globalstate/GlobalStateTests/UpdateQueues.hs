{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GlobalStateTests.UpdateQueues (tests) where

import Control.Monad.RWS.Strict as RWS hiding (state)
import Lens.Micro.Platform
import Test.HUnit (assertEqual)
import Test.Hspec

import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.Parameters as PCP
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as PU
import Concordium.Types

-- This tests that chain parameter updates that are scheduled at the same time are not lost
-- when calling 'PU.processUpdateQueues'.
testCase :: forall cpv auv. (IsChainParametersVersion cpv, IsAuthorizationsVersion auv) => SChainParametersVersion cpv -> SAuthorizationsVersion auv -> String -> IO ()
testCase _ _ pvString = do
    -- Schedule three updates
    let rootKeyUpdate = UVRootKeys dummyHigherLevelKeys
    let poolParameterUpdate = UVPoolParameters (dummyChainParameters' @cpv ^. cpPoolParameters)
    let euroEnergyExchange = UVEuroPerEnergy (_erEuroPerEnergy (dummyChainParameters' @cpv ^. cpExchangeRates))
    -- The first two are scheduled at effectiveTime = 123
    -- The last one is schedule for a millisecond earlier.
    let effectiveTime = 123 :: TransactionTime
    effects <- liftIO . runBlobStoreTemp "." $ do
        (u1 :: BufferedRef (PU.Updates' cpv auv)) <-
            refMake
                =<< PU.initialUpdates (dummyKeyCollection @auv) (dummyChainParameters' @cpv)
        enqueuedState <-
            PU.enqueueUpdate effectiveTime poolParameterUpdate
                =<< PU.enqueueUpdate (effectiveTime - 1) euroEnergyExchange
                =<< PU.enqueueUpdate effectiveTime rootKeyUpdate u1
        ars <- refMake dummyArs
        ips <- refMake dummyIdentityProviders
        fst <$> PU.processUpdateQueues (transactionTimeToTimestamp effectiveTime) (enqueuedState, ars, ips)
    assertEqual
        (pvString ++ ": All updates should be returned")
        [ (effectiveTime - 1, euroEnergyExchange),
          (effectiveTime, rootKeyUpdate),
          (effectiveTime, poolParameterUpdate)
        ]
        effects

testMaxLockDurationUpdate :: IO ()
testMaxLockDurationUpdate = do
    let effectiveTime = 123 :: TransactionTime
        newDuration = Duration 123456
        update = UVMaxLockDuration newDuration
    (effects, maxLockDuration) <- liftIO . runBlobStoreTemp "." $ do
        (u0 :: BufferedRef (PU.Updates' 'ChainParametersV3 'AuthorizationsVersion3)) <-
            refMake
                =<< PU.initialUpdates
                    (dummyKeyCollection @'AuthorizationsVersion3)
                    (dummyChainParameters @'P11)
        u1 <- PU.enqueueUpdate effectiveTime update u0
        ars <- refMake dummyArs
        ips <- refMake dummyIdentityProviders
        (processedEffects, (u2, _, _)) <- PU.processUpdateQueues (transactionTimeToTimestamp effectiveTime) (u1, ars, ips)
        updatedParametersRef <- PU.currentParameters <$> refLoad u2
        updatedParameters <- PCP.persistentChainParametersToChainParametersM =<< refLoad updatedParametersRef
        return (processedEffects, updatedParameters ^. cpMaxLockDuration)
    assertEqual
        "The max lock duration update should be returned"
        [(effectiveTime, update)]
        effects
    assertEqual
        "The public chain-parameter view should expose the updated max lock duration"
        (SomeParam (Just newDuration))
        maxLockDuration

tests :: Spec
tests = do
    describe "Scheduler.UpdateQueues" $ do
        specify "Correct effects are returned" $ do
            testCase SChainParametersV0 SAuthorizationsVersion0 "CPV0"
            testCase SChainParametersV1 SAuthorizationsVersion1 "CPV1"
            testCase SChainParametersV2 SAuthorizationsVersion1 "CPV2"
        specify "Effective max lock duration updates mutate external chain parameters" testMaxLockDurationUpdate
