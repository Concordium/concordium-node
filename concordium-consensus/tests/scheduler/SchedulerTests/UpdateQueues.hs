{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SchedulerTests.UpdateQueues (tests) where

import Control.Monad.RWS.Strict as RWS hiding (state)
import Lens.Micro.Platform
import Test.Hspec

import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as PU
import Concordium.Types

import qualified SchedulerTests.Helpers as Helpers
import Test.HUnit (assertEqual)

-- This is a regression test for https://github.com/Concordium/concordium-node/issues/972
-- In protocol version 1-5 the chain parameter update effects were sometimes lost
-- if they were effective at the same time. This is fixed in protocol 6.
testCase :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> String -> IO ()
testCase spv pvString = do
    -- Schedule three updates
    let rootKeyUpdate = UVRootKeys dummyHigherLevelKeys
    let poolParameterUpdate = UVPoolParameters (dummyChainParameters @(ChainParametersVersionFor pv) ^. cpPoolParameters)
    let euroEnergyExchange = UVEuroPerEnergy (_erEuroPerEnergy (dummyChainParameters @(ChainParametersVersionFor pv) ^. cpExchangeRates))
    -- The first two are scheduled at effectiveTime = 123
    -- The last one is schedule for a millisecond earlier.
    let effectiveTime = 123 :: TransactionTime
    effects <- liftIO . Helpers.runTestBlockState $ do
        u1 <- refMake =<< PU.initialUpdates (withIsAuthorizationsVersionForPV spv dummyKeyCollection) dummyChainParameters
        enqueuedState <-
            PU.enqueueUpdate effectiveTime poolParameterUpdate
                =<< PU.enqueueUpdate (effectiveTime - 1) euroEnergyExchange
                =<< PU.enqueueUpdate effectiveTime rootKeyUpdate u1
        ars <- refMake dummyArs
        ips <- refMake dummyIdentityProviders
        fst <$> PU.processUpdateQueues (protocolVersion @pv) (transactionTimeToTimestamp effectiveTime) (enqueuedState, ars, ips)
    -- In protocol version <= 5 the pool parameter update is not returned, since it occurs
    -- at the same time as the root keys update.
    if demoteProtocolVersion spv <= P5
        then
            assertEqual
                (pvString ++ ": Only the root key update is returned at effectiveTime")
                [ (effectiveTime - 1, euroEnergyExchange),
                  (effectiveTime, rootKeyUpdate)
                ]
                effects
        else -- In P6 and up all updates are returned.

            assertEqual
                (pvString ++ ": All updates should be returned")
                [ (effectiveTime - 1, euroEnergyExchange),
                  (effectiveTime, rootKeyUpdate),
                  (effectiveTime, poolParameterUpdate)
                ]
                effects

tests :: Spec
tests = do
    describe "Scheduler.UpdateQueues" $ do
        specify "Correct effects are returned" $
            sequence_ $
                Helpers.forEveryProtocolVersion testCase
