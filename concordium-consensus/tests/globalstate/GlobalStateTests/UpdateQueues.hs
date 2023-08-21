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
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as PU
import Concordium.Types

-- This tests that chain parameter updates that are scheduled at the same time are not lost
-- when calling 'PU.processUpdateQueues'.
testCase :: forall cpv. IsChainParametersVersion cpv => SChainParametersVersion cpv -> String -> IO ()
testCase scpv pvString = do
    -- Schedule three updates
    let rootKeyUpdate = UVRootKeys dummyHigherLevelKeys
    let poolParameterUpdate = UVPoolParameters (dummyChainParameters @cpv ^. cpPoolParameters)
    let euroEnergyExchange = UVEuroPerEnergy (_erEuroPerEnergy (dummyChainParameters @cpv ^. cpExchangeRates))
    -- The first two are scheduled at effectiveTime = 123
    -- The last one is schedule for a millisecond earlier.
    let effectiveTime = 123 :: TransactionTime
    effects <- liftIO . runBlobStoreTemp "." $ do
        (u1 :: BufferedRef (PU.Updates' cpv)) <-
            refMake
                =<< PU.initialUpdates (withIsAuthorizationsVersionFor scpv dummyKeyCollection) dummyChainParameters
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

tests :: Spec
tests = do
    describe "Scheduler.UpdateQueues" $ do
        specify "Correct effects are returned" $ do
            testCase SChainParametersV0 "CPV0"
            testCase SChainParametersV1 "CPV1"
            testCase SChainParametersV2 "CPV2"
