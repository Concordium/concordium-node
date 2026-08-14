{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for max-lock-duration chain updates.
module SchedulerTests.MaxLockDurationUpdate (tests) where

import qualified SchedulerTests.Helpers as Helpers
import Test.Hspec

import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Types as Types

-- | Test P11 scheduler handling of max-lock-duration chain updates.
tests :: Spec
tests = describe "MaxLockDurationUpdate" $ do
    specify "P11 max lock duration chain updates are accepted and enqueued" $ do
        Helpers.runSchedulerTestAssertIntermediateStates
            @'Types.P11
            Helpers.defaultTestConfig
            initialBlockState
            [ Helpers.BlockItemAndAssertion
                { biaaTransaction = maxLockDurationUpdate,
                  biaaAssertion = \result _ ->
                    return $
                        Helpers.assertSuccessWithEvents
                            [UpdateEnqueued effectiveTime payload]
                            result
                }
            ]
  where
    initialBlockState = Helpers.createTestBlockStateWithAccounts @'Types.P11 ([] :: [BS.PersistentAccount (Types.AccountVersionFor 'Types.P11)])
    effectiveTime = 123456789
    timeout = 123456788
    payload = MaxLockDurationUpdatePayload (Duration 123456)
    maxLockDurationUpdate =
        Runner.ChainUpdateTx $
            Runner.ChainUpdateTransaction
                { ctSeqNumber = 1,
                  ctEffectiveTime = effectiveTime,
                  ctTimeout = timeout,
                  ctPayload = payload,
                  ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                }
