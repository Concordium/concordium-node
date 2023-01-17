{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchedulerTests.InitialAccountCreationSpec where

import Data.Maybe
import Test.HUnit
import Test.Hspec

import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Types as Types

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.TransactionVerification

import Concordium.Scheduler.DummyData
import Concordium.Types.DummyData

import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "Account creation" $
        sequence_ $
            Helpers.forEveryProtocolVersion testAccountCreation

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState = Helpers.createTestBlockStateWithAccountsM []

-- We just put a dummy verification result such that it will be verified by the scheduler.
transactionsInput :: [CredentialDeploymentWithStatus]
transactionsInput =
    map
        ((\x -> (x, Nothing)) . Types.addMetadata Types.CredentialDeployment 0)
        [ icdi1,
          icdi2,
          icdi3 -- should fail because reuse of prf key
        ]

testAccountCreation ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testAccountCreation _ pvString = specify
    (pvString ++ ": 2 accounts created, next two rejected.")
    $ do
        let transactions = Types.TGCredentialDeployment <$> transactionsInput
        (_, doAssertions) <-
            Helpers.runSchedulerTest
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck makeAssertions)
                transactions
        doAssertions
  where
    makeAssertions ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    makeAssertions result state = do
        doInvariantAssertions <-
            Helpers.assertBlockStateInvariantsH
                state
                (Helpers.srExecutionCosts result)

        let addedAccountAddresses =
                map
                    (accountAddressFromCredential . Types.credential)
                    [icdi1, icdi2] -- cdi3 has the same address as cdi2
        lookups <- mapM (BS.bsoGetAccount state) addedAccountAddresses

        return $ do
            let Sch.FilteredTransactions{..} = Helpers.srTransactions result
            assertEqual "Execution cost should be 0." 0 (Helpers.srExecutionCosts result)
            assertEqual "The third transaction should fail." 1 (length ftFailedCredentials)

            case Helpers.getResults ftAdded of
                [(_, a11), (_, a12)]
                    | Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a11,
                      Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a12 ->
                        return ()
                _ -> assertFailure "Accounts failed to be created"

            doInvariantAssertions
            assertBool "Newly created accounts." $ all isJust lookups
