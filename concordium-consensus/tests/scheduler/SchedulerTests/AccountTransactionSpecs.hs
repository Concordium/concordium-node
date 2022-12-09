{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SchedulerTests.AccountTransactionSpecs where

import Data.Maybe
import Test.HUnit
import Test.Hspec

import qualified Concordium.ID.Types as Types
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import qualified Concordium.Scheduler.Types as Types

import Concordium.TransactionVerification
import Concordium.Types

import Concordium.GlobalState.Basic.BlockState.Account as Basic
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS

import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import Concordium.Types.DummyData

import qualified SchedulerTests.Helpers as Helpers

initialAmount :: Types.Amount
initialAmount = 0

accountA :: (IsAccountVersion av) => Basic.Account av
accountA = mkAccount alesVK alesAccount initialAmount

initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState = Helpers.createTestBlockStateWithAccounts [accountA]

-- cdi7, but with lowest possible expiry
cdi7' :: Types.AccountCreation
cdi7' =
    Types.AccountCreation
        { messageExpiry = 0,
          credential = Types.credential cdi7
        }

transactionsInput :: [CredentialDeploymentWithStatus]
transactionsInput =
    map ((\x -> (x, Nothing)) . Types.addMetadata Types.CredentialDeployment 0) $
        [ cdi1,
          cdi2,
          cdi3,
          cdi4, -- should fail because repeated credential ID
          cdi5,
          -- cdi6, -- deploy just a new predicate
          cdi7,
          cdi7'
        ]

accountTest ::
    forall pv av.
    (AccountVersionFor pv ~ av, IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    Assertion
accountTest _ = do
    let transactions = Types.TGCredentialDeployment <$> transactionsInput
    let contextState =
            Helpers.defaultContextState
                { EI._chainMetadata = dummyChainMeta{slotTime = 250}
                }
    let testConfig =
            Helpers.defaultTestConfig
                { Helpers.tcContextState = contextState
                }
    (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
        Helpers.runSchedulerTest
            @pv
            testConfig
            initialBlockState
            checkState
            transactions
    let Sch.FilteredTransactions{..} = srTransactions
    doBlockStateAssertions
    assertBool "Successful transaction results." $ case Helpers.getResults ftAdded of
        [(_, a11), (_, a12), (_, a13), (_, a15), (_, a17)]
            | Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a11,
              Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a12,
              Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a13,
              Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a15,
              Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a17 ->
                True
        _ -> False
    assertEqual
        "Fourth and eighth credential deployment should fail."
        [ Types.ExpiredTransaction,
          Types.DuplicateAccountRegistrationID (Types.credId . Types.credential $ cdi3)
        ]
        (map snd ftFailedCredentials)
    assertEqual "Execution cost should be 0." 0 srExecutionCosts
  where
    checkState :: BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    checkState state = do
        doAssertState <- blockStateAssertions state
        reloadedState <- Helpers.reloadBlockState state
        doAssertReloadedState <- blockStateAssertions reloadedState
        return $ do
            doAssertState
            doAssertReloadedState

    blockStateAssertions :: BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    blockStateAssertions state = do
        hashedState <- BS.hashBlockState state
        doInvariantAssertions <- Helpers.assertBlockStateInvariants hashedState 0
        let addedAccountAddresses =
                map
                    (accountAddressFromCredential . Types.credential)
                    [cdi1, cdi2, cdi3, cdi5, cdi7]
        lookups <- mapM (BS.bsoGetAccount state) addedAccountAddresses
        maybeAccount <- BS.bsoGetAccount state alesAccount
        maybeAmount <- mapM (BS.getAccountAmount . snd) maybeAccount
        return $ do
            doInvariantAssertions
            assertBool "Newly created accounts." $ all isJust lookups
            case maybeAmount of
                Nothing -> assertFailure "Account was created."
                Just accAmount ->
                    assertEqual "Account should keep the initial amount." initialAmount accAmount

tests :: Spec
tests =
    describe "Account creation" $ do
        sequence_ $ Helpers.forEveryProtocolVersion $ \spv pvString ->
            specify
                ( pvString
                    ++ ": 4 accounts created, fifth rejected, credential deployed, and one more account created."
                )
                $ accountTest spv
