{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SchedulerTests.BlockEnergyLimitSpec where

import Test.HUnit
import Test.Hspec

import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types

import Concordium.Scheduler.DummyData

import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 200000 0]

-- We will use simple transfer transactions.
usedTransactionEnergy :: Types.Energy
usedTransactionEnergy = Helpers.simpleTransferCost

maxBlockEnergy :: Types.Energy
maxBlockEnergy = usedTransactionEnergy * 2

testConfig :: Helpers.TestConfig
testConfig =
    Helpers.defaultTestConfig
        { Helpers.tcContextState = contextState
        }
  where
    contextState =
        Helpers.defaultContextState
            { -- Set the max energy block energy to enough for 2 successful transfers.
              EI._maxBlockEnergy = maxBlockEnergy
            }

transactions :: [TransactionJSON]
transactions =
    [ -- valid transaction: energy not over max limit
      TJSON
        { payload = Transfer{toaddress = Helpers.accountAddressFromSeed 0, amount = 100},
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 1 usedTransactionEnergy,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      -- invalid transaction: although its used energy amount (plus the energy of the
      -- previously valid transaction) is under the energy limit,
      -- the stated energy is above the limit, so this transaction cannot be added to the block
      TJSON
        { payload = Transfer{toaddress = Helpers.accountAddressFromSeed 0, amount = 100},
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 3 $ maxBlockEnergy + 1,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      -- will be an unprocessed transaction because together with the first valid transaction,
      -- its energy exceeds the limit
      TJSON
        { payload = Transfer{toaddress = Helpers.accountAddressFromSeed 0, amount = 100},
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 4 $ usedTransactionEnergy + 1,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        }
    ]

testMaxBlockEnergy :: forall pv. (Types.IsProtocolVersion pv) => Types.SProtocolVersion pv -> Assertion
testMaxBlockEnergy _ = do
    ts' <- processUngroupedTransactions transactions
    -- invalid transaction: its used and stated energy of 10000 exceeds the maximum
    -- block energy limit
    let ts =
            Types.TGCredentialDeployment
                (Types.addMetadata Types.CredentialDeployment 0 cdi1, Nothing)
                : ts' -- dummy arrival time of 0
    (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
        Helpers.runSchedulerTest
            testConfig
            initialBlockState
            checkState
            ts
    let Sch.FilteredTransactions{..} = srTransactions

    (t1, t3, t4) <- case concat (Types.perAccountTransactions ts) of
        [t1, t3, t4] -> return (t1, t3, t4)
        _ -> assertFailure "There should be three filtered transactions."

    case ftAdded of
        [ ( t,
            Types.TransactionSummary
                { tsResult = Types.TxSuccess{vrEvents = [Types.Transferred{}]},
                  tsEnergyCost = energyCost
                }
            )
            ] -> do
                assertEqual
                    "The first transaction should be valid:"
                    (Types.normalTransaction $ fst t1)
                    $ fst t
                assertEqual "Correct energy cost: " Helpers.simpleTransferCost energyCost
        _ -> assertFailure "There should be one valid transaction with a TxSuccess result."

    let (invalidTs, failures) = unzip ftFailed
    let (invalidCreds, credFailures) = unzip ftFailedCredentials
    assertEqual
        "The second transaction is invalid because it would exceed max block energy:"
        [t3]
        invalidTs
    assertEqual
        "The credential deployment is invalid."
        [Types.addMetadata Types.CredentialDeployment 0 cdi1]
        $ map fst invalidCreds
    assertEqual
        "There is one normal transaction whose energy exceeds the block energy limit, and one with non-sequential nonce:"
        [Types.ExceedsMaxBlockEnergy]
        failures
    assertEqual
        "There is one credential deployment whose energy exceeds the block energy limit:"
        [Types.ExceedsMaxBlockEnergy]
        credFailures

    assertEqual
        "The last transaction does not fit into the block since the block has reached the energy limit"
        [t4]
        ftUnprocessed
    doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        doAssertState <- blockStateAssertions result state
        reloadedState <- Helpers.reloadBlockState state
        doAssertReloadedState <- blockStateAssertions result reloadedState
        return $ do
            doAssertState
            doAssertReloadedState

    blockStateAssertions ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    blockStateAssertions result state = do
        hashedState <- BS.hashBlockState state
        Helpers.assertBlockStateInvariants hashedState (Helpers.srExecutionCosts result)

tests :: Spec
tests =
    describe "Maximum block energy limit test:" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString ->
                specify (pvString ++ ": One valid, two invalid, one unprocessed transaction") $
                    testMaxBlockEnergy spv
