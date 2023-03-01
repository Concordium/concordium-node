{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test that the init context of a contract is passed correctly by the scheduler.
module SchedulerTests.InitContextTest (tests) where

import Data.Serialize
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.ProtocolVersion

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.Wasm

import Concordium.Scheduler.DummyData

import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 1000000000 0]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

senderAccount :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> Types.AccountAddress
senderAccount spv
    | demoteProtocolVersion spv >= P3 = Types.createAlias accountAddress0 17
    | otherwise = accountAddress0

transactionInputs :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> [Runner.TransactionJSON]
transactionInputs spv =
    [ Runner.TJSON
        { metadata = makeDummyHeader (senderAccount spv) 1 100000,
          payload = Runner.DeployModule V0 "../concordium-base/smart-contracts/testdata/contracts/chain-meta-test.wasm",
          keys = [(0, [(0, keyPair0)])]
        },
      Runner.TJSON
        { metadata = makeDummyHeader (senderAccount spv) 2 100000,
          payload = Runner.InitContract 9 V0 "../concordium-base/smart-contracts/testdata/contracts/chain-meta-test.wasm" "init_origin" "",
          keys = [(0, [(0, keyPair0)])]
        }
    ]

testInit :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> String -> Spec
testInit spv pvString = specify
    (pvString ++ ": Passing init context to contract")
    $ do
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                (transactionInputs spv)
        let Sch.FilteredTransactions{..} = Helpers.srTransactions result
        assertEqual "There should be no failed transactions." [] ftFailed
        assertEqual "There should be no rejected transactions." []
            $ filter
                ( \case
                    (_, Types.TxSuccess{}) -> False
                    (_, Types.TxReject{}) -> True
                )
            $ Helpers.getResults ftAdded
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        hashedState <- BS.hashBlockState state
        doInvariantAssertions <- Helpers.assertBlockStateInvariants hashedState (Helpers.srExecutionCosts result)
        instances <- BS.getContractInstanceList hashedState
        maybeInstance <- BS.bsoGetInstance state (Types.ContractAddress 0 0)
        return $ do
            doInvariantAssertions
            assertEqual "There should be 1 instance." 1 (length instances)

            case maybeInstance of
                Nothing -> assertFailure "Instance at <0,0> does not exist."
                Just (BS.InstanceInfoV0 ii) -> do
                    let Instances.InstanceStateV0 model = BS.iiState ii
                    assertEqual
                        "Instance model is the sender address of the account which initialized it."
                        model
                        (ContractState $ encode $ senderAccount spv)
                Just _ -> assertFailure "Expected V0 instance."

tests :: Spec
tests =
    describe "Init context in transactions." $
        sequence_ $
            Helpers.forEveryProtocolVersion testInit
