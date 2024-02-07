{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests that make sure that the persistent state implementation
--  correctly handles the different cases of smart contract updates,
--  i.e., if the state, or module, or amount was updated
module SchedulerTests.SmartContracts.V1.TransfersPersistent (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize as S
import Data.Word

import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.Scheduler.Runner as SchedTest
import Concordium.Types.Execution
import qualified Concordium.Wasm as Wasm

import Concordium.Scheduler.DummyData

import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

-- The module which supports transfers and state updates.
testModuleSourceFile :: FilePath
testModuleSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/transfer-cases.wasm"

initialBlockState :: Helpers.PersistentBSM PV5 (HashedPersistentBlockState PV5)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 10_000_000 0]

-- Construct a basic upgrade test case.
-- Deploy two modules, initialize an instance from the module that supports an upgrade,
-- and then do the upgrade.
-- The Word8 should be 0 for no state changes, 1 for a state change **before** the upgrade,
-- and 2 for a state change **after** the upgrade
testCase :: Word8 -> [SchedTest.TransactionJSON]
testCase changeState =
    [ SchedTest.TJSON
        { payload = SchedTest.DeployModule Wasm.V1 testModuleSourceFile,
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 1 1_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      SchedTest.TJSON
        { payload = SchedTest.InitContract 1_000 Wasm.V1 testModuleSourceFile "init_contract" "",
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 2 1_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      SchedTest.TJSON
        { payload =
            SchedTest.Update
                0
                (Types.ContractAddress 0 0)
                "contract.transfer"
                upgradeParameters,
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 3 10_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        }
    ]
  where
    -- the upgrade parameters are the address to send to, the amount, and the tag stating whether the state should or should not be updated
    upgradeParameters =
        BSS.toShort $
            S.runPut $
                S.put (Helpers.accountAddressFromSeed 0)
                    <> S.putWord64le 123
                    <> S.putWord8 changeState

-- Run the transfer tests in different scenarios. The Word8 indicates how the state should be changed.
-- 0 for no change, 1 for change before transfer, 2 for change after transfer.
-- The boolean indicates whether to reload the state before querying the final value.
runTransferTests :: Word8 -> Bool -> Assertion
runTransferTests changeState reloadState = do
    (Helpers.SchedulerResult{..}, (bal, newState)) <-
        Helpers.runSchedulerTestTransactionJson
            Helpers.defaultTestConfig
            initialBlockState
            extractor
            (testCase changeState)
    let Sch.FilteredTransactions{..} = srTransactions
    forM_ (Helpers.getResults ftAdded) $ \(_, result) -> do
        case result of
            TxSuccess{} -> return ()
            TxReject{..} -> assertFailure $ "Transaction rejected: " ++ show vrRejectReason
    assertEqual "Amount was updated" (1_000 - 123) bal
    if changeState /= 0
        then assertEqual "State was updated" 1 (BS.index newState 0) -- non-empty state serialization starts with a 1 tag.
        else assertEqual "State was not updated" (BS.singleton 0) newState -- empty state serialization just puts a 0 tag.
  where
    extractor _ ubs = do
        blockState <-
            if reloadState
                then Helpers.reloadBlockState ubs
                else return ubs
        bsoGetInstance blockState (Types.ContractAddress 0 0) >>= \case
            Nothing -> error "Missing instance."
            Just (InstanceInfoV0 _) -> error "Expected V1 instance, but got V0."
            Just (InstanceInfoV1 ii) -> do
                let Instances.InstanceStateV1 s = iiState ii
                bs <- StateV1.toByteString s
                return (iiBalance ii, bs)

tests :: Spec
tests = describe "Upgrade contract cases with persistent state" $ do
    specify "V1: Just transfer" $ runTransferTests 0 False
    specify "V1: Transfer + state update before" $ runTransferTests 1 False
    specify "V1: Transfer + state update after" $ runTransferTests 2 False
    specify "V1: Reload: Just transfer" $ runTransferTests 0 True
    specify "V1: Reload: Transfer + state update before" $ runTransferTests 1 True
    specify "V1: Reload: Transfer + state update after" $ runTransferTests 2 True
