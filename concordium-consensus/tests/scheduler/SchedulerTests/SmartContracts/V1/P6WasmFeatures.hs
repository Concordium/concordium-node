{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests that new P6 additions are correctly allowed in P6 and
-- later, and not allowed in earlier protocol versions.
module SchedulerTests.SmartContracts.V1.P6WasmFeatures (tests) where

import Test.Hspec

import Control.Monad

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 100_000_000 0]

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

-- |A module with globals in initialization sections.
modDataGlobals :: FilePath
modDataGlobals = "../concordium-base/smart-contracts/testdata/contracts/global-data-section-test.wasm"

-- |A module with globals in initialization sections.
modElemGlobals :: FilePath
modElemGlobals = "../concordium-base/smart-contracts/testdata/contracts/global-element-section-test.wasm"

-- |Modules testing whether sign extension instructions are allowed or not.
modi32extend8s :: FilePath
modi32extend8s = "../concordium-base/smart-contracts/testdata/contracts/v1/i32.extend8_s.wasm"

modi32extend16s :: FilePath
modi32extend16s = "../concordium-base/smart-contracts/testdata/contracts/v1/i32.extend16_s.wasm"
modi64extend8s :: FilePath
modi64extend8s = "../concordium-base/smart-contracts/testdata/contracts/v1/i64.extend8_s.wasm"
modi64extend16s :: FilePath
modi64extend16s = "../concordium-base/smart-contracts/testdata/contracts/v1/i64.extend16_s.wasm"
modi64extend32s :: FilePath
modi64extend32s = "../concordium-base/smart-contracts/testdata/contracts/v1/i64.extend32_s.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion1 :: WasmVersion
wasmModVersion1 = V1

-- |Test the different modules for whether they can be deployed or not.
testCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    -- |Assertion that will be used to check the transaction outcome.
    Helpers.TransactionAssertion pv ->
    -- |Description of the test.
    String ->
    -- |Path to the module to attempt to deploy.
    FilePath ->
    Spec
testCase spv taaAssertion pvString sourceFile =
    -- we only check V1 contracts, which do not exist before P4.
    when (Types.demoteProtocolVersion spv >= Types.P4) $
        specify (pvString ++ ": Wasm features") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule wasmModVersion1 sourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              ..
            }
        ]

-- |Ensure that the outcome is success before P6, and ModuleNotWF after P6.
validBeforeP6 :: forall pv. Types.IsProtocolVersion pv => Helpers.TransactionAssertion pv
validBeforeP6 result _ =
    return $
        if Types.supportsGlobalsInInitSections (Types.protocolVersion @pv)
            then Helpers.assertSuccess result
            else Helpers.assertRejectWithReason Types.ModuleNotWF result

-- |Ensure that the outcome is success after P6, and ModuleNotWF before P6.
validAfterP6 :: forall pv. Types.IsProtocolVersion pv => Helpers.TransactionAssertion pv
validAfterP6 result _ =
    return $
        if Types.supportsSignExtensionInstructions (Types.protocolVersion @pv)
            then Helpers.assertSuccess result
            else Helpers.assertRejectWithReason Types.ModuleNotWF result

tests :: Spec
tests =
    describe "V1: P6 features" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                testCase spv validBeforeP6 (pvString ++ ": Globals in data sections") modDataGlobals
                testCase spv validBeforeP6 (pvString ++ ": Globals in element sections") modElemGlobals
                testCase spv validAfterP6 (pvString ++ ": i32.extend8_s") modi32extend8s
                testCase spv validAfterP6 (pvString ++ ": i32.extend16_s") modi32extend16s
                testCase spv validAfterP6 (pvString ++ ": i64.extend8_s") modi64extend8s
                testCase spv validAfterP6 (pvString ++ ": i64.extend16_s") modi64extend16s
                testCase spv validAfterP6 (pvString ++ ": i64.extend32_s") modi64extend32s
