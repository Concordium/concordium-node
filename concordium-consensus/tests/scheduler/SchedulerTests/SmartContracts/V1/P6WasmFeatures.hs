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

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion1 :: WasmVersion
wasmModVersion1 = V1

-- |Test the different modules for whether they can be deployed or not.
testCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    FilePath ->
    Spec
testCase spv pvString sourceFile =
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
              taaAssertion = \result _ ->
                return $
                    if Types.demoteProtocolVersion spv <= Types.P5 then
                       Helpers.assertSuccess result
                    else 
                       Helpers.assertRejectWithReason Types.ModuleNotWF result
            }
        ]

tests :: Spec
tests =
    describe "V1: P6 features" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                testCase spv (pvString ++ ": Globals in data sections") modDataGlobals
                testCase spv (pvString ++ ": Globals in element sections") modElemGlobals
