{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the upgrading feature which was implemented as part of P5.
--    In particular it tests that an initialized instance which has been deployed
--    in P5 can upgrade its underlying module i.e. the artifact via the host function
--    'upgrade'
module SchedulerTests.SmartContracts.V1.Upgrading (tests) where

import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.List as List
import Data.Serialize (Serialize (put), putWord32le, runPut)
import qualified Data.Set as Set

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers
import System.IO.Unsafe

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

-- |A module that is used as a base for upgrading.
upgrading0SourceFile :: FilePath
upgrading0SourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading_0.wasm"

-- |A v1 module with a matching contract name of 'upgrading0SourceFile'
-- so it should always be possible to upgrade to this from a contract based on
-- the former mentioned module.
upgrading1SourceFile :: FilePath
upgrading1SourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading_1.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion1 :: WasmVersion
wasmModVersion1 = V1

-- |The simple case, upgrading is intended to succeed, changes the module, and
-- changes the set of entrypoints.
upgradingTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
upgradingTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading 1") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ -- Deploy `upgrading_0.wasm
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule wasmModVersion1 upgrading0SourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 upgrading0SourceFile result
            },
          -- Deploy upgrading_1.wasm
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule wasmModVersion1 upgrading1SourceFile,
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 upgrading1SourceFile result
            },
          -- Initialize upgrading_0.wasm
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 wasmModVersion1 upgrading0SourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        upgrading0SourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          -- Invoke the `upgrade` by calling 'a.upgrade' with the resulting 'ModuleRef' of
          -- deploying upgrade_1.wasm.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 instanceAddr "a.bump" parameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 3 events from upgrading.
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 4) result
            },
          -- Invoke `new` which is only accessible after the module upgrade
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 instanceAddr "a.newfun" BSS.empty,
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doAssertState <- Helpers.checkReloadCheck (const assertFinalState) result state
                -- Check the number of events:
                -- - 1 event for a succesful update to the contract.
                return $ do
                    Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
                    doAssertState
            }
        ]
    lastModuleRef = getModuleRefFromV1File upgrading1SourceFile
    parameters = BSS.toShort $ runPut $ do
        -- The 'ModuleRef' to the desired module to upgrade to.
        put lastModuleRef

    instanceAddr = Types.ContractAddress 0 0

    assertFinalState :: BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    assertFinalState blockState = do
        maybeInstance <- BS.bsoGetInstance blockState instanceAddr
        return $ case maybeInstance of
            Nothing -> assertFailure "Instance does not exist."
            Just (BS.InstanceInfoV0 _) -> assertFailure "Instance should be a V1 instance."
            Just (BS.InstanceInfoV1 ii) -> do
                let curModuleRef =
                        GSWasm.moduleReference $
                            Types.instanceModuleInterface $
                                BS.iiParameters ii
                assertEqual "New module reference" lastModuleRef curModuleRef
                assertBool
                    "Instance has new entrypoint."
                    ( Set.member
                        (ReceiveName "a.newfun")
                        (Types.instanceReceiveFuns $ BS.iiParameters ii)
                    )
                assertBool
                    "Instance does not have the old entrypoint."
                    ( Set.notMember
                        (ReceiveName "a.bump")
                        (Types.instanceReceiveFuns $ BS.iiParameters ii)
                    )

selfInvokeSourceFile0 :: FilePath
selfInvokeSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-self-invoke0.wasm"

selfInvokeSourceFile1 :: FilePath
selfInvokeSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-self-invoke1.wasm"

-- | The contract in this test triggers an upgrade and then in the same invocation calls a
-- function in the upgraded module. Checking the new module is being used.
selfInvokeTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
selfInvokeTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading self invoke") $
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
                    { payload = DeployModule V1 selfInvokeSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 selfInvokeSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 selfInvokeSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        selfInvokeSourceFile0
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 selfInvokeSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 selfInvokeSourceFile1 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload =
                        Update
                            0
                            (Types.ContractAddress 0 0)
                            "contract.upgrade"
                            upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere eventsCheck result
            }
        ]
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File selfInvokeSourceFile1

    eventsCheck :: [Types.Event] -> Assertion
    eventsCheck events = do
        -- Check the number of events:
        -- - 3 events from invoking another contract ('contract.name').
        -- - 3 events from upgrading.
        -- - 3 events from invoking again  ('contract.name').
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 10 events

        -- Find and check the upgrade event
        case List.find isUpgradeEvent events of
            Nothing -> assertFailure "Missing event: Upgraded"
            Just Types.Upgraded{..} | euFrom == euTo -> assertFailure "Event Upgraded is incorrect"
            Just _ -> return ()
        -- Ensure only one upgrade event
        assertEqual "single Upgraded events" 1 (List.length (filter isUpgradeEvent events))

missingModuleSourceFile :: FilePath
missingModuleSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-missing-module.wasm"

-- |Upgrading to a missing module fails with the correct error code.
missingModuleTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
missingModuleTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading to a missing module fails") $
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
                    { payload = DeployModule V1 missingModuleSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 missingModuleSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 missingModuleSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        missingModuleSourceFile
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere eventsCheck result
            }
        ]
    eventsCheck :: [Types.Event] -> Assertion
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade'), 2 for interrupt
        Helpers.assertNumberOfEvents 3 events
        -- Find and check for upgrade events
        assertBool "Found unexpected event: Upgraded" (not $ List.any isUpgradeEvent events)

missingContractSourceFile0 :: FilePath
missingContractSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-missing-contract0.wasm"

missingContractSourceFile1 :: FilePath
missingContractSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-missing-contract1.wasm"

-- |Upgrading to a module which does not have the required contract fails with
-- the correct error code.
missingContractTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
missingContractTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading to a module without matching contract fails") $
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
                    { payload = DeployModule V1 missingContractSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 missingContractSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 missingContractSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        missingContractSourceFile0
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 missingContractSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 missingContractSourceFile1 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere eventsCheck result
            }
        ]
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File missingContractSourceFile1
    eventsCheck :: [Types.Event] -> Assertion
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade'), 2 for interrupt
        Helpers.assertNumberOfEvents 3 events
        -- Find and check for upgrade events
        assertBool "Found unexpected event: Upgraded" (not $ List.any isUpgradeEvent events)

unsupportedVersionSourceFile0 :: FilePath
unsupportedVersionSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-unsupported-version0.wasm"

unsupportedVersionSourceFile1 :: FilePath
unsupportedVersionSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-unsupported-version1.wasm"

-- |Attempt to upgrade to a V0 module. This should fail with a specific error
-- code.
unsupportedVersionTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
unsupportedVersionTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading to a module with an unsupported version") $
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
                    { payload = DeployModule V1 unsupportedVersionSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 unsupportedVersionSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 unsupportedVersionSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        unsupportedVersionSourceFile0
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V0 unsupportedVersionSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere eventsCheck result
            }
        ]
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV0File unsupportedVersionSourceFile1
    eventsCheck :: [Types.Event] -> Assertion
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade'),
        -- - 2 events for the interrupt
        Helpers.assertNumberOfEvents 3 events
        -- Find and check for upgrade events
        assertBool "Found unexpected event: Upgraded" (not $ List.any isUpgradeEvent events)

twiceSourceFile0 :: FilePath
twiceSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-twice0.wasm"

twiceSourceFile1 :: FilePath
twiceSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-twice1.wasm"

twiceSourceFile2 :: FilePath
twiceSourceFile2 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-twice2.wasm"

-- |Upgrading twice in the same transaction. The effect of the second upgrade
-- should be in effect at the end.
twiceTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
twiceTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading twice in the same invocation") $
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
                    { payload = DeployModule V1 twiceSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 twiceSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 twiceSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        upgrading0SourceFile
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 twiceSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 twiceSourceFile1 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 twiceSourceFile2,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 twiceSourceFile2 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 instanceAddr "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere eventsCheck result
            }
        ]
    lastModuleRef = getModuleRefFromV1File twiceSourceFile2
    upgradeParameters = BSS.toShort $ runPut $ do
        put $ getModuleRefFromV1File twiceSourceFile1
        put lastModuleRef
    instanceAddr = Types.ContractAddress 0 0
    eventsCheck :: [Types.Event] -> Assertion
    eventsCheck events = do
        -- Check the number of events:
        -- - 3 events for invoking self.
        -- - 3 events for upgrading.
        -- - 3 events for invoking again.
        -- - 3 events for upgrading again.
        -- - 3 events for invoking again.
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 16 events
        -- Find and check for upgrade events
        assertEqual "Expected two Upgraded events" (List.length (filter isUpgradeEvent events)) 2

chainedSourceFile0 :: FilePath
chainedSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-chained0.wasm"

-- | The contract in this test exposes an 'upgrade' entrypoint, triggering an upgrade to the same
-- module and then invokes 'upgrade' again until a counter (provided as the parameter) is 0.
-- This is to trigger a large number of upgrade events in the same transaction.
chainedTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
chainedTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading chained") $
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
                    { payload = DeployModule V1 chainedSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 chainedSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 chainedSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        chainedSourceFile0
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere eventsCheck result
            }
        ]
    chainLength = 100
    upgradeParameters = BSS.toShort $ runPut $ do
        putWord32le $ fromIntegral chainLength
        put $ getModuleRefFromV1File chainedSourceFile0
    eventsCheck :: [Types.Event] -> Assertion
    eventsCheck events = do
        -- Check the number of events:
        -- - chainLength x 3 events for upgrading and 3 events for invoking.
        -- - 3 events for upgrading.
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents (6 * chainLength + 4) events
        -- Find and check for upgrade events
        let upgradedEvents = List.length (filter isUpgradeEvent events)
        assertEqual
            ("Unexpected number of Upgraded events: " ++ show upgradedEvents)
            (chainLength + 1)
            upgradedEvents

rejectSourceFile0 :: FilePath
rejectSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-reject0.wasm"

rejectSourceFile1 :: FilePath
rejectSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-reject1.wasm"

-- |Tests whether a contract which triggers a succesful upgrade, but rejects the transaction from
-- another cause, rollbacks the upgrade as well.
rejectTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
rejectTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Upgrading reject") $
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
                    { payload = DeployModule V1 rejectSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 rejectSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 rejectSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        upgrading0SourceFile
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 rejectSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 rejectSourceFile1 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWhere rejectReasonCheck result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.new_feature" "",
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWhere assertInvalidReceiveMethod result
            }
        ]
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File rejectSourceFile1

    rejectReasonCheck :: Types.RejectReason -> Assertion
    rejectReasonCheck reason =
        case reason of
            Types.RejectedReceive{..} ->
                assertEqual "Unexpected receive reject reason " (-1) rejectReason
            other -> assertFailure $ "Unexpected reject reason " ++ show other

changingEntrypointsSourceFile0 :: FilePath
changingEntrypointsSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-changing-entrypoints0.wasm"

changingEntrypointsSourceFile1 :: FilePath
changingEntrypointsSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-changing-entrypoints1.wasm"

-- | Tests calling an entrypoint introduced by an upgrade of the module can be called and whether an
-- entrypoint removed by an upgrade fail with the appropriate reject reason.
changingEntrypointsTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
changingEntrypointsTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Check added and removed entrypoints of a contract") $
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
                    { payload = DeployModule V1 changingEntrypointsSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 changingEntrypointsSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 changingEntrypointsSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        changingEntrypointsSourceFile0
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 changingEntrypointsSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 changingEntrypointsSourceFile1 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.old_feature" "",
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere contractOldFeatureEventsCheck result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.new_feature" "",
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWhere assertInvalidReceiveMethod result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 6 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere contractUpgradeEventsCheck result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.new_feature" "",
                      metadata = makeDummyHeader accountAddress0 7 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere contractNewFeatureEventsCheck result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.old_feature" "",
                      metadata = makeDummyHeader accountAddress0 8 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWhere assertInvalidReceiveMethod result
            }
        ]
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File changingEntrypointsSourceFile1

    contractOldFeatureEventsCheck :: [Types.Event] -> Expectation
    contractOldFeatureEventsCheck =
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 1

    contractUpgradeEventsCheck :: [Types.Event] -> Expectation
    contractUpgradeEventsCheck events = do
        -- Check the number of events:
        -- - 3 events for upgrading.
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 4 events
        -- Find and check for upgrade events
        let upgradedEvents = List.length (filter isUpgradeEvent events)
        assertEqual "Unexpected number of Upgraded events" 1 upgradedEvents

    contractNewFeatureEventsCheck :: [Types.Event] -> Expectation
    contractNewFeatureEventsCheck =
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 1

persistingStateSourceFile0 :: FilePath
persistingStateSourceFile0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-persisting-state0.wasm"

persistingStateSourceFile1 :: FilePath
persistingStateSourceFile1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-persisting-state1.wasm"

-- | Tests wether state changes, during an invocation triggering an upgrade, persists when using the
-- upgraded instance.
persistingStateTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
persistingStateTestCase spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Check writing to state before and after calling the upgrade contract") $
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
                    { payload = DeployModule V1 persistingStateSourceFile0,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 persistingStateSourceFile0 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 persistingStateSourceFile0 "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        persistingStateSourceFile0
                        (InitName "init_contract")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 persistingStateSourceFile1,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 persistingStateSourceFile1 result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere contractUpgradeEventsCheck result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.check" "",
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere contractCheckEventsCheck result
            }
        ]
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File persistingStateSourceFile1
    contractUpgradeEventsCheck :: [Types.Event] -> Expectation
    contractUpgradeEventsCheck events = do
        -- Check the number of events:
        -- - 3 events for upgrading.
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 4 events
        -- Find and check for upgrade events
        let upgradedEvents = List.length (filter isUpgradeEvent events)
        assertEqual "Unexpected number of Upgraded events" 1 upgradedEvents
    contractCheckEventsCheck :: [Types.Event] -> Expectation
    contractCheckEventsCheck =
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        Helpers.assertNumberOfEvents 1

-- | Check if some event is the Upgraded event.
isUpgradeEvent :: Types.Event -> Bool
isUpgradeEvent event = case event of
    Types.Upgraded{} -> True
    _ -> False

-- |Get a 'ModuleRef' from a given V1 'Module' specified via the 'FilePath'.
getModuleRefFromV1File :: FilePath -> Types.ModuleRef
getModuleRefFromV1File f =
    unsafePerformIO $
        getModuleRef @V1 . WasmModuleV . ModuleSource <$> BS.readFile f

-- |Get a 'ModuleRef' from a given V1 'Module' specified via the 'FilePath'.
getModuleRefFromV0File :: FilePath -> Types.ModuleRef
getModuleRefFromV0File f =
    unsafePerformIO $
        getModuleRef @V0 . WasmModuleV . ModuleSource <$> BS.readFile f

-- | Assert the reject reason is invalid receive method.
assertInvalidReceiveMethod :: Types.RejectReason -> Assertion
assertInvalidReceiveMethod reason =
    case reason of
        Types.InvalidReceiveMethod _ _ -> return ()
        other -> assertFailure $ "Unexpected reject reason" ++ show other

tests :: Spec
tests =
    describe "V1: Upgrade" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                upgradingTestCase spv pvString
                selfInvokeTestCase spv pvString
                missingModuleTestCase spv pvString
                missingContractTestCase spv pvString
                unsupportedVersionTestCase spv pvString
                twiceTestCase spv pvString
                chainedTestCase spv pvString
                rejectTestCase spv pvString
                changingEntrypointsTestCase spv pvString
                persistingStateTestCase spv pvString
