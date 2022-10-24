{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module tests the upgrading feature which was implemented as part of P5.
    In particular it tests that an initialized instance which has been deployed
    in P5 can upgrade its underlying module i.e. the artifact via the host function
    'upgrade'
-}
module SchedulerTests.SmartContracts.V1.Upgrading (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, assertBool)

import Lens.Micro.Platform
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.Set as Set
import Data.Serialize(runPut, Serialize (put), putWord32le)
import qualified Data.Text as T
import qualified Data.List as List

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.Wasm
import qualified Concordium.Cost as Cost

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils
import System.IO.Unsafe


initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

-- |A module that is used as a base for upgrading.
upgrading0SourceFile :: FilePath
upgrading0SourceFile = "./testdata/contracts/v1/upgrading_0.wasm"

-- |A v1 module with a matching contract name of 'upgrading0SourceFile'
-- so it should always be possible to upgrade to this from a contract based on
-- the former mentioned module.
upgrading1SourceFile :: FilePath
upgrading1SourceFile = "./testdata/contracts/v1/upgrading_1.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion1 :: WasmVersion
wasmModVersion1 = V1

-- |The simple case, upgrading is intended to succeed, changes the module, and
-- changes the set of entrypoints.
upgradingTestCase :: TestCase PV5
upgradingTestCase =
    TestCase
    { tcName = "Upgrading 1"
    , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ -- Deploy `upgrading_0.wasm
        ( TJSON { payload = DeployModule wasmModVersion1 upgrading0SourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck upgrading0SourceFile), emptySpec)
        )
      , -- Deploy upgrading_1.wasm
        ( TJSON { payload = DeployModule wasmModVersion1 upgrading1SourceFile
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck upgrading1SourceFile), emptySpec)
        )
        -- Initialize upgrading_0.wasm
      , ( TJSON { payload = InitContract 0 wasmModVersion1 upgrading0SourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck upgrading0SourceFile "init_a"), emptySpec)
        )
      ,
        -- Invoke the `upgrade` by calling 'a.upgrade' with the resulting 'ModuleRef' of
        -- deploying upgrade_1.wasm.
        ( TJSON { payload = Update 0 instanceAddr "a.bump" parameters
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (successWithEventsCheck bumpEventsCheck), emptySpec)
        )
      ,
        -- Invoke `new` which is only accessible after the module upgrade
        ( TJSON { payload = Update 0 instanceAddr "a.newfun" BSS.empty
                , metadata = makeDummyHeader alesAccount 5 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (successWithEventsCheck newFunEventsCheck), finalStateSpec)
        )
      ]
    }
  where
    lastModuleRef = getModuleRefFromV1File upgrading1SourceFile
    parameters = BSS.toShort $ runPut $ do
        -- The 'ModuleRef' to the desired module to upgrade to.
        put lastModuleRef

    instanceAddr = Types.ContractAddress 0 0
    bumpEventsCheck :: [Types.Event] -> Expectation
    bumpEventsCheck events = do
      -- Check the number of events:
      -- - 3 events from upgrading.
      -- - 1 event for a succesful update to the contract.
      eventsLengthCheck 4 events

    newFunEventsCheck :: [Types.Event] -> Expectation
    newFunEventsCheck events = do
      -- Check the number of events:
      -- - 1 event for a succesful update to the contract.
      eventsLengthCheck 1 events

    finalStateSpec bs = specify "The new instance has correct module and entrypoint." $ 
      case bs ^? blockInstances . ix instanceAddr of
        Nothing -> assertFailure "Instance does not exist."
        Just (Types.InstanceV0 _) -> assertFailure "Instance should be a V1 instance."
        Just (Types.InstanceV1 Types.InstanceV{..}) -> do
          let curModuleRef = GSWasm.moduleReference . Types.instanceModuleInterface $ _instanceVParameters
          assertEqual "New module reference" lastModuleRef curModuleRef
          assertBool "Instance has new entrypoint." (Set.member (ReceiveName "a.newfun") (Types.instanceReceiveFuns _instanceVParameters))
          assertBool "Instance does not have the old entrypoint." (Set.notMember (ReceiveName "a.bump") (Types.instanceReceiveFuns _instanceVParameters))


selfInvokeSourceFile0 :: FilePath
selfInvokeSourceFile0 = "./testdata/contracts/v1/upgrading-self-invoke0.wasm"

selfInvokeSourceFile1 :: FilePath
selfInvokeSourceFile1 = "./testdata/contracts/v1/upgrading-self-invoke1.wasm"

-- | The contract in this test, triggers an upgrade and then in the same invocation, calls a function in the upgraded module.
-- Checking the new module is being used.
selfInvokeTestCase :: TestCase PV5
selfInvokeTestCase =
  TestCase { tcName = "Upgrading self invoke"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 selfInvokeSourceFile0
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck selfInvokeSourceFile0), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 selfInvokeSourceFile0 "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck selfInvokeSourceFile0 "init_contract"), emptySpec))
             , ( TJSON { payload = DeployModule V1 selfInvokeSourceFile1
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck selfInvokeSourceFile1), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File selfInvokeSourceFile1

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 3 events from invoking another contract ('contract.name').
        -- - 3 events from upgrading.
        -- - 3 events from invoking again  ('contract.name').
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 10 events

        -- Find and check the upgrade event
        case List.find isUpgradeEvent events of
          Nothing -> assertFailure "Missing event: Upgraded"
          Just Types.Upgraded{..} | euFrom == euTo -> assertFailure "Event Upgraded is incorrect"
          Just _ -> return ()
        -- Ensure only one upgrade event
        unless (List.length (filter isUpgradeEvent events) == 1) $ assertFailure "Multiple events: Upgraded"

missingModuleSourceFile :: FilePath
missingModuleSourceFile = "./testdata/contracts/v1/upgrading-missing-module.wasm"

-- |Upgrading to a missing module fails with the correct error code.
missingModuleTestCase :: TestCase PV5
missingModuleTestCase = TestCase { tcName = "Upgrading to a missing module fails"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule V1 missingModuleSourceFile
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck missingModuleSourceFile), emptySpec))
    , ( TJSON { payload = InitContract 0 V1 missingModuleSourceFile "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck missingModuleSourceFile "init_contract"), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" ""
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
    ]
  }
  where
    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 1 events
        -- Find and check for upgrade events
        when (List.any isUpgradeEvent events) $ assertFailure "Found unexpected event: Upgraded"

missingContractSourceFile0 :: FilePath
missingContractSourceFile0 = "./testdata/contracts/v1/upgrading-missing-contract0.wasm"

missingContractSourceFile1 :: FilePath
missingContractSourceFile1 = "./testdata/contracts/v1/upgrading-missing-contract1.wasm"

-- |Upgrading to a module which does not have the required contract fails with
-- the correct error code.
missingContractTestCase :: TestCase PV5
missingContractTestCase =
  TestCase { tcName = "Upgrading to a module without matching contract fails"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule V1 missingContractSourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck missingContractSourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 V1 missingContractSourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck missingContractSourceFile0 "init_contract"), emptySpec))
    , ( TJSON { payload = DeployModule V1 missingContractSourceFile1
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck missingContractSourceFile1), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
              , metadata = makeDummyHeader alesAccount 4 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
    ]
  }
  where
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File missingContractSourceFile1

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 1 events
        -- Find and check for upgrade events
        when (List.any isUpgradeEvent events) $ assertFailure "Found unexpected event: Upgraded"

unsupportedVersionSourceFile0 :: FilePath
unsupportedVersionSourceFile0 = "./testdata/contracts/v1/upgrading-unsupported-version0.wasm"

unsupportedVersionSourceFile1 :: FilePath
unsupportedVersionSourceFile1 = "./testdata/contracts/v1/upgrading-unsupported-version1.wasm"

-- |Attempt to upgrade to a V0 module. This should fail with a specific error
-- code.
unsupportedVersionTestCase :: TestCase PV5
unsupportedVersionTestCase =
  TestCase { tcName = "Upgrading to a module with an unsupported version"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule V1 unsupportedVersionSourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck unsupportedVersionSourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 V1 unsupportedVersionSourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck unsupportedVersionSourceFile0 "init_contract"), emptySpec))
    , ( TJSON { payload = DeployModule V0 unsupportedVersionSourceFile1
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck unsupportedVersionSourceFile1), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
              , metadata = makeDummyHeader alesAccount 4 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
    ]
  }
  where
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV0File unsupportedVersionSourceFile1

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 1 events
        -- Find and check for upgrade events
        when (List.any isUpgradeEvent events) $ assertFailure "Found unexpected event: Upgraded"

twiceSourceFile0 :: FilePath
twiceSourceFile0 = "./testdata/contracts/v1/upgrading-twice0.wasm"

twiceSourceFile1 :: FilePath
twiceSourceFile1 = "./testdata/contracts/v1/upgrading-twice1.wasm"

twiceSourceFile2 :: FilePath
twiceSourceFile2 = "./testdata/contracts/v1/upgrading-twice2.wasm"

-- |Upgrading twice in the same transaction. The effect of the second upgrade
-- should be in effect at the end.
twiceTestCase :: TestCase PV5
twiceTestCase =
  TestCase { tcName = "Upgrading twice in the same invocation"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule V1 twiceSourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck twiceSourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 V1 twiceSourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck twiceSourceFile0 "init_contract"), emptySpec))
    , ( TJSON { payload = DeployModule V1 twiceSourceFile1
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck twiceSourceFile1), emptySpec))
    , ( TJSON { payload = DeployModule V1 twiceSourceFile2
              , metadata = makeDummyHeader alesAccount 4 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck twiceSourceFile2), emptySpec))

    , ( TJSON { payload = Update 0 instanceAddr "contract.upgrade" upgradeParameters
              , metadata = makeDummyHeader alesAccount 5 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (successWithEventsCheck eventsCheck), finalStateSpec))
    ]
  }
  where
    lastModuleRef = getModuleRefFromV1File twiceSourceFile2

    upgradeParameters = BSS.toShort $ runPut $ do
      put $ getModuleRefFromV1File twiceSourceFile1
      put lastModuleRef

    instanceAddr = Types.ContractAddress 0 0

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 3 events for invoking self.
        -- - 3 events for upgrading.
        -- - 3 events for invoking again.
        -- - 3 events for upgrading again.
        -- - 3 events for invoking again.
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 16 events
        -- Find and check for upgrade events
        unless (List.length (filter isUpgradeEvent events) == 2) $ assertFailure "Expected two Upgraded events"

    finalStateSpec bs = specify "The new instance has correct module." $ 
      case bs ^? blockInstances . ix instanceAddr of
        Nothing -> assertFailure "Instance does not exist."
        Just (Types.InstanceV0 _) -> assertFailure "Instance should be a V1 instance."
        Just (Types.InstanceV1 Types.InstanceV{..}) -> do
          let curModuleRef = GSWasm.moduleReference . Types.instanceModuleInterface $ _instanceVParameters
          assertEqual "New module reference" lastModuleRef curModuleRef

chainedSourceFile0 :: FilePath
chainedSourceFile0 = "./testdata/contracts/v1/upgrading-chained0.wasm"

-- | The contract in this test exposes an 'upgrade' entrypoint, triggering an upgrade to the same module and then invokes 'upgrade' again until a counter (provided as the parameter) is 0.
-- This is to trigger a large number of upgrade events in the same transaction.
chainedTestCase :: TestCase PV5
chainedTestCase =
  TestCase { tcName = "Upgrading chained"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule V1 chainedSourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck chainedSourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 V1 chainedSourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck chainedSourceFile0 "init_contract"), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
    ]
  }
  where
    chainLength = 100

    upgradeParameters = BSS.toShort $ runPut $ do
      putWord32le $ fromIntegral chainLength
      put $ getModuleRefFromV1File chainedSourceFile0

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - chainLength x 3 events for upgrading and 3 events for invoking.
        -- - 3 events for upgrading.
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck (6 * chainLength + 4) events
        -- Find and check for upgrade events
        let upgradedEvents = List.length (filter isUpgradeEvent events)
        unless (upgradedEvents == chainLength + 1) $ assertFailure $ "Unexpected number of Upgraded events: " ++ show upgradedEvents


rejectSourceFile0 :: FilePath
rejectSourceFile0 = "./testdata/contracts/v1/upgrading-reject0.wasm"

rejectSourceFile1 :: FilePath
rejectSourceFile1 = "./testdata/contracts/v1/upgrading-reject1.wasm"

-- | Tests whether a contract which triggers a succesful upgrade, but rejects the transaction from another cause, rollbacks the upgrade as well.
rejectTestCase :: TestCase PV5
rejectTestCase =
  TestCase { tcName = "Upgrading reject"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 rejectSourceFile0
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck rejectSourceFile0), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 rejectSourceFile0 "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck rejectSourceFile0 "init_contract"), emptySpec))
             , ( TJSON { payload = DeployModule V1 rejectSourceFile1
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck rejectSourceFile1), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (rejectWithReasonCheck rejectReasonCheck), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.new_feature" ""
                       , metadata = makeDummyHeader alesAccount 5 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary rejectInvalidReceiveMethodCheck, emptySpec))
             ]
           }
  where
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File rejectSourceFile1

    rejectReasonCheck :: Types.RejectReason -> Expectation
    rejectReasonCheck reason =
      case reason of
          Types.RejectedReceive {..} ->
            unless (rejectReason == -1) $ assertFailure $ "Unexpected receive reject reason " ++ show rejectReason
          other -> assertFailure $ "Unexpected reject reason " ++ show other

changingEntrypointsSourceFile0 :: FilePath
changingEntrypointsSourceFile0 = "./testdata/contracts/v1/upgrading-changing-entrypoints0.wasm"

changingEntrypointsSourceFile1 :: FilePath
changingEntrypointsSourceFile1 = "./testdata/contracts/v1/upgrading-changing-entrypoints1.wasm"

-- | Tests calling an entrypoint introduced by an upgrade of the module can be called and whether an entrypoint removed by an upgrade fail with the appropriate reject reason.
changingEntrypointsTestCase :: TestCase PV5
changingEntrypointsTestCase =
  TestCase { tcName = "Check added and removed entrypoints of a contract"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 changingEntrypointsSourceFile0
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck changingEntrypointsSourceFile0), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 changingEntrypointsSourceFile0 "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck changingEntrypointsSourceFile0 "init_contract"), emptySpec))
             , ( TJSON { payload = DeployModule V1 changingEntrypointsSourceFile1
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck changingEntrypointsSourceFile1), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.old_feature" ""
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck contractOldFeatureEventsCheck), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.new_feature" ""
                       , metadata = makeDummyHeader alesAccount 5 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary rejectInvalidReceiveMethodCheck, emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
                       , metadata = makeDummyHeader alesAccount 6 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck contractUpgradeEventsCheck), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.new_feature" ""
                       , metadata = makeDummyHeader alesAccount 7 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck contractNewFeatureEventsCheck), emptySpec))

               , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.old_feature" ""
                       , metadata = makeDummyHeader alesAccount 8 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary rejectInvalidReceiveMethodCheck, emptySpec))
             ]
           }
  where
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File changingEntrypointsSourceFile1

    contractOldFeatureEventsCheck :: [Types.Event] -> Expectation
    contractOldFeatureEventsCheck events =
      -- Check the number of events:
      -- - 1 event for a succesful update to the contract ('contract.upgrade').
      eventsLengthCheck 1 events

    contractUpgradeEventsCheck :: [Types.Event] -> Expectation
    contractUpgradeEventsCheck events = do
      -- Check the number of events:
      -- - 3 events for upgrading.
      -- - 1 event for a succesful update to the contract ('contract.upgrade').
      eventsLengthCheck 4 events
      -- Find and check for upgrade events
      let upgradedEvents = List.length (filter isUpgradeEvent events)
      unless (upgradedEvents == 1) $
        assertFailure $ "Unexpected number of Upgraded events: " ++ show upgradedEvents

    contractNewFeatureEventsCheck :: [Types.Event] -> Expectation
    contractNewFeatureEventsCheck =
      -- Check the number of events:
      -- - 1 event for a succesful update to the contract ('contract.upgrade').
      eventsLengthCheck 1

persistingStateSourceFile0 :: FilePath
persistingStateSourceFile0 = "./testdata/contracts/v1/upgrading-persisting-state0.wasm"

persistingStateSourceFile1 :: FilePath
persistingStateSourceFile1 = "./testdata/contracts/v1/upgrading-persisting-state1.wasm"

-- | Tests wether state changes, during an invocation triggering an upgrade, persists when using the upgraded instance.
persistingStateTestCase :: TestCase PV5
persistingStateTestCase =
  TestCase { tcName = "Check writing to state before and after calling the upgrade contract"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 persistingStateSourceFile0
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck persistingStateSourceFile0), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 persistingStateSourceFile0 "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck persistingStateSourceFile0 "init_contract"), emptySpec))
             , ( TJSON { payload = DeployModule V1 persistingStateSourceFile1
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck persistingStateSourceFile1), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck contractUpgradeEventsCheck), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.check" ""
                       , metadata = makeDummyHeader alesAccount 5 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck contractCheckEventsCheck), emptySpec))
             ]
           }
  where
    upgradeParameters = BSS.toShort $ runPut $ put $ getModuleRefFromV1File persistingStateSourceFile1

    contractUpgradeEventsCheck :: [Types.Event] -> Expectation
    contractUpgradeEventsCheck events = do
      -- Check the number of events:
      -- - 3 events for upgrading.
      -- - 1 event for a succesful update to the contract ('contract.upgrade').
      eventsLengthCheck 4 events
      -- Find and check for upgrade events
      let upgradedEvents = List.length (filter isUpgradeEvent events)
      unless (upgradedEvents == 1) $
        assertFailure $ "Unexpected number of Upgraded events: " ++ show upgradedEvents

    contractCheckEventsCheck :: [Types.Event] -> Expectation
    contractCheckEventsCheck =
      -- Check the number of events:
      -- - 1 event for a succesful update to the contract ('contract.upgrade').
      eventsLengthCheck 1



-- | Check if some event is the Upgraded event.
isUpgradeEvent :: Types.Event -> Bool
isUpgradeEvent event = case event of
  Types.Upgraded{} -> True
  _ -> False

-- |Get a 'ModuleRef' from a given V1 'Module' specified via the 'FilePath'.
getModuleRefFromV1File :: FilePath -> Types.ModuleRef
getModuleRefFromV1File f = unsafePerformIO $ do
        getModuleRef @V1 . WasmModuleV . ModuleSource <$> BS.readFile f

-- |Get a 'ModuleRef' from a given V1 'Module' specified via the 'FilePath'.
getModuleRefFromV0File :: FilePath -> Types.ModuleRef
getModuleRefFromV0File f = unsafePerformIO $ do
        getModuleRef @V0 . WasmModuleV . ModuleSource <$> BS.readFile f

-- This only checks that the cost of initialization is correct.
-- If the state was not set up correctly the latter tests in the suite will fail.
initializationCostCheck :: FilePath -> T.Text -> (TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation)
initializationCostCheck sourceFile initName _ Types.TransactionSummary{..} = do
  checkSuccess "Contract initialization failed: " tsResult
  moduleSource <- BS.readFile sourceFile
  let modLen = fromIntegral $ BS.length moduleSource
      modRef = Types.ModuleRef (Hash.hash moduleSource)
      payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName initName) (Parameter "")))
      -- size of the transaction minus the signatures.
      txSize = Types.transactionHeaderSize + fromIntegral payloadSize
      -- transaction is signed with 1 signature
      baseTxCost = Cost.baseCost txSize 1
      -- lower bound on the cost of the transaction, assuming no interpreter energy
      -- The state size of A is 0 and larger for B. We put the lower bound at A's size.
      costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 0)

  unless (tsEnergyCost >= costLowerBound) $
    assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound
  where
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess _ _ = return ()

deploymentCostCheck :: FilePath -> (TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation)
deploymentCostCheck sourceFile _ Types.TransactionSummary{..} = do
  checkSuccess "Module deployment failed: " tsResult
  moduleSource <- BS.readFile sourceFile
  let len = fromIntegral $ BS.length moduleSource
      -- size of the module deploy payload
      payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModuleV1 (WasmModuleV ModuleSource{..}))))
      -- size of the transaction minus the signatures.
      txSize = Types.transactionHeaderSize + fromIntegral payloadSize
      -- transaction is signed with 1 signature
  assertEqual "Deployment has correct cost " (Cost.baseCost txSize 1 + Cost.deployModuleCost len) tsEnergyCost
  where
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess _ _ = return ()

-- | Check the transaction failed because of invalid receive method.
rejectInvalidReceiveMethodCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
rejectInvalidReceiveMethodCheck _ summary = case Types.tsResult summary of
      Types.TxReject {..} ->
        case vrRejectReason of
          Types.InvalidReceiveMethod _ _ -> return ()
          other -> assertFailure $ "Unexpected reject reason" ++ show other
      Types.TxSuccess {} -> assertFailure "Update should reject with InvalidReceiveMethod"

-- | Check the transaction succeeded, taking a function to check the events.
successWithEventsCheck :: ([Types.Event] -> Expectation) -> TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
successWithEventsCheck checkEvents _ summary = case Types.tsResult summary of
  Types.TxReject {..} -> assertFailure $ "Transaction rejected unexpectedly with " ++ show vrRejectReason
  Types.TxSuccess {..} -> checkEvents vrEvents

-- | Check the transaction rejected, taking a function to check the reason.
rejectWithReasonCheck :: (Types.RejectReason -> Expectation) -> TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
rejectWithReasonCheck checkReason _ summary = case Types.tsResult summary of
  Types.TxReject {..} -> checkReason vrRejectReason
  Types.TxSuccess {} -> assertFailure "Transaction succeeded unexpectedly"

-- | Check the number of events is as expected.
eventsLengthCheck :: Int -> [Types.Event] -> Expectation
eventsLengthCheck expected events = unless (length events == expected) $
  assertFailure $ "Unexpected number of events produced: " ++ show (length events) ++ " where the expected was " ++ show expected


tests :: Spec
tests = describe "V1: Upgrade" $ mkSpecs [
  upgradingTestCase
  , selfInvokeTestCase
  , missingModuleTestCase
  , missingContractTestCase
  , unsupportedVersionTestCase
  , twiceTestCase
  , chainedTestCase
  , rejectTestCase
  , changingEntrypointsTestCase
  , persistingStateTestCase
  ]

