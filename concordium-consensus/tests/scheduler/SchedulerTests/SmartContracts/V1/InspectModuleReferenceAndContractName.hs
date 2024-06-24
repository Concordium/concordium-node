{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the contract inspection functionality of the invoke host function for
-- getting the module reference and contract name of an instance.
module SchedulerTests.SmartContracts.V1.InspectModuleReferenceAndContractName where

import qualified Data.ByteString.Short as BSS
import Data.Serialize (Serialize (put), encode, putWord64le, runPut)
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm
import SchedulerTests.Helpers ()
import qualified SchedulerTests.Helpers as Helpers
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 100_000_000 0
        ]

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

-- | Source file for contracts that invoke the contract inspection queries.
srcQueriesContractInspection :: FilePath
srcQueriesContractInspection = "../concordium-base/smart-contracts/testdata/contracts/v1/queries-contract-inspection.wasm"

-- | Another smart contract module that is used for deploying a contract that has a different
--  module reference.
srcQueriesAccountBalance :: FilePath
srcQueriesAccountBalance = "../concordium-base/smart-contracts/testdata/contracts/v1/queries-account-balance.wasm"

-- | Compute the module reference of a module at a given source path.
modRefOf :: FilePath -> IO Types.ModuleRef
modRefOf modSrc = do
    modl <- Helpers.readV1ModuleFile modSrc
    return $! case modl of
        WasmModuleV0 m -> getModuleRef m
        WasmModuleV1 m -> getModuleRef m

-- | Module reference of 'srcQueriesContractInspection'.
modRefQueriesContractInspection :: Types.ModuleRef
{-# NOINLINE modRefQueriesContractInspection #-}
modRefQueriesContractInspection = unsafePerformIO $ modRefOf srcQueriesContractInspection

-- | Module reference of 'srcQueriesAccountBalance'.
modRefQueriesAccountBalance :: Types.ModuleRef
{-# NOINLINE modRefQueriesAccountBalance #-}
modRefQueriesAccountBalance = unsafePerformIO $ modRefOf srcQueriesAccountBalance

-- | If lower module loading costs are in effect for the given protocol version
--  return the second Energy argument, otherwise the first.
ifLowerModuleCost :: Types.SProtocolVersion pv -> Types.Energy -> Types.Energy -> Types.Energy
ifLowerModuleCost spv old new
    | Types.demoteProtocolVersion spv <= Types.P6 = old
    | otherwise = new

-- | This test deploys three different smart contracts, from two different modules.
--  One of these contracts <0,0> has a function that queries and logs the module reference of a
--  specified contract address. Another <2,0> queries and logs the contract name of a specified
--  contract. Both functions are invoked for each instance, and for non-existant contract
--  addresses, ensuring the values are as expected.
--
--  The entrypoints are designed to succeed in the case of a match, fail with code -1 if there is
--  a mismatch, and fail with code -2 if the contract address does not exist. If the protocol
--  version does not support the contract inspection functionality, then the call should fail with
--  a runtime exception.
--
--  As well as testing the result of the queries, this also tests that the costs of the operations
--  are as expected.
testModuleRefAndName :: forall pv. (Types.IsProtocolVersion pv) => Types.SProtocolVersion pv -> [Char] -> SpecWith ()
testModuleRefAndName spv pvString
    | Types.supportsV1Contracts spv =
        specify (pvString ++ ": inspect contract module reference and contract name") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
    | otherwise = return ()
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ deployModHelper 1 srcQueriesContractInspection,
          deployModHelper 2 srcQueriesAccountBalance,
          initContractHelper 3 srcQueriesContractInspection "init_contract",
          initContractHelper 4 srcQueriesAccountBalance "init_contract",
          initContractHelper 5 srcQueriesContractInspection "init_contract2",
          getModRefHelper 6 (Types.ContractAddress 0 0) (Just modRefQueriesContractInspection),
          getModRefHelper 7 (Types.ContractAddress 1 0) (Just modRefQueriesAccountBalance),
          getModRefHelper 8 (Types.ContractAddress 2 0) (Just modRefQueriesContractInspection),
          getModRefHelper 9 (Types.ContractAddress 3 0) Nothing,
          getModRefHelper 10 (Types.ContractAddress 0 1) Nothing,
          getNameHelper 11 (Types.ContractAddress 0 0) (Just "init_contract") (ifLowerModuleCost spv 754 745),
          getNameHelper 12 (Types.ContractAddress 1 0) (Just "init_contract") (ifLowerModuleCost spv 754 745),
          getNameHelper 13 (Types.ContractAddress 2 0) (Just "init_contract2") (ifLowerModuleCost spv 754 746),
          getNameHelper 14 (Types.ContractAddress 3 0) Nothing (ifLowerModuleCost spv 741 732),
          getNameHelper 15 (Types.ContractAddress 0 1) Nothing (ifLowerModuleCost spv 741 732)
        ]
    deployModHelper nce src =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 src,
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 src result
            }
    initContractHelper nce src constructor =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 src constructor "",
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        (Types.protocolVersion @pv)
                        src
                        (InitName constructor)
                        (Parameter mempty)
                        Nothing
                        result
            }
    getModRefHelper ::
        Types.Nonce ->
        Types.ContractAddress ->
        Maybe Types.ModuleRef ->
        Helpers.TransactionAndAssertion pv
    getModRefHelper nce scAddr mExpectModRef =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.get_module_reference" params,
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    if Types.supportsContractInspectionQueries spv
                        then case mExpectModRef of
                            Nothing -> do
                                Helpers.assertRejectWhere
                                    ( \case
                                        Types.RejectedReceive{rejectReason = -1} -> return ()
                                        _ -> assertFailure "Rejected for incorrect reason"
                                    )
                                    result
                                assertEqual
                                    "Energy usage (non-existing instance)"
                                    (ifLowerModuleCost spv 743 734)
                                    (Helpers.srUsedEnergy result)
                            Just modRef -> do
                                Helpers.assertSuccessWhere (checkEvents modRef) result
                                assertEqual
                                    "Energy usage (existing instance)"
                                    (ifLowerModuleCost spv 775 766)
                                    (Helpers.srUsedEnergy result)
                        else do
                            Helpers.assertRejectWithReason Types.RuntimeFailure result
                            assertEqual
                                "Energy usage (unsupported protocol version)"
                                543
                                (Helpers.srUsedEnergy result)
            }
      where
        params = case scAddr of
            (Types.ContractAddress i si) -> BSS.toShort $ runPut $ do
                putWord64le $ fromIntegral i
                putWord64le $ fromIntegral si
        checkEvents modRef [Types.Updated{euEvents = [ContractEvent ce]}] =
            assertEqual "Module reference" (BSS.toShort $ encode modRef) ce
        checkEvents _ _ = assertFailure "Expected exactly one event"
    getNameHelper ::
        Types.Nonce ->
        Types.ContractAddress ->
        Maybe BSS.ShortByteString ->
        Types.Energy ->
        Helpers.TransactionAndAssertion pv
    getNameHelper nce scAddr mExpectName expectEnergy =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 2 0) "contract2.get_contract_name" params,
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    if Types.supportsContractInspectionQueries spv
                        then do
                            case mExpectName of
                                Nothing -> do
                                    Helpers.assertRejectWhere
                                        ( \case
                                            Types.RejectedReceive{rejectReason = -1} -> return ()
                                            _ -> assertFailure "Rejected for incorrect reason"
                                        )
                                        result
                                Just name -> do
                                    Helpers.assertSuccessWhere (checkEvents name) result
                            assertEqual "Energy usage" expectEnergy (Helpers.srUsedEnergy result)
                        else do
                            Helpers.assertRejectWithReason Types.RuntimeFailure result
                            assertEqual "Energy usage" 541 (Helpers.srUsedEnergy result)
            }
      where
        params = case scAddr of
            (Types.ContractAddress i si) -> BSS.toShort $ runPut $ do
                putWord64le $ fromIntegral i
                putWord64le $ fromIntegral si
        checkEvents name [Types.Updated{euEvents = [ContractEvent ce]}] =
            assertEqual "Contract name" name ce
        checkEvents _ _ = assertFailure "Expected exactly one event"

-- | First source file for contract upgrade and query module reference interaction test.
srcUpgrade0 :: FilePath
srcUpgrade0 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-inspect-module0.wasm"

-- | Second source file for contract upgrade and query module reference interaction test.
srcUpgrade1 :: FilePath
srcUpgrade1 = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-inspect-module1.wasm"

-- | Module reference of 'srcUpgrade0'.
modUpgrade0 :: Types.ModuleRef
{-# NOINLINE modUpgrade0 #-}
modUpgrade0 = unsafePerformIO $ modRefOf srcUpgrade0

-- | Module reference of 'srcUpgrade1'.
modUpgrade1 :: Types.ModuleRef
{-# NOINLINE modUpgrade1 #-}
modUpgrade1 = unsafePerformIO $ modRefOf srcUpgrade1

testUpgradeModuleRef :: forall pv. (Types.IsProtocolVersion pv) => Types.SProtocolVersion pv -> [Char] -> SpecWith ()
testUpgradeModuleRef spv pvString
    | Types.supportsV1Contracts spv && Types.supportsUpgradableContracts spv =
        specify (pvString ++ ": upgrade contract and inspect module reference") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
    | otherwise = return ()
  where
    addr0 = Types.ContractAddress 0 0
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ deployModHelper 1 srcUpgrade0,
          deployModHelper 2 srcUpgrade1,
          initContractHelper 3 srcUpgrade0 "init_contract",
          checkModRefHelper 4 addr0 modUpgrade0 Nothing,
          checkModRefHelper 5 addr0 modUpgrade1 (Just (-1)),
          upgradeHelper 6 addr0 modUpgrade1 Nothing,
          checkModRefHelper 7 addr0 modUpgrade0 (Just (-1)),
          checkModRefHelper 8 addr0 modUpgrade1 Nothing
        ]
    deployModHelper nce src =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 src,
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 src result
            }
    initContractHelper nce src constructor =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 src constructor "",
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        (Types.protocolVersion @pv)
                        src
                        (InitName constructor)
                        (Parameter mempty)
                        Nothing
                        result
            }
    checkModRefHelper nce scAddr expectModRef mreject =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload =
                        Update
                            0
                            (Types.ContractAddress 0 0)
                            "contract.check_module_reference"
                            (params scAddr expectModRef),
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    if Types.supportsContractInspectionQueries spv
                        then case mreject of
                            Nothing -> Helpers.assertSuccess result
                            Just reject ->
                                Helpers.assertRejectWhere
                                    ( \case
                                        Types.RejectedReceive{..}
                                            | rejectReason == reject -> return ()
                                        _ -> assertFailure "Rejected for incorrect reason"
                                    )
                                    result
                        else Helpers.assertRejectWithReason Types.RuntimeFailure result
            }
      where
        params (Types.ContractAddress i si) modRef = BSS.toShort $ runPut $ do
            putWord64le $ fromIntegral i
            putWord64le $ fromIntegral si
            put modRef
    upgradeHelper nce scAddr toModRef mreject =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 scAddr "contract.upgrade" (BSS.toShort $ encode toModRef),
                      metadata = makeDummyHeader accountAddress0 nce 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    if Types.supportsContractInspectionQueries spv
                        then case mreject of
                            Nothing -> Helpers.assertSuccess result
                            Just reject ->
                                Helpers.assertRejectWhere
                                    ( \case
                                        Types.RejectedReceive{..}
                                            | rejectReason == reject -> return ()
                                        _ -> assertFailure "Rejected for incorrect reason"
                                    )
                                    result
                        else Helpers.assertRejectWithReason Types.RuntimeFailure result
            }

tests :: Spec
tests = describe "V1: Contract inspection queries" . sequence_ $
    Helpers.forEveryProtocolVersion $ \spv pvString -> do
        testModuleRefAndName spv pvString
        testUpgradeModuleRef spv pvString
