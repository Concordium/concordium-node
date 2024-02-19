{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the contract inspection functionality of the invoke host function.
module SchedulerTests.SmartContracts.V1.ContractInspection where

import qualified Data.ByteString.Short as BSS
import Data.Serialize (Serialize (put), encode, putByteString, putWord64le, runPut)
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

-- | This test deploys three different smart contracts, from two different modules.
--  One of these contracts <0,0> has a function for checking if the module reference matches an
--  expected value. Another <2,0> has a function for checking if the contract name matches an
--  expected value. Both functions are invoked for each instance, and for non-existant contract
--  addresses, ensuring the values are as expected.
--
--  The entrypoints are designed to succeed in the case of a match, fail with code -1 if there is
--  a mismatch, and fail with code -2 if the contract address does not exist. If the protocol
--  version does not support the contract inspection functionality, then the call should fail with
--  a runtime exception.
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
          checkModRefHelper 6 (Types.ContractAddress 0 0) modRefQueriesContractInspection Nothing,
          checkModRefHelper 7 (Types.ContractAddress 1 0) modRefQueriesContractInspection (Just (-1)),
          checkModRefHelper 8 (Types.ContractAddress 2 0) modRefQueriesContractInspection Nothing,
          checkModRefHelper 9 (Types.ContractAddress 3 0) modRefQueriesContractInspection (Just (-2)),
          checkModRefHelper 10 (Types.ContractAddress 0 1) modRefQueriesContractInspection (Just (-2)),
          checkModRefHelper 11 (Types.ContractAddress 1 0) modRefQueriesAccountBalance Nothing,
          checkNameHelper 12 (Types.ContractAddress 0 0) "init_contract" Nothing,
          checkNameHelper 13 (Types.ContractAddress 1 0) "init_contract" Nothing,
          checkNameHelper 14 (Types.ContractAddress 2 0) "init_contract" (Just (-1)),
          checkNameHelper 15 (Types.ContractAddress 2 0) "init_contract2" Nothing,
          checkNameHelper 16 (Types.ContractAddress 3 0) "init_contract" (Just (-2)),
          checkNameHelper 17 (Types.ContractAddress 0 0) "init_contract2" (Just (-1)),
          checkNameHelper 18 (Types.ContractAddress 0 1) "init_contract2" (Just (-2))
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
    checkNameHelper nce scAddr expectName mreject =
        Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload =
                        Update
                            0
                            (Types.ContractAddress 2 0)
                            "contract2.check_name"
                            (params scAddr expectName),
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
        params (Types.ContractAddress i si) name = BSS.toShort $ runPut $ do
            putWord64le $ fromIntegral i
            putWord64le $ fromIntegral si
            putByteString name

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
