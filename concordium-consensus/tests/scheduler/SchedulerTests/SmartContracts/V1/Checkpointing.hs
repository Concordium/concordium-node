{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This file contains three test cases.
--    The details are outlined in 'checkpointing.wat'
--
--    Most checks are being carried out in the smart contracts.
--    However some checks are also being carried out here in this file.
--    The latter checks are geared towards outcome checking while the
--    former checks are checking that host functions behave as expected and that
--    the integrity of the smart contracts during rollback are upheld.
module SchedulerTests.SmartContracts.V1.Checkpointing (tests) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Serialize (encode, putByteString, putWord16le, putWord64le, runPut)
import Test.Hspec

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
        [ Helpers.makeTestAccountFromSeed 100_000_000 0,
          Helpers.makeTestAccountFromSeed 0 1
        ]

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: ID.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

checkpointingSourceFile :: FilePath
checkpointingSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/checkpointing.wasm"

v0ProxySourceFile :: FilePath
v0ProxySourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/send-message-v1.wasm"

-- | This test has the following call pattern:
-- A
--   -->  B
--          --> A
--          <--
--        B(trap)
-- A <--
--
-- The state at A should be left unchanged by the changes of the 'inner' invocation on contract A.
-- A correctly perceives B's trapping signal.
-- Only V1 contracts are being used.
checkpointingTest1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest1 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Checkpointing 1") $
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
                    { payload = DeployModule V1 checkpointingSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointingSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_b" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_b")
                        (Parameter "")
                        Nothing
                        result
            },
          -- We supply one micro CCD as we expect a trap from a v1 contract.
          -- See the contract for details.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 1 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 3) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        putWord64le 1 -- contract index of contract B
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
        putByteString forwardParameter
        putWord16le (fromIntegral (BSS.length "b_forward_crash")) -- contract b's receive function.
        putByteString "b_forward_crash" -- entrypoint name
        putWord64le 0 -- amount
    forwardParameter = runPut $ do
        putWord64le 0 -- index of contract A
        putWord64le 0 -- subindex of the counter contract
        putWord16le 0 -- length of the empty parameter
        putWord16le (fromIntegral (BSS.length "a_modify"))
        putByteString "a_modify" -- entrypoint name
        putWord64le 0 -- amount

-- | This test has the following call pattern:
-- A
--   -->  B
--          --> A (no modification, just lookup entry)
--          <--
--        B
-- A <--
--
-- The state at A should be left unchanged.
-- An iterator is created in the outer A, and the inner A tries to use it.
-- It is asserted that the iterator does not exist in the inner A call.
-- Only V1 contracts are being used.
checkpointingTest2 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest2 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Checkpointing 2") $
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
                    { payload = DeployModule V1 checkpointingSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointingSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_b" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_b")
                        (Parameter "")
                        Nothing
                        result
            },
          -- We supply zero micro CCDs as we're instructing the contract to not expect state modifications also the contract
          -- does not expect errors i.e. a trap signal from underlying invocations.
          -- The 'inner' call to contract A does not modify the state.
          -- See the contract for details.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 7) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        putWord64le 1 -- contract index of contract B
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
        putByteString forwardParameter
        putWord16le (fromIntegral (BSS.length "b_forward")) -- contract b's receive function.
        putByteString "b_forward" -- entrypoint name
        putWord64le 0 -- amount
    forwardParameter = runPut $ do
        putWord64le 0 -- index of contract A
        putWord64le 0 -- subindex of the counter contract
        putWord16le 0 -- length of the empty parameter
        putWord16le (fromIntegral (BSS.length "a_no_modify"))
        putByteString "a_no_modify" -- entrypoint name
        putWord64le 0 -- amount

-- | This test has the following call pattern:
-- A
--   -->  Transfer
-- A <--
--
-- The state at A should be left unchanged.
-- The iterator initialized at A should after the call point to the same entry as before the call.
-- Only V1 contracts are being used.
checkpointingTest3 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest3 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Checkpointing 3") $
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
                    { payload = DeployModule V1 checkpointingSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointingSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_b" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_b")
                        (Parameter "")
                        Nothing
                        result
            },
          -- We supply three micro CCDs as we're instructing the contract to carry out a transfer instead of a call.
          -- See the contract for details.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 3 (Types.ContractAddress 0 0) "a.a_modify_proxy" (BSS.toShort (encode accountAddress1)),
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 4) result
            }
        ]

-- | This test has the following call pattern:
-- A
--   -->  B
--          --> A modify
--          <--
--        B
-- A <--
--
-- The state at A should have changed according to the 'inner' invocation on contract A.
-- Only V1 contracts are being used.
checkpointingTest4 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest4 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Checkpointing 4") $
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
                    { payload = DeployModule V1 checkpointingSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointingSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_b" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_b")
                        (Parameter "")
                        Nothing
                        result
            },
          -- We supply four micro CCDs as we're instructing the contract to expect state modifications
          -- being made from the 'inner' contract A call to be in effect when returned to the caller (a.a_modify_proxy)
          -- See the contract for details.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 4 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 7) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        putWord64le 1 -- contract index of contract B
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
        putByteString forwardParameter
        putWord16le (fromIntegral (BSS.length "b_forward")) -- contract b's receive function.
        putByteString "b_forward" -- entrypoint name
        putWord64le 0 -- amount
    forwardParameter = runPut $ do
        putWord64le 0 -- index of contract A
        putWord64le 0 -- subindex of the counter contract
        putWord16le 0 -- length of the empty parameter
        putWord16le (fromIntegral (BSS.length "a_modify"))
        putByteString "a_modify" -- entrypoint name
        putWord64le 0 -- amount

-- | This test has the following call pattern:
-- A
--      -->  V0Proxy
--                --> B
--                      --> A
--                      <--
--                    B(trap)
--                <--
--
-- A    <--
--
-- The state at A should have changed according to the 'inner' invocation on contract A.
-- Contract A is V1, contract B is V1, and the proxy contract is V0.
checkpointingTest5 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest5 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Cross Checkpointing 1") $
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
                    { payload = DeployModule V1 checkpointingSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointingSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V0 v0ProxySourceFile,
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV0 v0ProxySourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_b" "",
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_b")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V0 v0ProxySourceFile "init_proxy" "",
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        v0ProxySourceFile
                        (InitName "init_proxy")
                        (Parameter "")
                        Nothing
                        result
            },
          -- We supply two micro CCDs as we expect a trap from a v0 contract.
          -- See the contract for details.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 2 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters,
                      metadata = makeDummyHeader accountAddress0 6 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 3) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        putWord64le 2 -- contract index of proxy contract
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BS.length forwardParameter0)) -- length of parameter
        putByteString forwardParameter0
        putWord16le (fromIntegral (BSS.length "forward")) -- contract b's receive function.
        putByteString "forward" -- entrypoint name
        putWord64le 0 -- amount
        -- This is invoked by a v0 contract so the parameter and receive adddress + method are swapped.
        -- Also V0 contracts invoke others by their fully qualified name i.e. contract-name.receive-name
    forwardParameter0 = runPut $ do
        putWord64le 1 -- contract index of contract B
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BSS.length "b.b_forward_crash")) -- contract b's receive function.
        putByteString "b.b_forward_crash" -- entrypoint name
        putWord16le (fromIntegral (BS.length forwardParameter1)) -- length of parameter
        putByteString forwardParameter1
        putWord64le 0 -- amount
    forwardParameter1 = runPut $ do
        putWord64le 0 -- index of contract A
        putWord64le 0 -- subindex of the counter contract
        putWord16le 0 -- length of the empty parameter
        putWord16le (fromIntegral (BSS.length "a_modify"))
        putByteString "a_modify" -- entrypoint name
        putWord64le 0 -- amount

-- | This test has the following call pattern:
-- A
--      -->  V0Proxy
--                --> B
--                      --> A
--                      <--
--                    B
--                <--
--
-- A    <--
--
-- The state at A should have changed according to the 'inner' invocation on contract A.
-- Contract A is V1, contract B is V1, and the proxy contract is V0.
checkpointingTest6 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest6 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Cross Checkpointing 2") $
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
                    { payload = DeployModule V1 checkpointingSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointingSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V0 v0ProxySourceFile,
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV0 v0ProxySourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_a" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_a")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointingSourceFile "init_b" "",
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointingSourceFile
                        (InitName "init_b")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V0 v0ProxySourceFile "init_proxy" "",
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        v0ProxySourceFile
                        (InitName "init_proxy")
                        (Parameter "")
                        Nothing
                        result
            },
          -- We supply four micro CCDs as we're instructing the contract to expect state modifications
          -- being made from the 'inner' contract A call to be in effect when returned to the caller (a.a_modify_proxy)
          -- See the contract for details.
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 4 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters,
                      metadata = makeDummyHeader accountAddress0 6 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 8) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        putWord64le 2 -- contract index of proxy contract
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BS.length forwardParameter0)) -- length of parameter
        putByteString forwardParameter0
        putWord16le (fromIntegral (BSS.length "forward")) -- contract b's receive function.
        putByteString "forward" -- entrypoint name
        putWord64le 0 -- amount
        -- This is invoked by a v0 contract so the parameter and receive adddress + method are swapped.
        -- Also V0 contracts invoke others by their fully qualified name i.e. contract-name.receive-name
    forwardParameter0 = runPut $ do
        putWord64le 1 -- contract index of contract B
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BSS.length "b.b_forward")) -- contract b's receive function.
        putByteString "b.b_forward" -- entrypoint name
        putWord16le (fromIntegral (BS.length forwardParameter1)) -- length of parameter
        putByteString forwardParameter1
        putWord64le 0 -- amount
    forwardParameter1 = runPut $ do
        putWord64le 0 -- index of contract A
        putWord64le 0 -- subindex of the counter contract
        putWord16le 0 -- length of the empty parameter
        putWord16le (fromIntegral (BSS.length "a_modify"))
        putByteString "a_modify" -- entrypoint name
        putWord64le 0 -- amount

checkpointing2SourceFile :: FilePath
checkpointing2SourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/checkpointing-2.wasm"

-- | Tests the following flow within a single contract instance.
--
--  - Invoke entrypoint 'a'
--    - Set state = 112
--    - Invoke entrypoint 'b'
--      - Query own balance
--      - Set state = 113
--      - Fail with error -1
--    - Assert state == 112 (rollback occurred)
--
-- In protocols 4 & 5 the behaviour of rollbacks is incorrect which leads to an
-- assertion violation. In P6 the behaviour is fixed.
checkpointingTest7 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest7 spv pvString =
    when (Types.demoteProtocolVersion spv >= Types.P4) $
        specify (pvString ++ ": Checkpointing 7") $
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
                    { payload = DeployModule V1 checkpointing2SourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointing2SourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointing2SourceFile "init_test" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointing2SourceFile
                        (InitName "init_test")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "test.a" callArgs,
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    if Types.demoteProtocolVersion spv >= Types.P6
                        then Helpers.assertSuccess result
                        else Helpers.assertRejectWithReason Types.RuntimeFailure result
            }
        ]

    -- Tell the contract to call entrypoint 'c'.
    callArgs = BSS.toShort $ runPut $ putByteString "b"

-- | Tests the following flow within a single contract instance.
--
--   - Invoke entrypoint 'a'
--     - Set state = 112
--     - Invoke entrypoint 'c'
--       - Invoke entrypoint 'd'
--         - Return success
--       - Set state = 113
--       - Fail with error -1
--     - Assert state == 112 (rollback occurred)
--
-- In protocols 4 & 5 the behaviour of rollbacks is incorrect which leads to an
-- assertion violation. In P6 the behaviour is fixed.
checkpointingTest8 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest8 spv pvString =
    when (Types.demoteProtocolVersion spv >= Types.P4) $
        specify (pvString ++ ": Checkpointing 8") $
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
                    { payload = DeployModule V1 checkpointing2SourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointing2SourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointing2SourceFile "init_test" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointing2SourceFile
                        (InitName "init_test")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "test.a" callArgs,
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    if Types.demoteProtocolVersion spv >= Types.P6
                        then Helpers.assertSuccess result
                        else Helpers.assertRejectWithReason Types.RuntimeFailure result
            }
        ]
    -- Tell the contract to call entrypoint 'c'.
    callArgs = BSS.toShort $ runPut $ putByteString "c"

-- | Tests the following flow within a single contract instance.
--
--   - Invoke entrypoint 'e'
--     - Set state at []
--     - Invoke entrypoint 'f'
--       - look up an and entry at [0,0,0,0]
--       - invoke entrypoint d which does nothing and returns
--       - try to read from the previously created entry, make sure it succeeds and returns the correct value.
--
-- In protocols 4 & 5 the behaviour of rollbacks is incorrect which leads to an
-- assertion violation. In P6 the behaviour is fixed.
checkpointingTest9 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
checkpointingTest9 spv pvString =
    when (Types.demoteProtocolVersion spv >= Types.P4) $
        specify (pvString ++ ": Checkpointing 9") $
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
                    { payload = DeployModule V1 checkpointing2SourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 checkpointing2SourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 checkpointing2SourceFile "init_test" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        checkpointing2SourceFile
                        (InitName "init_test")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "test.e" callArgs,
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    if Types.demoteProtocolVersion spv >= Types.P6
                        then Helpers.assertSuccess result
                        else Helpers.assertRejectWithReason Types.RuntimeFailure result
            }
        ]
    -- Tell the contract to call entrypoint 'f' that looks up an entry and then
    -- calls the success entrypoint.
    callArgs = BSS.toShort $ runPut $ putByteString "f"

tests :: Spec
tests =
    describe "V1: Checkpointing." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                checkpointingTest1 spv pvString
                checkpointingTest2 spv pvString
                checkpointingTest3 spv pvString
                checkpointingTest4 spv pvString
                checkpointingTest5 spv pvString
                checkpointingTest6 spv pvString
                checkpointingTest7 spv pvString
                checkpointingTest8 spv pvString
                checkpointingTest9 spv pvString
