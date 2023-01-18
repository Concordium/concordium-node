{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the relaxed smart contract restrictions introduced in P5 for V1 contracts.
--    The old and new limits are checked, in P4 and P5, respectively.
--    The limit changes in P5 are:
--      - Parameter size limit: 1kb -> 65kb
--      - Return value size limit: 16kb -> no limit (apart from energy)
--      - Number of logs: 64 -> no limit (apart from energy)
--      - Cost of parameters:
--        - Of size <= 1kb: base cost + 1NRG / 1 *kilobyte* (same as before P5)
--        - Of size > 1 kb: base cost + 1NRG / 1 *byte*
module SchedulerTests.SmartContracts.V1.RelaxedRestrictions (tests) where

import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Serialize (putByteString, putWord16le, putWord32le, runPut)
import Data.Word (Word16, Word32)

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.ID.Types as ID
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

sourceFile :: FilePath
sourceFile = "./testdata/contracts/v1/relaxed-restrictions.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

-- | Ensure the parameter limit is correct before the relaxation in protocol version 5
oldParameterLimitTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
oldParameterLimitTest spv pvString =
    -- The relaxed restrictions was introduced together with upgradable smart contracts.
    unless (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Correct parameter size limits (old)") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        deployAndInitTransactions
            ++ [
                 -- Check that the max size parameter is allowed.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 1_024 1_024),
                              metadata = makeDummyHeader accountAddress0 3 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    },
                 -- Check that if the top-level parameter is too big, we get a serialization failure.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 1_024 1_025),
                              metadata = makeDummyHeader accountAddress0 4 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertRejectWithReason Types.SerializationFailure result
                    },
                 -- Check that if the inter-contract parameter is too big, we get a runtime failure.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 1_025 1_024),
                              metadata = makeDummyHeader accountAddress0 5 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertRejectWithReason Types.RuntimeFailure result
                    }
               ]

-- | Ensure the return value size limit is correct before the relaxation in protocol version 5
oldReturnValueLimitTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
oldReturnValueLimitTest spv pvString =
    -- The relaxed restrictions was introduced together with upgradable smart contracts.
    unless (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Correct return value size limits (old)") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        deployAndInitTransactions
            ++ [
                 -- Check that the max size return value is allowed.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.return-value" (callArgsWord32 16_384),
                              metadata = makeDummyHeader accountAddress0 3 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    },
                 -- Check that one above the max size is truncated to the max. Which in turn makes the contract fail.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.return-value" (callArgsWord32 16_385),
                              metadata = makeDummyHeader accountAddress0 4 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertRejectWithReason Types.RuntimeFailure result
                    }
               ]

-- | Ensure the log limit is correct before the relaxation in protocol version 5
oldLogLimitTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
oldLogLimitTest spv pvString =
    -- The relaxed restrictions was introduced together with upgradable smart contracts.
    unless (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Correct number of logs limits (old)") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        deployAndInitTransactions
            ++ [
                 -- Check that the max number of logs is allowed.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.logs" (callArgsWord32 64),
                              metadata = makeDummyHeader accountAddress0 3 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    },
                 -- Check that one above the max number of logs is _not_ allowed.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.logs" (callArgsWord32 65),
                              metadata = makeDummyHeader accountAddress0 4 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertRejectWithReason Types.RuntimeFailure result
                    }
               ]

-- | Ensure the parameter limit is correct after the relaxation in protocol version 5.
newParameterLimitTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
newParameterLimitTest spv pvString =
    -- The relaxed restrictions was introduced together with upgradable smart contracts.
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Correct parameter size limits (new)") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        deployAndInitTransactions
            ++ [
                 -- Check that the max size parameter is allowed. We cannot check above it easily,
                 -- because it is Word16::MAX.
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 65_535 65_535),
                              metadata = makeDummyHeader accountAddress0 3 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    }
               ]

-- | Ensure the return value size limit is correct after the relaxation in protocol version 5
newReturnValueLimitTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
newReturnValueLimitTest spv pvString =
    -- The relaxed restrictions was introduced together with upgradable smart contracts.
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Correct return value size limits (new)") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        deployAndInitTransactions
            ++ [
                 -- -- Check that a large return value can be returned (larger than what is allowed in P4).
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.return-value" (callArgsWord32 100_000),
                              metadata = makeDummyHeader accountAddress0 3 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    }
               ]

-- | Ensure the log limit is correct after the relaxation in protocol version 5.
newLogLimitTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
newLogLimitTest spv pvString =
    when (Types.supportsUpgradableContracts spv) $
        specify (pvString ++ ": Correct number of logs limits (new)") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        deployAndInitTransactions
            ++ [
                 -- Check that a large number of logs is allowed (more than allowed in P4).
                 Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (Types.ContractAddress 0 0) "relax.logs" (callArgsWord32 64),
                              metadata = makeDummyHeader accountAddress0 3 700_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    }
               ]

-- |Transactions and assertions for deploying and initializing the "relax" contract.
deployAndInitTransactions :: [Helpers.TransactionAndAssertion pv]
deployAndInitTransactions =
    [ Helpers.TransactionAndAssertion
        { taaTransaction =
            TJSON
                { payload = DeployModule wasmModVersion sourceFile,
                  metadata = makeDummyHeader accountAddress0 1 100_000,
                  keys = [(0, [(0, keyPair0)])]
                },
          taaAssertion = \result _ ->
            return $ do
                Helpers.assertSuccess result
                Helpers.assertUsedEnergyDeploymentV1 sourceFile result
        },
      Helpers.TransactionAndAssertion
        { taaTransaction =
            TJSON
                { payload = InitContract 0 wasmModVersion sourceFile "init_relax" "",
                  metadata = makeDummyHeader accountAddress0 2 100_000,
                  keys = [(0, [(0, keyPair0)])]
                },
          taaAssertion = \result _ ->
            return $ do
                Helpers.assertSuccess result
                Helpers.assertUsedEnergyInitialization
                    sourceFile
                    (InitName "init_relax")
                    (Parameter "")
                    Nothing
                    result
        }
    ]

-- |Creates a parameter for "relax.param".
--
-- The first input is the size of the internal parameter to be passed to "param-aux".
-- This is used to test the parameter limit checked inside the wasm interpreter.
--
-- The second input is the desired total length of the bytestring produced by this function.
-- Once the necessary data is written, extra 1s are written until the desired length is reached.
-- This is used to test the parameter limit checked in the scheduler.
callArgsParam ::
    -- |Size of the internal parameter to be used by the contract when invoking "param-aux".
    Word16 ->
    -- |The (desired) length of bytestring returned by this function.
    Int ->
    BSS.ShortByteString
callArgsParam internalParamSize desiredLen = BSS.toShort $ runPut $ do
    putWord16le internalParamSize
    putWord16le auxNameLen -- entrypoint name len
    putByteString auxName -- entrypoint name
    putByteString (BS.pack $ replicate numBytes 1) -- arbitrary bytes to fill the parameter
  where
    auxName = "param-aux"
    auxNameLen :: Word16
    auxNameLen = fromIntegral $ BS.length auxName
    -- Calculate the number of arbitrary bytes to put in the end, so that the whole parameter gets the desired length.
    numBytes =
        desiredLen
            - 2 -- internalParamSize
            - 2 -- auxNameLen
            - fromIntegral auxNameLen

-- |Create a Word32 parameter.
callArgsWord32 :: Word32 -> BSS.ShortByteString
callArgsWord32 = BSS.toShort . runPut . putWord32le

tests :: Spec
tests =
    describe "Smart contracts V1: Relax restrictions." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString ->
                when (Types.supportsV1Contracts spv) $ do
                    oldParameterLimitTest spv pvString
                    oldReturnValueLimitTest spv pvString
                    oldLogLimitTest spv pvString
                    newParameterLimitTest spv pvString
                    newLogLimitTest spv pvString
                    newReturnValueLimitTest spv pvString
