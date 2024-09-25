{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests calling a contract from a contract and inspecting the return
--    message. Concretely it invokes a counter contract that maintains a 64-bit
--    counter in its state.
module SchedulerTests.SmartContracts.V1.Caller (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Data.Serialize (putByteString, putWord16le, putWord32le, putWord64le, runPut)
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
        [Helpers.makeTestAccountFromSeed 100_000_000 0]

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

contractSourceFile :: FilePath
contractSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/caller.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

test1 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
test1 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Calling another smart contract using entrypoint name containing '<>' fails.") $
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
                    { payload = DeployModule wasmModVersion contractSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 contractSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 wasmModVersion contractSourceFile "init_caller" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        contractSourceFile
                        (InitName "init_caller")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "caller.call" callArgs,
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
            }
        ]
    callArgs = BSS.toShort $ runPut $ do
        putWord32le 1 -- invoke instruction (call to contract)
        putWord64le 0 -- contract index
        putWord64le 0 -- contract subindex
        putWord16le 0 -- length of parameter
        putWord16le (fromIntegral (BSS.length "<String>"))
        putByteString "<String>" -- entrypoint name
        putWord64le 0 -- amount

tests :: Spec
tests =
    describe "V1: Calling other smart contracts" $
        sequence_ $
            Helpers.forEveryProtocolVersion test1
