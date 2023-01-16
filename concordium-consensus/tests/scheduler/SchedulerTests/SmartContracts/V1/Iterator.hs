{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests calling a contract which makes use of an iterator.
--    The checks are being performed in the contract itself so if invoking the
--    contract completes successfully then this implies that the tests have done so as well.
--    Note. as per above no checks are being performed in this file wrt. the state etc. after execution etc.
module SchedulerTests.SmartContracts.V1.Iterator (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "V1: Iterator." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                test1 spv pvString

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

iteratorSourceFile :: FilePath
iteratorSourceFile = "./testdata/contracts/v1/iterator.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

test1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
test1 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Iterator") $
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
                    { payload = DeployModule wasmModVersion iteratorSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 iteratorSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 wasmModVersion iteratorSourceFile "init_iterator" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        iteratorSourceFile
                        (InitName "init_iterator")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "iterator.iteratetest" BSS.empty,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "iterator.lockingtest" BSS.empty,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            }
        ]
