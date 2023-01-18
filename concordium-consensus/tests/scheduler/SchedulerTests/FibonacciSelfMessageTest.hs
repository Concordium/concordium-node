{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This tests sending messages to contracts. Concretely it invokes
--    A fibonacci contract that adds the n-th Fibonacci number to its state,
--    by repeatedly sending itself messages.
--
--    See ../smart-contracts/rust-contracts/example-contracts/fib for the source code.
module SchedulerTests.FibonacciSelfMessageTest (tests) where

import Test.HUnit
import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Serialize (putWord64le, runPut)
import Data.Word

import qualified Concordium.Cost as Cost
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm

import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    Types.IsProtocolVersion pv =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 100_000_000 0,
          Helpers.makeTestAccountFromSeed 100_000_000 1
        ]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

fibParamBytes :: Word64 -> BSS.ShortByteString
fibParamBytes n = BSS.toShort $ runPut (putWord64le n)

fibSourceFile :: FilePath
fibSourceFile = "./testdata/contracts/fib.wasm"

testCase1 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
testCase1 _ pvString =
    specify
        (pvString ++ ": Error handling in contracts.")
        $ Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactions
  where
    transactions =
        [ Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.DeployModule V0 fibSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV0 fibSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.InitContract 0 V0 fibSourceFile "init_fib" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        fibSourceFile
                        (InitName "init_fib")
                        (Parameter "")
                        (Just 8) -- The initial state size should be 8 bytes.
                        result
            },
          -- compute F(10)
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload = Runner.Update 0 (Types.ContractAddress 0 0) "fib.receive" (fibParamBytes 10),
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
                    },
              taaAssertion = ensureAllUpdates
            }
        ]
    ensureAllUpdates ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    ensureAllUpdates Helpers.SchedulerResult{..} bs = do
        maybeInstance <- BS.bsoGetInstance bs (Types.ContractAddress 0 0)
        return $ do
            -- Check that the contract state contains the n-th Fib number.
            case maybeInstance of
                Nothing -> assertFailure "Instance at <0,0> does not exist."
                Just (BS.InstanceInfoV0 ii) -> do
                    let Instances.InstanceStateV0 s = BS.iiState ii
                    assertEqual "State contains the n-th Fibonacci number." (fibNBytes 10) s
                Just _ -> assertFailure "Expected V0 instance."

            case Helpers.getResults $ Sch.ftAdded srTransactions of
                [(_, Types.TxSuccess evs)] -> do
                    mapM_ p evs -- check that all updates are the right ones
                    -- and check that the cost was adequate (we again only check the lower bound only)
                    moduleSource <- wasmSource <$> Helpers.readV0ModuleFile fibSourceFile
                    let modLen = fromIntegral $ BS.length moduleSource
                        payloadSize =
                            Types.payloadSize $
                                Types.encodePayload $
                                    Types.Update 0 (Types.ContractAddress 0 0) (ReceiveName "fib.receive") (Parameter (fibParamBytes 10))
                        -- size of the transaction minus the signatures.
                        txSize = Types.transactionHeaderSize + fromIntegral payloadSize
                        -- transaction is signed with 1 signature
                        baseTxCost = Cost.baseCost txSize 1
                        -- lower bound on the cost of the transaction, assuming no interpreter energy and the instance is not created.
                        -- the number of invocations of the smart contract is fibOne 10, see below for the definition of fibOne
                        -- the size of the contract state is 8 bytes (one u64)
                        costLowerBound = baseTxCost + (fibOne 10) * Cost.updateContractInstanceCost 0 modLen 8 (Just 8)
                    unless (srUsedEnergy >= costLowerBound) $
                        assertFailure $
                            "Actual update cost " ++ show srUsedEnergy ++ " not more than lower bound " ++ show costLowerBound
                [(_, Types.TxReject rr)] -> assertFailure $ "Fibonacci update should succeed, but it failed with " ++ show rr
                _ -> assertFailure "Fibonacci update should succeed, but failed"

    p
        Types.Updated
            { euAmount = 0,
              euEvents = [],
              ..
            } = case euInstigator of
            Types.AddressAccount addr -> assertEqual "Only the initial account can be in events." addr accountAddress0
            Types.AddressContract addr -> assertEqual "Contract address is self-address" addr euAddress
    p event = assertFailure ("Unexpected event: " ++ show event)

    fib n =
        let go = 1 : 1 : zipWith (+) go (tail go)
        in  go !! n
    fibNBytes n = ContractState (runPut (putWord64le (fib n)))

    -- the number of invocations of the contract follows https://oeis.org/A001595
    fibOne n = 2 * fib n - 1

tests :: Spec
tests =
    describe "Self-referential Fibonacci." $
        sequence_ $
            Helpers.forEveryProtocolVersion testCase1
