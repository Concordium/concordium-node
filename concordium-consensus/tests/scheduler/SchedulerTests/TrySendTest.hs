{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This tests a minimal example of error handling in returns of smart contracts.
--  We do two invocations of the receive method, one with a valid account to send
--  to, and one with invalid.
--
--  In the first case we expect a transfer to happen, in the second case we expect no events
--  since the transfer did not happen. However we still expect the transaction to succeed.
module SchedulerTests.TrySendTest (tests) where

import qualified Data.ByteString.Short as BSS
import Data.Serialize (encode)
import Test.HUnit
import Test.Hspec

import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers

import Concordium.Scheduler.DummyData

tests :: Spec
tests =
    describe "SimpleTransfer from contract to account." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                errorHandlingTest spv pvString

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 100_000_000 0]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

toAddr :: BSS.ShortByteString
toAddr = BSS.toShort (encode accountAddress0)

errorHandlingTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
errorHandlingTest _ pvString =
    specify
        (pvString ++ ": Error handling in contracts.")
        $ Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
  where
    -- NOTE: Could also check resulting balances on each affected account or contract, but
    -- the block state invariant at least tests that the total amount is preserved.
    transactionsAndAssertions =
        [ Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V0 "./testdata/contracts/try-send-test.wasm",
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V0 "./testdata/contracts/try-send-test.wasm" "init_try" "",
                      metadata = makeDummyHeader accountAddress0 2 100000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          -- valid account, should succeed in transferring
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 11 (Types.ContractAddress 0 0) "try.receive" toAddr,
                      metadata = makeDummyHeader accountAddress0 3 70000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    Helpers.assertSuccessWithEvents
                        [ Types.Updated
                            { euAddress = Types.ContractAddress 0 0,
                              euInstigator = Types.AddressAccount accountAddress0,
                              euAmount = 11,
                              euMessage = Parameter toAddr,
                              euReceiveName = ReceiveName "try.receive",
                              euContractVersion = V0,
                              euEvents = []
                            },
                          Types.Transferred
                            { etFrom = Types.AddressContract (Types.ContractAddress 0 0),
                              etAmount = 11,
                              etTo = Types.AddressAccount accountAddress0
                            }
                        ]
                        result
            },
          -- transfer did not happen
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 11 (Types.ContractAddress 0 0) "try.receive" (BSS.pack (replicate 32 0)),
                      metadata = makeDummyHeader accountAddress0 4 70000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    Helpers.assertSuccessWithEvents
                        [ Types.Updated
                            { euAddress = Types.ContractAddress 0 0,
                              euInstigator = Types.AddressAccount accountAddress0,
                              euAmount = 11,
                              euMessage = Parameter (BSS.pack (replicate 32 0)),
                              euReceiveName = ReceiveName "try.receive",
                              euContractVersion = V0,
                              euEvents = []
                            }
                        ]
                        result
            }
        ]
