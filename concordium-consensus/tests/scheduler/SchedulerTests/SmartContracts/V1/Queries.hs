{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the chain queries which were implemented as part of P5.
module SchedulerTests.SmartContracts.V1.Queries (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Data.Serialize (Serialize (put), putWord64le, runPut)
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.Accounts
import Concordium.Types.DummyData
import Concordium.Wasm
import qualified Concordium.Wasm as Wasm
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "V1: Queries" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                accountBalanceTestCase spv pvString
                accountBalanceInvokerTestCase spv pvString
                accountBalanceTransferTestCase spv pvString
                accountBalanceMissingAccountTestCase spv pvString
                contractBalanceTestCase spv pvString
                contractBalanceSelfTestCase spv pvString
                contractBalanceTransferTestCase spv pvString
                contractBalanceMissingContractTestCase spv pvString
                exchangeRatesTestCase spv pvString
                allTestCase spv pvString

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed balance0 0,
          Helpers.makeTestAccountFromSeed balance1 1
        ]

initialBlockStateWithStakeAndSchedule ::
    (Types.IsProtocolVersion pv, Types.SupportsDelegation pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockStateWithStakeAndSchedule =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed balance0 0,
          account1
        ]
  where
    account1 =
        Helpers.makeTestAccountFromSeed balance1 1
            >>= BS.addAccountDelegator accountDelegation

    accountDelegation =
        AccountDelegationV1
            { _delegationIdentity = 1,
              _delegationStakedAmount = 234,
              _delegationStakeEarnings = False,
              _delegationTarget = Types.DelegatePassive,
              _delegationPendingChange = NoChange
            }

balance0 :: Types.Amount
balance0 = 100_000_000

balance1 :: Types.Amount
balance1 = 100_000_000

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: ID.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

keyPair1 :: SigScheme.KeyPair
keyPair1 = Helpers.keyPairFromSeed 1

blockEnergyRate :: Types.EnergyRate
blockEnergyRate = dummyChainParameters @'Types.ChainParametersV1 ^. Types.energyRate

accountBalanceSourceFile :: FilePath
accountBalanceSourceFile = "./testdata/contracts/v1/queries-account-balance.wasm"

accountBalanceTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
accountBalanceTestCase spv pvString =
    case Types.delegationSupport @(AccountVersionFor pv) of
        Types.SAVDelegationNotSupported -> return ()
        Types.SAVDelegationSupported -> do
            when (Types.supportsChainQueryContracts spv) $
                specify (pvString ++ ": Simple account balance query") $
                    Helpers.runSchedulerTestAssertIntermediateStates
                        @pv
                        Helpers.defaultTestConfig
                        initialBlockStateWithStakeAndSchedule
                        transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule V1 accountBalanceSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 accountBalanceSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = TransferWithSchedule accountAddress1 [(Types.Timestamp maxBound, 123)],
                      metadata = makeDummyHeader accountAddress0 3 10_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        put accountAddress1
        putWord64le (fromIntegral balance1 + 123) -- expected public balance
        putWord64le 234 -- expected staked balance
        putWord64le 123 -- expected locked balance

accountBalanceInvokerTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
accountBalanceInvokerTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the account balance of the invoker") $
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
                    { payload = DeployModule V1 accountBalanceSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 accountBalanceSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update updateAmount (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress1 1 energyLimit,
                      keys = [(0, [(0, keyPair1)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    energyLimit = 100_000
    updateAmount = 123
    parameters = BSS.toShort $ runPut $ do
        put accountAddress1
        Wasm.putAmountLE $ balance1 - costUpperBound - updateAmount -- expected public balance
        Wasm.putAmountLE 0 -- expected staked amount
        Wasm.putAmountLE 0 -- expected locked amount
    costUpperBound = Types.computeCost blockEnergyRate energyLimit

accountBalanceTransferSourceFile :: FilePath
accountBalanceTransferSourceFile = "./testdata/contracts/v1/queries-account-balance-transfer.wasm"

accountBalanceTransferTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
accountBalanceTransferTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Contracts transfers to an account and then queries") $
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
                    { payload = DeployModule V1 accountBalanceTransferSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 123 V1 accountBalanceTransferSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 3 events for the transfer.
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 4) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        put accountAddress1
        Wasm.putAmountLE 123
        Wasm.putAmountLE $ balance1 + 123 -- expected public balance
        Wasm.putAmountLE 0 -- expected staked amount
        Wasm.putAmountLE 0 -- expected locked amount

accountBalanceMissingAccountSourceFile :: FilePath
accountBalanceMissingAccountSourceFile = "./testdata/contracts/v1/queries-account-balance-missing-account.wasm"

accountBalanceMissingAccountTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
accountBalanceMissingAccountTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the balance of a missing account") $
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
                    { payload = DeployModule V1 accountBalanceMissingAccountSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 123 V1 accountBalanceMissingAccountSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        put $ accountAddressFrom 3

contractBalanceSourceFile :: FilePath
contractBalanceSourceFile = "./testdata/contracts/v1/queries-contract-balance.wasm"

contractBalanceTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
contractBalanceTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the balance a contract") $
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
                    { payload = DeployModule V1 contractBalanceSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 contractBalanceSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract initAmount V1 contractBalanceSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    initAmount = 123
    parameters = BSS.toShort $ runPut $ do
        putWord64le 1 -- Index of the contract address.
        putWord64le 0 -- Subindex of the contract address.
        Wasm.putAmountLE initAmount -- Expected balance.

contractBalanceSelfTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
contractBalanceSelfTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the balance of the contract itself") $
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
                    { payload = DeployModule V1 contractBalanceSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract initAmount V1 contractBalanceSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update updateAmount (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    initAmount = 123
    updateAmount = 456
    parameters = BSS.toShort $ runPut $ do
        putWord64le 0 -- Index of the contract address.
        putWord64le 0 -- Subindex of the contract address.
        Wasm.putAmountLE $ initAmount + updateAmount -- Expected balance.

contractBalanceTransferSourceFile :: FilePath
contractBalanceTransferSourceFile = "./testdata/contracts/v1/queries-contract-balance-transfer.wasm"

contractBalanceTransferTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
contractBalanceTransferTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the balance a contract") $
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
                    { payload = DeployModule V1 contractBalanceTransferSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract initAmount V1 contractBalanceTransferSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update updateAmount (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 3 events for transfering
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 4) result
            }
        ]
    initAmount = 123
    updateAmount = 456
    transferAmount = 78

    parameters = BSS.toShort $ runPut $ do
        put accountAddress1
        Wasm.putAmountLE transferAmount
        putWord64le 0 -- Index of the contract address.
        putWord64le 0 -- Subindex of the contract address.
        Wasm.putAmountLE $ initAmount + updateAmount - transferAmount -- Expected balance.

contractBalanceMissingContractSourceFile :: FilePath
contractBalanceMissingContractSourceFile = "./testdata/contracts/v1/queries-contract-balance-missing-contract.wasm"

contractBalanceMissingContractTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
contractBalanceMissingContractTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the balance of a missing contract") $
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
                    { payload = DeployModule V1 contractBalanceMissingContractSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 123 V1 contractBalanceMissingContractSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        putWord64le 1 -- Contract address index
        putWord64le 0 -- Contract address subindex

exchangeRatesSourceFile :: FilePath
exchangeRatesSourceFile = "./testdata/contracts/v1/queries-exchange-rates.wasm"

exchangeRatesTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
exchangeRatesTestCase spv pvString =
    when (Types.supportsChainQueryContracts spv) $
        specify (pvString ++ ": Query the exchange rates") $
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
                    { payload = DeployModule V1 exchangeRatesSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 exchangeRatesSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters,
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                -- Check the number of events:
                -- - 1 event for a succesful update to the contract.
                return $ Helpers.assertSuccessWhere (Helpers.assertNumberOfEvents 1) result
            }
        ]
    parameters = BSS.toShort $ runPut $ do
        Wasm.putExchangeRateLE currentEuroPerEnergy
        Wasm.putExchangeRateLE currentAmountPerEnergy

    currentEuroPerEnergy = dummyChainParameters @'Types.ChainParametersV1 ^. Types.euroPerEnergy
    currentAmountPerEnergy = dummyChainParameters @'Types.ChainParametersV1 ^. Types.microGTUPerEuro

allSourceFile :: FilePath
allSourceFile = "./testdata/contracts/v1/queries-all.wasm"

allTestCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
allTestCase spv pvString =
    when (Types.supportsV1Contracts spv) $
        unless (Types.supportsChainQueryContracts spv) $
            specify (pvString ++ ": Ensure all of the queries fail prior to PV5") $
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
                    { payload = DeployModule V1 allSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 V1 allSourceFile "init_contract" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.account_balance" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason Types.RuntimeFailure result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.contract_balance" "",
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason Types.RuntimeFailure result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "contract.exchange_rates" "",
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason Types.RuntimeFailure result
            }
        ]
