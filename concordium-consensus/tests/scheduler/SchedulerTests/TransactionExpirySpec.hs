{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchedulerTests.TransactionExpirySpec (tests) where

import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.Accounts (
    bakerAggregationVerifyKey,
    bakerElectionVerifyKey,
    bakerInfo,
    bakerSignatureVerifyKey,
 )
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "Transaction expiry test:" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                specify (pvString ++ ": Valid transactions of all payloads with expiry after slot time pass") $
                    testExpiryTime (expiryTime + 1) (allTransactions spv) spv
                specify (pvString ++ ": Same transactions with expiry set to slot time pass") $
                    testExpiryTime expiryTime (allTransactions spv) spv
                specify (pvString ++ ": Same transactions with expiry set before slot time fail") $
                    testExpiryTime (expiryTime - 1) (allTransactions spv) spv

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 310_000_000_000 0]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

baker :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker = mkFullBaker 1 0

-- A list of transactions all of which are valid unless they are expired.
-- Ideally, this should include all payload types to ensure that expiry is handled for
-- all types of transactions.
-- TODO: Add other transaction types.
allTransactions ::
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    Types.TransactionExpiryTime ->
    [TransactionJSON]
allTransactions spv expiry =
    transferTransactions expiry
        ++ if Types.supportsDelegation spv then [] else bakerV0Transactions expiry

transferTransactions :: Types.TransactionExpiryTime -> [TransactionJSON]
transferTransactions expiry =
    [ TJSON
        { payload = Transfer{toaddress = accountAddress0, amount = 10_000},
          metadata = makeHeaderWithExpiry accountAddress0 1 100_000 expiry,
          keys = [(0, [(0, keyPair0)])]
        }
    ]

bakerV0Transactions :: Types.TransactionExpiryTime -> [TransactionJSON]
bakerV0Transactions expiry =
    [ TJSON
        { payload =
            AddBaker
                { bElectionVerifyKey = baker ^. _1 . bakerInfo . bakerElectionVerifyKey,
                  bElectionSecretKey = baker ^. _2,
                  bSignVerifyKey = baker ^. _1 . bakerInfo . bakerSignatureVerifyKey,
                  bSignSecretKey = baker ^. _3,
                  bAggregateVerifyKey = baker ^. _1 . bakerInfo . bakerAggregationVerifyKey,
                  bAggregateSecretKey = baker ^. _4,
                  bInitialStake = 300_000_000_000,
                  bRestakeEarnings = True
                },
          metadata = makeHeaderWithExpiry accountAddress0 2 100_000 expiry,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { payload = UpdateBakerStake 300_000_000_001,
          metadata = makeHeaderWithExpiry accountAddress0 3 100_000 expiry,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { payload =
            UpdateBakerKeys
                { bElectionVerifyKey = baker ^. _1 . bakerInfo . bakerElectionVerifyKey,
                  bElectionSecretKey = baker ^. _2,
                  bSignVerifyKey = baker ^. _1 . bakerInfo . bakerSignatureVerifyKey,
                  bSignSecretKey = baker ^. _3,
                  bAggregateVerifyKey = baker ^. _1 . bakerInfo . bakerAggregationVerifyKey,
                  bAggregateSecretKey = baker ^. _4
                },
          metadata = makeHeaderWithExpiry accountAddress0 4 100_000 expiry,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { payload = UpdateBakerRestakeEarnings False,
          metadata = makeHeaderWithExpiry accountAddress0 5 1_000_000 expiry,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { payload = RemoveBaker,
          metadata = makeHeaderWithExpiry accountAddress0 6 100_000 expiry,
          keys = [(0, [(0, keyPair0)])]
        }
    ]

expiryTime :: Types.TransactionExpiryTime
expiryTime = 1

slotTime :: Types.Timestamp
slotTime = Types.transactionTimeToTimestamp expiryTime

testExpiryTime ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.TransactionExpiryTime ->
    ( Types.TransactionExpiryTime ->
      [TransactionJSON]
    ) ->
    Types.SProtocolVersion pv ->
    Assertion
testExpiryTime expiry transactions _ =
    do
        let contextState =
                Helpers.defaultContextState
                    { Types._chainMetadata = dummyChainMeta{Types.slotTime = slotTime}
                    }
        let testConfig =
                Helpers.defaultTestConfig
                    { Helpers.tcContextState = contextState
                    }

        -- Run the test
        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                testConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                (transactions expiry)

        let Sch.FilteredTransactions{..} = srTransactions
        assertEqual "No unprocessed transactions." [] ftUnprocessed
        let results = Helpers.getResults ftAdded
        if expiryTime <= expiry
            then -- transactions haven't expired, so they should all succeed
            do
                assertEqual "No failed transactions." [] ftFailed
                assertBool "All added transactions succeed." $ all isSuccess results
            else -- transactions expired and they should all fail
            do
                assertEqual "No transactions added." [] results
                assertBool "All failed transactions expired." $ all isExpired ftFailed
        doBlockStateAssertions
  where
    isSuccess (_, Types.TxSuccess{}) = True
    isSuccess _ = False

    isExpired (_, Types.ExpiredTransaction) = True
    isExpired _ = False

    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockstate =
        Helpers.assertBlockStateInvariantsH blockstate (Helpers.srExecutionCosts result)
