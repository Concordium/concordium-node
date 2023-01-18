{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SchedulerTests.StakedAmountLocked (tests) where

import Control.Monad
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import Concordium.Scheduler.Types hiding (Transfer, TransferWithSchedule)
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.Accounts
import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV0) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ makeTestBakerV0FromSeed 10_000_058 10_000_000 0 0,
          Helpers.makeTestAccountFromSeed 33 1,
          Helpers.makeTestAccountFromSeed 100_000_000 2
        ]

accountAddress0 :: AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

accountAddress2 :: AccountAddress
accountAddress2 = Helpers.accountAddressFromSeed 2

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

keyPair1 :: SigScheme.KeyPair
keyPair1 = Helpers.keyPairFromSeed 1

keyPair2 :: SigScheme.KeyPair
keyPair2 = Helpers.keyPairFromSeed 2

makeTestBakerV0FromSeed ::
    Blob.MonadBlobStore m =>
    -- | The initial balance of the account.
    Amount ->
    -- | The initial staked amount of the account.
    -- Must be less than or equal to the initial balance.
    Amount ->
    -- | The baker id of the account.
    -- Must match the account index, which is the index of the account in the initial block state.
    BakerId ->
    -- | Seed used to generate account and baker keys.
    Int ->
    m (BS.PersistentAccount 'AccountV0)
makeTestBakerV0FromSeed amount stake bakerId seed = do
    account <- Helpers.makeTestAccountFromSeed amount seed
    BS.addAccountBakerV0 bakerId info account
  where
    info =
        BakerAdd
            { baKeys = bakerKeys,
              baStake = stake,
              baStakeEarnings = False
            }
    bakerKeys =
        BakerKeyUpdate
            { bkuSignKey = bakrInfo ^. bakerSignatureVerifyKey,
              bkuAggregationKey = bakrInfo ^. bakerAggregationVerifyKey,
              bkuElectionKey = bakrInfo ^. bakerElectionVerifyKey
            }
    bakrInfo :: BakerInfo
    bakrInfo =
        let (fullBakr, _, _, _) = mkFullBaker seed bakerId
        in  fullBakr ^. theBakerInfo

testCase0 ::
    forall pv.
    (IsProtocolVersion pv, AccountVersionFor pv ~ 'AccountV0) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase0 _ pvString =
    specify
        (pvString ++ ": Attempt transfer insufficient amount due to amount being stake and locked")
        $ do
            let transactions =
                    [ TJSON
                        { payload = Transfer accountAddress0 0,
                          metadata = makeDummyHeader accountAddress0 1 10_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                      TJSON
                        { payload = TransferWithSchedule accountAddress1 [(1, 10_000_000)],
                          metadata = makeDummyHeader accountAddress2 1 10_000,
                          keys = [(0, [(0, keyPair2)])]
                        },
                      TJSON
                        { payload = Transfer accountAddress1 1,
                          metadata = makeDummyHeader accountAddress1 1 10_000,
                          keys = [(0, [(0, keyPair1)])]
                        }
                    ]

            -- Run the test
            (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
                Helpers.runSchedulerTestTransactionJson
                    Helpers.defaultTestConfig
                    initialBlockState
                    (Helpers.checkReloadCheck checkState)
                    transactions

            let Sch.FilteredTransactions{..} = srTransactions
            assertEqual "There are 1 valid transactions" 1 (length ftAdded)
            assertEqual "Incorrect number of invalid transactions" 2 (length ftFailed)
            zipWithM_
                ( \(_, invalidReason) idx ->
                    assertEqual ("Incorrect failure reason for transaction " ++ show idx) Types.InsufficientFunds invalidReason
                )
                ftFailed
                [0 :: Int ..]
            doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

tests :: Spec
tests =
    describe "Insufficient available amount." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString ->
                case accountVersionFor spv of
                    SAccountV0 -> testCase0 spv pvString
                    _ -> return ()
