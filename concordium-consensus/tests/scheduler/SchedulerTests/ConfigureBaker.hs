{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for the 'ConfigureBaker' transaction.
module SchedulerTests.ConfigureBaker (tests) where

import Data.Bool.Singletons
import Lens.Micro.Platform

import qualified Concordium.Cost as Cost
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.Proofs as Proofs
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.ID.Types as ID
import Concordium.Types.Accounts

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.CooldownQueue
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import qualified Concordium.Types.DummyData as DummyData
import Concordium.Types.Option
import Data.Maybe
import qualified SchedulerTests.Helpers as Helpers
import Test.HUnit
import Test.Hspec

-- | Deterministically generate a baker account from a seed.
makeTestBakerV1FromSeed ::
    forall av m.
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
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
    -- | Whether to suspend the account initially.
    Bool ->
    m (BS.PersistentAccount av)
makeTestBakerV1FromSeed amount stake bakerId seed suspend = do
    account <- Helpers.makeTestAccountFromSeed amount seed
    let (fulBaker, _, _, _) = mkFullBaker seed bakerId
    let bakerInfoEx =
            BakerInfoExV1
                { _bieBakerInfo = fulBaker ^. theBakerInfo,
                  _bieBakerPoolInfo = poolInfo,
                  _bieIsSuspended = conditionally (sSupportsValidatorSuspension (accountVersion @av)) suspend
                }
    BS.addAccountBakerV1 bakerInfoEx stake True account
  where
    poolInfo =
        BakerPoolInfo
            { _poolOpenStatus = OpenForAll,
              _poolMetadataUrl = UrlText "Some URL",
              _poolCommissionRates =
                CommissionRates
                    { _finalizationCommission = makeAmountFraction 50_000,
                      _bakingCommission = makeAmountFraction 50_000,
                      _transactionCommission = makeAmountFraction 50_000
                    }
            }

-- | Deterministically generate a delegator account from a seed.
makeTestDelegatorFromSeed ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    -- | The initial balance of the account.
    Amount ->
    -- | The delegating details added to the account.
    AccountDelegation av ->
    -- | Seed used to generate the account.
    Int ->
    m (BS.PersistentAccount av)
makeTestDelegatorFromSeed amount accountDelegation seed = do
    account <- Helpers.makeTestAccountFromSeed amount seed
    BS.addAccountDelegator accountDelegation account

-- Accounts

-- | Account of the baker 0.
baker0Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker0Account = makeTestBakerV1FromSeed 1_000_000_000_000 1_000_000_000_000 bakerId seed False
  where
    bakerId = 0
    seed = 16

-- | Account of the delegator1.
delegator1Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
delegator1Account = makeTestDelegatorFromSeed 20_000_000_000_000 accountDelegation 17
  where
    accountDelegation =
        AccountDelegationV1
            { _delegationIdentity = 1,
              _delegationStakedAmount = 19_000_000_000_000, -- leverage cap is set to 5 in createBlockState, so this puts it over the cap.
              _delegationStakeEarnings = False,
              _delegationTarget = DelegateToBaker 0,
              _delegationPendingChange = NoChange
            }

-- | Account address of the delegator1.
delegator1Address :: AccountAddress
delegator1Address = Helpers.accountAddressFromSeed 17

-- | Account keys of the delegator1 account.
delegator1KP :: SigScheme.KeyPair
delegator1KP = Helpers.keyPairFromSeed 17

-- | Account of the baker 2.
baker2Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker2Account = makeTestBakerV1FromSeed balance stake bakerId seed False
  where
    balance = 1_000_000_000_000
    stake = 1_000_000_000
    bakerId = 2
    seed = 18

-- | An account with no staking.
dummy3Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
dummy3Account = Helpers.makeTestAccountFromSeed 20_000_000_000_000 19

-- | Address of the dummy3 account.
dummy3Address :: AccountAddress
dummy3Address = Helpers.accountAddressFromSeed 19

-- | Keys of the dummy3 account.
dummy3KP :: SigScheme.KeyPair
dummy3KP = Helpers.keyPairFromSeed 19

-- | Account of the baker 4.
baker4Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker4Account = makeTestBakerV1FromSeed 20_000_000_000_000 500_000_000_000 bakerId seed False
  where
    bakerId = 4
    seed = 20

-- | Account of the baker 5. This account is suspended.
baker5Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker5Account = makeTestBakerV1FromSeed 20_000_000_000_000 500_000_000_000 bakerId seed True
  where
    bakerId = 4
    seed = 20

-- | Account address of the delegator3.
baker4Address :: AccountAddress
baker4Address = Helpers.accountAddressFromSeed 20

-- | Account keys of the delegator3 account.
baker4KP :: SigScheme.KeyPair
baker4KP = Helpers.keyPairFromSeed 20

-- | Create initial block state with account
--  account index 0 is baker0
--  account index 1 is delegator 1 (delegates to baker 0 with overdelegation)
--  account index 2 is baker 2
--  account index 4 is baker 4
initialBlockState2 ::
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState2 =
    Helpers.createTestBlockStateWithAccountsM
        [ baker0Account,
          delegator1Account,
          baker2Account,
          dummy3Account,
          baker4Account
        ]

-- | Construct a (valid) 'BakerKeysWithProofs' from the given account address for keys generated
--  with the given seed.
makeBakerKeysWithProofs :: AccountAddress -> Int -> IO BakerKeysWithProofs
makeBakerKeysWithProofs senderAddress seed = do
    bkwpProofElection <- fromJust <$> Proofs.proveDlog25519VRF challenge kpElection
    bkwpProofSig <- fromJust <$> Proofs.proveDlog25519Block challenge kpSignature
    bkwpProofAggregation <- Bls.proveKnowledgeOfSK challenge skAggregate
    return BakerKeysWithProofs{..}
  where
    (fulBaker, skElection, skSignature, skAggregate) = mkFullBaker seed 0
    bkwpElectionVerifyKey = fulBaker ^. theBakerInfo . bakerElectionVerifyKey
    bkwpSignatureVerifyKey = fulBaker ^. theBakerInfo . bakerSignatureVerifyKey
    bkwpAggregationVerifyKey = fulBaker ^. theBakerInfo . bakerAggregationVerifyKey
    kpElection = VRF.KeyPair skElection bkwpElectionVerifyKey
    kpSignature = Sig.KeyPair skSignature bkwpSignatureVerifyKey
    challenge = Types.configureBakerKeyChallenge senderAddress bkwpElectionVerifyKey bkwpSignatureVerifyKey bkwpAggregationVerifyKey

-- | Transition delegator 1 to baker 1.
testDelegatorToBakerOk ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testDelegatorToBakerOk spv pvString =
    specify (pvString ++ ": Delegator -> Baker (OK)") $ do
        keysWithProofs <- makeBakerKeysWithProofs delegator1Address 1
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader delegator1Address 1 10_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck (checkState keysWithProofs))
                transactions
        () <- case sSupportsFlexibleCooldown (sAccountVersionFor spv) of
            SFalse -> Helpers.assertRejectWithReason AlreadyADelegator result
            STrue -> Helpers.assertSuccessWithEvents (events keysWithProofs) result
        doBlockStateAssertions
  where
    stakeAmount = 300_000_000_000
    events :: BakerKeysWithProofs -> [Event]
    events keysWithProofs =
        [ DelegationRemoved{edrDelegatorId = 1, edrAccount = delegator1Address},
          BakerAdded
            { ebaBakerId = 1,
              ebaAccount = delegator1Address,
              ebaSignKey = bkwpSignatureVerifyKey keysWithProofs,
              ebaElectionKey = bkwpElectionVerifyKey keysWithProofs,
              ebaAggregationKey = bkwpAggregationVerifyKey keysWithProofs,
              ebaStake = stakeAmount,
              ebaRestakeEarnings = False -- Inherited from the delegator
            },
          BakerSetRestakeEarnings
            { ebsreBakerId = 1,
              ebsreAccount = delegator1Address,
              ebsreRestakeEarnings = False
            },
          BakerSetOpenStatus
            { ebsosBakerId = 1,
              ebsosAccount = delegator1Address,
              ebsosOpenStatus = OpenForAll
            },
          BakerSetMetadataURL
            { ebsmuBakerId = 1,
              ebsmuAccount = delegator1Address,
              ebsmuMetadataURL = emptyUrlText
            },
          BakerSetTransactionFeeCommission
            { ebstfcBakerId = 1,
              ebstfcAccount = delegator1Address,
              ebstfcTransactionFeeCommission = makeAmountFraction 1_000
            },
          BakerSetBakingRewardCommission
            { ebsbrcBakerId = 1,
              ebsbrcAccount = delegator1Address,
              ebsbrcBakingRewardCommission = makeAmountFraction 1_000
            },
          BakerSetFinalizationRewardCommission
            { ebsfrcBakerId = 1,
              ebsfrcAccount = delegator1Address,
              ebsfrcFinalizationRewardCommission = makeAmountFraction 1_000
            }
        ]
    -- Transaction length is 438 bytes (378 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 438 1
    checkState ::
        BakerKeysWithProofs ->
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState keysWithProofs result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 1
        initialAccount1 <- BS.toTransientAccount =<< delegator1Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                    & updateStaking keysWithProofs
                )
                updatedAccount1
    updatedBakerInfo :: BakerKeysWithProofs -> BakerInfoEx (AccountVersionFor pv)
    updatedBakerInfo keysWithProofs =
        BakerInfoExV1
            { _bieBakerInfo =
                BakerInfo
                    { _bakerSignatureVerifyKey = bkwpSignatureVerifyKey keysWithProofs,
                      _bakerElectionVerifyKey = bkwpElectionVerifyKey keysWithProofs,
                      _bakerAggregationVerifyKey = bkwpAggregationVerifyKey keysWithProofs,
                      _bakerIdentity = 1
                    },
              _bieBakerPoolInfo =
                BakerPoolInfo
                    { _poolOpenStatus = OpenForAll,
                      _poolMetadataUrl = emptyUrlText,
                      _poolCommissionRates =
                        CommissionRates
                            { _finalizationCommission = makeAmountFraction 1_000,
                              _bakingCommission = makeAmountFraction 1_000,
                              _transactionCommission = makeAmountFraction 1_000
                            }
                    },
              _bieIsSuspended = conditionally (sSupportsValidatorSuspension (sAccountVersionFor spv)) False
            }
    updateStaking keysWithProofs = case sSupportsFlexibleCooldown (sAccountVersionFor spv) of
        SFalse -> id
        STrue ->
            ( Transient.accountStaking
                .~ AccountStakeBaker
                    ( AccountBaker
                        { _stakedAmount = stakeAmount,
                          _stakeEarnings = False,
                          _accountBakerInfo = updatedBakerInfo keysWithProofs,
                          _bakerPendingChange = NoChange
                        }
                    )
            )
                . ( Transient.accountStakeCooldown
                        .~ CTrue (emptyCooldowns{prePreCooldown = Present 18_700_000_000_000})
                  )

-- | Transition delegator 1 to baker 1 using a duplicate aggregation key.
testDelegatorToBakerDuplicateKey ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testDelegatorToBakerDuplicateKey spv pvString =
    specify (pvString ++ ": Delegator -> Baker (Duplicate Aggregation Key)") $ do
        -- Reuse the same keys that baker 0 uses.
        keysWithProofs <- makeBakerKeysWithProofs delegator1Address 16
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader delegator1Address 1 10_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        () <- case sSupportsFlexibleCooldown (sAccountVersionFor spv) of
            SFalse -> Helpers.assertRejectWithReason AlreadyADelegator result
            STrue ->
                Helpers.assertRejectWithReason
                    (DuplicateAggregationKey (bkwpAggregationVerifyKey keysWithProofs))
                    result
        doBlockStateAssertions
  where
    stakeAmount = 300_000_000_000
    -- Transaction length is 438 bytes (378 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 438 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 1
        initialAccount1 <- BS.toTransientAccount =<< delegator1Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Transition delegator 1 to baker 1 using a duplicate aggregation key.
testDelegatorToBakerMissingParam ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testDelegatorToBakerMissingParam spv pvString =
    specify (pvString ++ ": Delegator -> Baker (Missing Parameter)") $ do
        -- Reuse the same keys that baker 0 uses.
        keysWithProofs <- makeBakerKeysWithProofs delegator1Address 16
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Nothing,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader delegator1Address 1 10_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        () <- case sSupportsFlexibleCooldown (sAccountVersionFor spv) of
            SFalse -> Helpers.assertRejectWithReason AlreadyADelegator result
            STrue -> Helpers.assertRejectWithReason MissingBakerAddParameters result
        doBlockStateAssertions
  where
    stakeAmount = 300_000_000_000
    -- Transaction length is 437 bytes (378 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 437 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 1
        initialAccount1 <- BS.toTransientAccount =<< delegator1Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Test adding a baker successfully.
testAddBakerOk ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testAddBakerOk spv pvString =
    specify (pvString ++ ": AddBaker (OK)") $ do
        keysWithProofs <- makeBakerKeysWithProofs dummy3Address 3
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader dummy3Address 1 transactionEnergy,
                      keys = [(0, [(0, dummy3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck (checkState keysWithProofs))
                transactions
        Helpers.assertSuccessWithEvents (events keysWithProofs) result
        doBlockStateAssertions
  where
    -- This stake is the maximal amount at which this will succeed.
    stakeAmount = 20_000_000_000_000 - Helpers.energyToAmount transactionEnergy
    events keysWithProofs =
        [ BakerAdded
            { ebaBakerId = 3,
              ebaAccount = dummy3Address,
              ebaSignKey = bkwpSignatureVerifyKey keysWithProofs,
              ebaElectionKey = bkwpElectionVerifyKey keysWithProofs,
              ebaAggregationKey = bkwpAggregationVerifyKey keysWithProofs,
              ebaStake = stakeAmount,
              ebaRestakeEarnings = True
            },
          BakerSetRestakeEarnings
            { ebsreBakerId = 3,
              ebsreAccount = dummy3Address,
              ebsreRestakeEarnings = True
            },
          BakerSetOpenStatus
            { ebsosBakerId = 3,
              ebsosAccount = dummy3Address,
              ebsosOpenStatus = OpenForAll
            },
          BakerSetMetadataURL
            { ebsmuBakerId = 3,
              ebsmuAccount = dummy3Address,
              ebsmuMetadataURL = emptyUrlText
            },
          BakerSetTransactionFeeCommission
            { ebstfcBakerId = 3,
              ebstfcAccount = dummy3Address,
              ebstfcTransactionFeeCommission = makeAmountFraction 1_000
            },
          BakerSetBakingRewardCommission
            { ebsbrcBakerId = 3,
              ebsbrcAccount = dummy3Address,
              ebsbrcBakingRewardCommission = makeAmountFraction 1_000
            },
          BakerSetFinalizationRewardCommission
            { ebsfrcBakerId = 3,
              ebsfrcAccount = dummy3Address,
              ebsfrcFinalizationRewardCommission = makeAmountFraction 1_000
            }
        ]
    -- Transaction length is 438 bytes (379 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 439 1
    checkState ::
        BakerKeysWithProofs ->
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState keysWithProofs result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 3
        initialAccount1 <- BS.toTransientAccount =<< dummy3Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                    & updateStaking keysWithProofs
                )
                updatedAccount1
    updatedBakerInfo :: BakerKeysWithProofs -> BakerInfoEx (AccountVersionFor pv)
    updatedBakerInfo keysWithProofs =
        BakerInfoExV1
            { _bieBakerInfo =
                BakerInfo
                    { _bakerSignatureVerifyKey = bkwpSignatureVerifyKey keysWithProofs,
                      _bakerElectionVerifyKey = bkwpElectionVerifyKey keysWithProofs,
                      _bakerAggregationVerifyKey = bkwpAggregationVerifyKey keysWithProofs,
                      _bakerIdentity = 3
                    },
              _bieBakerPoolInfo =
                BakerPoolInfo
                    { _poolOpenStatus = OpenForAll,
                      _poolMetadataUrl = emptyUrlText,
                      _poolCommissionRates =
                        CommissionRates
                            { _finalizationCommission = makeAmountFraction 1_000,
                              _bakingCommission = makeAmountFraction 1_000,
                              _transactionCommission = makeAmountFraction 1_000
                            }
                    },
              _bieIsSuspended = conditionally (sSupportsValidatorSuspension (sAccountVersionFor spv)) False
            }
    updateStaking keysWithProofs =
        ( Transient.accountStaking
            .~ AccountStakeBaker
                ( AccountBaker
                    { _stakedAmount = stakeAmount,
                      _stakeEarnings = True,
                      _accountBakerInfo = updatedBakerInfo keysWithProofs,
                      _bakerPendingChange = NoChange
                    }
                )
        )

-- | Test that adding a baker with insufficient balance is rejected.
testAddBakerInsufficientBalance ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testAddBakerInsufficientBalance _spv pvString =
    specify (pvString ++ ": AddBaker (InsufficientBalance)") $ do
        keysWithProofs <- makeBakerKeysWithProofs dummy3Address 3
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader dummy3Address 1 transactionEnergy,
                      keys = [(0, [(0, dummy3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason InsufficientBalanceForBakerStake result
        doBlockStateAssertions
  where
    -- This stake is the minimal amount at which this will fail.
    stakeAmount = 20_000_000_000_000 - Helpers.energyToAmount transactionEnergy + 1
    -- Transaction length is 438 bytes (379 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 439 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 3
        initialAccount1 <- BS.toTransientAccount =<< dummy3Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Test that adding a baker with incomplete parameters is rejected.
testAddBakerMissingParam ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testAddBakerMissingParam _spv pvString =
    specify (pvString ++ ": AddBaker (Missing Parameter)") $ do
        keysWithProofs <- makeBakerKeysWithProofs dummy3Address 3
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Nothing,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader dummy3Address 1 transactionEnergy,
                      keys = [(0, [(0, dummy3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason MissingBakerAddParameters result
        doBlockStateAssertions
  where
    -- This stake is the minimal amount at which this will fail.
    stakeAmount = 20_000_000_000_000 - Helpers.energyToAmount transactionEnergy + 1
    -- Transaction length is 437 bytes (377 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 437 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 3
        initialAccount1 <- BS.toTransientAccount =<< dummy3Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Test that adding a baker with invalid proofs is rejected.
testAddBakerInvalidProofs ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testAddBakerInvalidProofs _spv pvString =
    specify (pvString ++ ": AddBaker (InvalidProofs)") $ do
        keysWithProofsOk <- makeBakerKeysWithProofs dummy3Address 3
        let keysWithProofs = keysWithProofsOk{bkwpSignatureVerifyKey = Sig.verifyKey $ DummyData.bakerSignKey 0}
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader dummy3Address 1 transactionEnergy,
                      keys = [(0, [(0, dummy3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason InvalidProof result
        doBlockStateAssertions
  where
    stakeAmount = 20_000_000_000_000 - Helpers.energyToAmount transactionEnergy
    -- Transaction length is 438 bytes (379 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 439 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 3
        initialAccount1 <- BS.toTransientAccount =<< dummy3Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Test updating an existing baker successfully.
testUpdateBakerOk ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testUpdateBakerOk _spv pvString =
    specify (pvString ++ ": UpdateBaker (OK)") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Nothing,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Nothing,
                              cbFinalizationRewardCommission = Nothing,
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertSuccessWithEvents events result
        doBlockStateAssertions
  where
    stakeAmount = 10_000_000_000_000 - Helpers.energyToAmount transactionEnergy
    events =
        [ BakerSetRestakeEarnings
            { ebsreBakerId = 4,
              ebsreAccount = baker4Address,
              ebsreRestakeEarnings = True
            },
          BakerSetOpenStatus
            { ebsosBakerId = 4,
              ebsosAccount = baker4Address,
              ebsosOpenStatus = OpenForAll
            },
          BakerSetMetadataURL
            { ebsmuBakerId = 4,
              ebsmuAccount = baker4Address,
              ebsmuMetadataURL = emptyUrlText
            },
          BakerSetTransactionFeeCommission
            { ebstfcBakerId = 4,
              ebstfcAccount = baker4Address,
              ebstfcTransactionFeeCommission = makeAmountFraction 1_000
            },
          BakerStakeIncreased
            { ebsiBakerId = 4,
              ebsiAccount = baker4Address,
              ebsiNewStake = stakeAmount
            }
        ]
    -- Transaction length is 79 bytes (19 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithoutKeys + Cost.baseCost 79 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< baker4Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                    & updateStaking
                )
                updatedAccount1
    updateStaking =
        ( Transient.accountStaking
            . accountBaker
            %~ (stakedAmount .~ stakeAmount)
                . (stakeEarnings .~ True)
                . ( accountBakerInfo . bieBakerPoolInfo
                        %~ (poolCommissionRates . transactionCommission .~ makeAmountFraction 1_000)
                            . (poolMetadataUrl .~ emptyUrlText)
                  )
        )
    accountBaker f (AccountStakeBaker b) = AccountStakeBaker <$> f b
    accountBaker _ x = pure x

-- | Test that configuring a baker with capital this is above its balance is rejected.
testUpdateBakerInsufficientBalance ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testUpdateBakerInsufficientBalance _spv pvString =
    specify (pvString ++ ": UpdateBaker (Insufficient Balance)") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Nothing,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Nothing,
                              cbFinalizationRewardCommission = Nothing,
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason InsufficientBalanceForBakerStake result
        doBlockStateAssertions
  where
    stakeAmount = 100_000_000_000_000 - Helpers.energyToAmount transactionEnergy
    -- Transaction length is 79 bytes (19 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithoutKeys + Cost.baseCost 79 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< baker4Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Test that configuring a baker with capital this is above its balance is rejected.
testUpdateBakerLowStake ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testUpdateBakerLowStake _spv pvString =
    specify (pvString ++ ": UpdateBaker (StakeUnderThreshold)") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Just True,
                              cbOpenForDelegation = Just OpenForAll,
                              cbKeysWithProofs = Nothing,
                              cbMetadataURL = Just emptyUrlText,
                              cbTransactionFeeCommission = Just (makeAmountFraction 1_000),
                              cbBakingRewardCommission = Nothing,
                              cbFinalizationRewardCommission = Nothing,
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason StakeUnderMinimumThresholdForBaking result
        doBlockStateAssertions
  where
    stakeAmount = 100_000
    -- Transaction length is 79 bytes (19 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithoutKeys + Cost.baseCost 79 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< baker4Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

-- | Test that configuring a baker with invalid proofs is rejected.
testUpdateBakerInvalidProofs ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testUpdateBakerInvalidProofs _spv pvString =
    specify (pvString ++ ": UpdateBaker (InvalidProofs)") $ do
        -- Generate keys with challenge for a different account.
        keysWithProofs <- makeBakerKeysWithProofs dummy3Address 912
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Nothing,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Nothing,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Nothing,
                              cbTransactionFeeCommission = Nothing,
                              cbBakingRewardCommission = Nothing,
                              cbFinalizationRewardCommission = Nothing,
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason InvalidProof result
        doBlockStateAssertions
  where
    -- Transaction length is 415 bytes (355 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 415 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< baker4Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                )
                updatedAccount1

testUpdateBakerRemoveOk ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testUpdateBakerRemoveOk spv pvString =
    specify (pvString ++ ": RemoveBaker (OK)") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just 0,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Nothing,
                              cbKeysWithProofs = Nothing,
                              cbMetadataURL = Nothing,
                              cbTransactionFeeCommission = Nothing,
                              cbBakingRewardCommission = Nothing,
                              cbFinalizationRewardCommission = Nothing,
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertSuccessWithEvents events result
        doBlockStateAssertions
  where
    events =
        [ BakerRemoved
            { ebrBakerId = 4,
              ebrAccount = baker4Address
            }
        ]
    -- Transaction length is 71 bytes (11 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithoutKeys + Cost.baseCost 71 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< baker4Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                    & updateStaking
                )
                updatedAccount1
    updateStaking = case sSupportsFlexibleCooldown (sAccountVersionFor spv) of
        SFalse ->
            Transient.accountStaking . accountBaker . bakerPendingChange
                .~ RemoveStake (PendingChangeEffectiveV1 86400000)
        STrue ->
            (Transient.accountStaking .~ AccountStakeNone)
                . (Transient.accountStakeCooldown . unconditionally .~ emptyCooldowns{prePreCooldown = Present 500_000_000_000})
    accountBaker f (AccountStakeBaker b) = AccountStakeBaker <$> f b
    accountBaker _ x = pure x

testUpdateBakerReduceStakeOk ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testUpdateBakerReduceStakeOk spv pvString =
    specify (pvString ++ ": UpdateBaker: ReduceStake (OK)") $ do
        keysWithProofs <- makeBakerKeysWithProofs baker4Address 4000
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Just stakeAmount,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Nothing,
                              cbKeysWithProofs = Just keysWithProofs,
                              cbMetadataURL = Nothing,
                              cbTransactionFeeCommission = Nothing,
                              cbBakingRewardCommission = Just (makeAmountFraction 1_000),
                              cbFinalizationRewardCommission = Just (makeAmountFraction 1_000),
                              cbSuspend = Nothing
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck (checkState keysWithProofs))
                transactions
        Helpers.assertSuccessWithEvents (events keysWithProofs) result
        doBlockStateAssertions
  where
    stakeAmount = 300_000_000_000 -- Minimum stake amount
    events keysWithProofs =
        [ BakerKeysUpdated
            { ebkuBakerId = 4,
              ebkuAccount = baker4Address,
              ebkuSignKey = bkwpSignatureVerifyKey keysWithProofs,
              ebkuElectionKey = bkwpElectionVerifyKey keysWithProofs,
              ebkuAggregationKey = bkwpAggregationVerifyKey keysWithProofs
            },
          BakerSetBakingRewardCommission
            { ebsbrcBakerId = 4,
              ebsbrcAccount = baker4Address,
              ebsbrcBakingRewardCommission = makeAmountFraction 1_000
            },
          BakerSetFinalizationRewardCommission
            { ebsfrcBakerId = 4,
              ebsfrcAccount = baker4Address,
              ebsfrcFinalizationRewardCommission = makeAmountFraction 1_000
            },
          BakerStakeDecreased
            { ebsiBakerId = 4,
              ebsiAccount = baker4Address,
              ebsiNewStake = stakeAmount
            }
        ]
    -- Transaction length is 431 bytes (371 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithKeys + Cost.baseCost 431 1
    checkState ::
        BakerKeysWithProofs ->
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState keysWithProofs result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< baker4Account @(AccountVersionFor pv)
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                    & updateStaking keysWithProofs
                )
                updatedAccount1
    updateStaking keysWithProofs =
        ( case sSupportsFlexibleCooldown (sAccountVersionFor spv) of
            SFalse ->
                Transient.accountStaking . accountBaker
                    %~ (bakerPendingChange .~ ReduceStake stakeAmount (PendingChangeEffectiveV1 86400000))
            STrue ->
                (Transient.accountStakeCooldown . unconditionally .~ emptyCooldowns{prePreCooldown = Present 200_000_000_000})
                    . (Transient.accountStaking . accountBaker . stakedAmount .~ stakeAmount)
        )
            . ( Transient.accountStaking . accountBaker . accountBakerInfo
                    %~ (poolCommissionRates . bakingCommission .~ makeAmountFraction 1_000)
                        . (poolCommissionRates . finalizationCommission .~ makeAmountFraction 1_000)
                        . (bakerElectionVerifyKey .~ bkwpElectionVerifyKey keysWithProofs)
                        . (bakerSignatureVerifyKey .~ bkwpSignatureVerifyKey keysWithProofs)
                        . (bakerAggregationVerifyKey .~ bkwpAggregationVerifyKey keysWithProofs)
              )
    accountBaker f (AccountStakeBaker b) = AccountStakeBaker <$> f b
    accountBaker _ x = pure x

data TestSuspendOrResume
    = Suspend
    | Resume
    deriving (Show, Eq)

testUpdateBakerSuspendResumeOk ::
    forall m pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv, m ~ Helpers.PersistentBSM pv) =>
    SProtocolVersion pv ->
    String ->
    TestSuspendOrResume ->
    m (BS.PersistentAccount (AccountVersionFor pv)) ->
    Spec
testUpdateBakerSuspendResumeOk spv pvString suspendOrResume accM =
    specify (pvString ++ ": UpdateBaker: " ++ show suspendOrResume ++ " (OK)") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureBaker
                            { cbCapital = Nothing,
                              cbRestakeEarnings = Nothing,
                              cbOpenForDelegation = Nothing,
                              cbKeysWithProofs = Nothing,
                              cbMetadataURL = Nothing,
                              cbTransactionFeeCommission = Nothing,
                              cbBakingRewardCommission = Nothing,
                              cbFinalizationRewardCommission = Nothing,
                              cbSuspend = Just $ suspendOrResume == Suspend
                            },
                      metadata = makeDummyHeader baker4Address 1 transactionEnergy,
                      keys = [(0, [(0, baker4KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState2 @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertSuccessWithEvents events result
        doBlockStateAssertions
  where
    -- Transaction length is 64 bytes (4 bytes for the transaction and 60 bytes for the header).
    transactionEnergy = Cost.configureBakerCostWithoutKeys + Cost.baseCost 64 1
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        invariants <- Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)
        updatedAccount1 <- BS.toTransientAccount . fromJust =<< BS.bsoGetAccountByIndex blockState 4
        initialAccount1 <- BS.toTransientAccount =<< accM
        return $ do
            invariants
            assertEqual
                "Expected account update"
                ( initialAccount1
                    & Transient.accountNonce .~ 2
                    & Transient.accountAmount -~ Helpers.energyToAmount transactionEnergy
                    & updateResumed
                )
                updatedAccount1
    updateResumed = case sSupportsValidatorSuspension (sAccountVersionFor spv) of
        STrue ->
            Transient.accountStaking
                . accountBaker
                . accountBakerInfo
                . bieIsSuspended
                .~ (suspendOrResume == Suspend)
        SFalse -> id
    accountBaker f (AccountStakeBaker b) = AccountStakeBaker <$> f b
    accountBaker _ x = pure x
    events =
        [ if suspendOrResume == Suspend
            then BakerSuspended{ebsBakerId = 4, ebsAccount = baker4Address}
            else BakerResumed{ebrBakerId = 4, ebrAccount = baker4Address}
        ]

tests :: Spec
tests =
    describe "ConfigureBaker transactions" $
        sequence_ $
            Helpers.forEveryProtocolVersion testCases
  where
    testCases :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> String -> Spec
    testCases spv pvString = do
        case delegationSupport @(AccountVersionFor pv) of
            SAVDelegationNotSupported -> return ()
            SAVDelegationSupported -> do
                testDelegatorToBakerOk spv pvString
                testDelegatorToBakerDuplicateKey spv pvString
                testDelegatorToBakerMissingParam spv pvString
                testAddBakerOk spv pvString
                testAddBakerInsufficientBalance spv pvString
                testAddBakerMissingParam spv pvString
                testAddBakerInvalidProofs spv pvString
                testUpdateBakerOk spv pvString
                testUpdateBakerInsufficientBalance spv pvString
                testUpdateBakerLowStake spv pvString
                testUpdateBakerInvalidProofs spv pvString
                testUpdateBakerRemoveOk spv pvString
                testUpdateBakerReduceStakeOk spv pvString
                case sSupportsValidatorSuspension (sAccountVersionFor spv) of
                    STrue -> do
                        testUpdateBakerSuspendResumeOk spv pvString Suspend (baker4Account @(AccountVersionFor pv))
                        testUpdateBakerSuspendResumeOk spv pvString Resume (baker5Account @(AccountVersionFor pv))
                    SFalse -> return ()
