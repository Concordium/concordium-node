{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.ConfigureValidator where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer.CPS
import Data.Bool.Singletons
import qualified Data.Map.Strict as Map
import Data.Maybe
import Lens.Micro.Platform
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.Types.Accounts
import qualified Concordium.Types.DummyData as DummyData
import Concordium.Types.Execution
import Concordium.Types.Option
import Concordium.Types.Parameters

import Concordium.GlobalState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CooldownQueue
import qualified Concordium.GlobalState.DummyData as DummyData
import Concordium.GlobalState.Persistent.BlockState

import GlobalStateTests.BlockStateHelpers

-- | Test accounts used for testing the 'bsoAddValidator' function.
--  The first account is a regular account, the second account is a baker account.
addValidatorTestAccounts ::
    -- | Whether the first account should have cooldowns.
    Bool ->
    [AccountConfig pv]
addValidatorTestAccounts withCooldowns =
    [ AccountConfig
        { acAccountIndex = 0,
          acAmount = 1_000_000_000_000,
          acStaking = StakeDetailsNone,
          acPoolInfo = Nothing,
          acCooldowns = if withCooldowns then initialTestCooldowns else emptyCooldowns
        },
      AccountConfig
        { acAccountIndex = 1,
          acAmount = 1_000_000_000_000,
          acStaking =
            StakeDetailsBaker
                { sdStakedCapital = 500_000_000_000,
                  sdRestakeEarnings = True,
                  sdPendingChange = NoChange
                },
          acPoolInfo = Nothing,
          acCooldowns = emptyCooldowns
        }
    ]

-- | Conditions that can trigger a specific error in 'bsoAddValidator' or 'bsoUpdateValidator'.
data ValidatorConditions = ValidatorConditions
    { vcUnderThreshold :: Bool,
      vcTransactionFeeNotInRange :: Bool,
      vcBakingRewardNotInRange :: Bool,
      vcFinalizationRewardNotInRange :: Bool,
      vcAggregationKeyDuplicate :: Bool
    }
    deriving (Show)

-- | All possible 'ValidatorConditions' configurations.
validatorConditions :: [ValidatorConditions]
validatorConditions = do
    vcUnderThreshold <- [True, False]
    vcTransactionFeeNotInRange <- [True, False]
    vcBakingRewardNotInRange <- [True, False]
    vcFinalizationRewardNotInRange <- [True, False]
    vcAggregationKeyDuplicate <- [True, False]
    return ValidatorConditions{..}

-- | Derive a 'BakerKeyUpdate' from a seed.
makeBakerKeyUpdate :: Int -> BakerKeyUpdate
makeBakerKeyUpdate seed =
    BakerKeyUpdate
        { bkuSignKey = Sig.verifyKey $ DummyData.bakerSignKey seed,
          bkuAggregationKey = Bls.derivePublicKey $ DummyData.bakerAggregationKey seed,
          bkuElectionKey = VRF.publicKey $ DummyData.bakerElectionKey seed
        }

-- | Test 'bsoAddValidator' in a variety of cases that exercise the different error conditions,
--   and ensure that the behaviour is as expected (including on success).
testAddValidatorAllCases ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    Spec
testAddValidatorAllCases spv = describe "bsoAddValidator" $ do
    forM_ validatorConditions $ \vc -> do
        it (show vc) $ runTest False False vc
        when supportCooldown $ it (show vc <> " with cooldown") $ runTest True False vc
        when supportSuspension $ it (show vc <> " with suspended validator") $ runTest True True vc
  where
    supportCooldown = supportsFlexibleCooldown $ accountVersionFor $ demoteProtocolVersion (protocolVersion @pv)
    supportSuspension = supportsValidatorSuspension $ accountVersionFor $ demoteProtocolVersion (protocolVersion @pv)
    minEquity = 1_000_000_000
    chainParams =
        DummyData.dummyChainParameters @(ChainParametersVersionFor pv)
            & cpPoolParameters . ppMinimumEquityCapital .~ minEquity
            & cpPoolParameters . ppCommissionBounds
                .~ CommissionRanges
                    { _transactionCommissionRange = InclusiveRange (makeAmountFraction 100) (makeAmountFraction 200),
                      _finalizationCommissionRange = InclusiveRange (makeAmountFraction 300) (makeAmountFraction 400),
                      _bakingCommissionRange = InclusiveRange (makeAmountFraction 500) (makeAmountFraction 600)
                    }
    mkInitialState accounts =
        hpbsPointers
            <$> initialPersistentState @pv
                (dummySeedState (protocolVersion @pv))
                DummyData.dummyCryptographicParameters
                accounts
                DummyData.dummyIdentityProviders
                DummyData.dummyArs
                (withIsAuthorizationsVersionForPV spv DummyData.dummyKeyCollection)
                chainParams
    runTest withCooldown suspended ValidatorConditions{..} = runTestBlockState @pv $ do
        let va =
                ValidatorAdd
                    { vaKeys = if vcAggregationKeyDuplicate then badKeys else goodKeys,
                      vaCapital = if vcUnderThreshold then minEquity - 1 else minEquity,
                      vaRestakeEarnings = True,
                      vaOpenForDelegation = OpenForAll,
                      vaMetadataURL = UrlText "Some URL",
                      vaCommissionRates =
                        CommissionRates
                            { _finalizationCommission = makeAmountFraction $ if vcFinalizationRewardNotInRange then 100 else 300,
                              _bakingCommission = makeAmountFraction $ if vcBakingRewardNotInRange then 100 else 500,
                              _transactionCommission = makeAmountFraction $ if vcTransactionFeeNotInRange then 300 else 100
                            },
                      vaSuspended = suspended
                    }
        initialAccounts <- mapM makeDummyAccount (addValidatorTestAccounts withCooldown)
        initialBS <- mkInitialState initialAccounts
        res <- bsoAddValidator initialBS 0 va
        let expect
                | vcUnderThreshold = Left VCFStakeUnderThreshold
                | vcTransactionFeeNotInRange = Left VCFTransactionFeeCommissionNotInRange
                | vcBakingRewardNotInRange = Left VCFBakingRewardCommissionNotInRange
                | vcFinalizationRewardNotInRange = Left VCFFinalizationRewardCommissionNotInRange
                | vcAggregationKeyDuplicate = Left (VCFDuplicateAggregationKey (bkuAggregationKey (vaKeys va)))
                | otherwise = Right ()
        liftIO $ void res `shouldBe` expect
        forM_ res $ \bs -> do
            -- Check the active bakers are correct
            checkActiveBakers bs
            () <- case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)) of
                STrue -> do
                    -- Check that the cooldowns are correct
                    newCooldowns <- checkCooldowns bs
                    let bakerInitialCooldowns
                            | withCooldown = initialTestCooldowns
                            | otherwise = emptyCooldowns
                    let bakerExpectedCooldowns = reactivateCooldownAmount (vaCapital va) bakerInitialCooldowns
                    liftIO $ newCooldowns `shouldBe` [bakerExpectedCooldowns, emptyCooldowns]
                SFalse -> return ()
            acc <- bsoGetAccountByIndex bs 0
            let expectedBaker =
                    AccountBaker
                        { _stakedAmount = vaCapital va,
                          _stakeEarnings = vaRestakeEarnings va,
                          _bakerPendingChange = NoChange,
                          _accountBakerInfo =
                            BakerInfoExV1
                                { _bieBakerInfo =
                                    BakerInfo
                                        { _bakerSignatureVerifyKey = bkuSignKey (vaKeys va),
                                          _bakerElectionVerifyKey = bkuElectionKey (vaKeys va),
                                          _bakerAggregationVerifyKey = bkuAggregationKey (vaKeys va),
                                          _bakerIdentity = BakerId 0
                                        },
                                  _bieBakerPoolInfo =
                                    BakerPoolInfo
                                        { _poolOpenStatus = vaOpenForDelegation va,
                                          _poolMetadataUrl = vaMetadataURL va,
                                          _poolCommissionRates = vaCommissionRates va
                                        },
                                  _bieIsSuspended = conditionally (sSupportsValidatorSuspension (accountVersion @(AccountVersionFor pv))) suspended
                                }
                        }
            bkr <- getAccountBaker (fromJust acc)
            liftIO $ bkr `shouldBe` Just expectedBaker
            return ()
    goodKeys = makeBakerKeyUpdate 0
    badKeys = makeBakerKeyUpdate 1

-- | The initial stake amount for the test accounts.
initialStakedAmount :: Amount
initialStakedAmount = 500_000_000_000

-- | Some non-trivial cooldowns that may be set on an account for testing.
initialTestCooldowns :: Cooldowns
initialTestCooldowns =
    Cooldowns
        { inCooldown = Map.fromList [(1000, 100_000_000_000), (2000, 100_000_000_000)],
          prePreCooldown = Present 2000,
          preCooldown = Present 8000
        }

-- | Test account set up for 'bsoUpdateValidator'. The first two accounts are validators, and the
--  third is delegating to the first.
updateValidatorTestAccounts ::
    forall av.
    (IsAccountVersion av, AVSupportsDelegation av) =>
    Bool ->
    [AccountConfig av]
updateValidatorTestAccounts pendingChangeOrCooldown =
    [ AccountConfig
        { acAccountIndex = 0,
          acAmount = 1_000_000_000_000,
          acStaking =
            StakeDetailsBaker
                { sdStakedCapital = initialStakedAmount,
                  sdRestakeEarnings = True,
                  sdPendingChange = pendingChange
                },
          acPoolInfo = Nothing,
          acCooldowns = cooldowns
        },
      AccountConfig
        { acAccountIndex = 1,
          acAmount = 1_000_000_000_000,
          acStaking =
            StakeDetailsBaker
                { sdStakedCapital = initialStakedAmount,
                  sdRestakeEarnings = True,
                  sdPendingChange = NoChange
                },
          acPoolInfo = Nothing,
          acCooldowns = emptyCooldowns
        },
      AccountConfig
        { acAccountIndex = 2,
          acAmount = 1_000_000_000_001,
          acStaking =
            StakeDetailsDelegator
                { sdStakedCapital = initialStakedAmount,
                  sdRestakeEarnings = True,
                  sdPendingChange = NoChange,
                  sdDelegationTarget = DelegateToBaker 0
                },
          acPoolInfo = Nothing,
          acCooldowns = emptyCooldowns
        }
    ]
  where
    (pendingChange, cooldowns)
        | pendingChangeOrCooldown = case sSupportsFlexibleCooldown (accountVersion @av) of
            STrue -> (NoChange, initialTestCooldowns)
            SFalse -> (ReduceStake (initialStakedAmount `div` 2) (PendingChangeEffectiveV1 1000), emptyCooldowns)
        | otherwise = (NoChange, emptyCooldowns)

-- | A configuration for testing 'bsoUpdateValidator'.
data ValidatorUpdateConfig = ValidatorUpdateConfig
    { -- | The update to perform
      vucValidatorUpdate :: ValidatorUpdate,
      -- | Conditions that should trigger a specific error
      vucValidatorConditions :: ValidatorConditions,
      -- | Whether the account should have a pending change or cooldown set initially
      vucPendingChangeOrCooldown :: Bool,
      -- | A description of the configuration
      vucDescription :: String
    }

instance Show ValidatorUpdateConfig where
    show = vucDescription

-- | Test cases for updating a validator. These cover a very broad combination of updates to
--  different fields.
validatorUpdateCases :: [ValidatorUpdateConfig]
validatorUpdateCases = do
    (vuKeys, vuKeysDesc, vcAggregationKeyDuplicate) <-
        [ (Nothing, "none", False),
          (Just (makeBakerKeyUpdate 0), "old keys", False),
          (Just (makeBakerKeyUpdate 2), "fresh keys", False),
          (Just (makeBakerKeyUpdate 1), "duplicate keys", True)
            ]
    (vuCapital, vuCapitalDesc, vcUnderThreshold) <-
        [ (Just 600_000_000_000, "increase", False),
          (Just initialStakedAmount, "same", False),
          (Just 1_000_000_000, "decrease", False),
          (Just 999_999_999, "insufficient", True),
          (Just 0, "zero", False),
          (Nothing, "no change", False)
            ]
    (vuRestakeEarnings, vuRestakeEarningsDesc) <-
        [ (Just True, "restake"),
          (Just False, "no restake"),
          (Nothing, "no change")
            ]
    (vuOpenForDelegation, vuOpenForDelegationDesc) <-
        [ (Just OpenForAll, "open"),
          (Just ClosedForAll, "closed for all"),
          (Just ClosedForNew, "closed for new"),
          (Nothing, "no change")
            ]
    (vuMetadataURL, vuMetadataURLDesc) <-
        [ (Just (UrlText "Some URL"), "same URL"),
          (Just (UrlText "Some new URL"), "new URL"),
          (Nothing, "no change")
            ]
    (vuTransactionFeeCommission, vuTransactionFeeCommissionDesc, vcTransactionFeeNotInRange) <-
        [ (Just (makeAmountFraction 100), "in range", False),
          (Just (makeAmountFraction 201), "out of range", True),
          (Just (makeAmountFraction 150), "same", False),
          (Nothing, "no change", False)
            ]
    (vuBakingRewardCommission, vuBakingRewardCommissionDesc, vcBakingRewardNotInRange) <-
        [ (Just (makeAmountFraction 500), "in range", False),
          (Just (makeAmountFraction 400), "out of range", True),
          (Just (makeAmountFraction 550), "same", False),
          (Nothing, "no change", False)
            ]
    (vuFinalizationRewardCommission, vuFinalizationRewardCommissionDesc, vcFinalizationRewardNotInRange) <-
        [ (Just (makeAmountFraction 300), "in range", False),
          (Just (makeAmountFraction 401), "out of range", True),
          (Just (makeAmountFraction 350), "same", False),
          (Nothing, "no change", False)
            ]
    (vuSuspend, vuSuspendDesc) <-
        [ (Just True, "suspend"),
          (Just False, "resume"),
          (Nothing, "no change")
            ]
    let vucValidatorUpdate = ValidatorUpdate{..}
    let vucValidatorConditions = ValidatorConditions{..}
    vucPendingChangeOrCooldown <- [True, False]
    let vucDescription =
            "keys: "
                <> vuKeysDesc
                <> ", capital: "
                <> vuCapitalDesc
                <> ", restake: "
                <> vuRestakeEarningsDesc
                <> ", open: "
                <> vuOpenForDelegationDesc
                <> ", URL: "
                <> vuMetadataURLDesc
                <> ", transaction fee: "
                <> vuTransactionFeeCommissionDesc
                <> ", baking reward: "
                <> vuBakingRewardCommissionDesc
                <> ", finalization reward: "
                <> vuFinalizationRewardCommissionDesc
                <> ( if vucPendingChangeOrCooldown
                        then ", pending change/cooldown"
                        else ", no pending change/cooldown"
                   )
                <> ", validator suspend/resume: "
                <> vuSuspendDesc
    return $ ValidatorUpdateConfig{..}

-- | Commission ranges that narrowly include the commission rates used in the test cases.
narrowCommissionRanges :: CommissionRanges
narrowCommissionRanges =
    CommissionRanges
        { _transactionCommissionRange = InclusiveRange (makeAmountFraction 100) (makeAmountFraction 200),
          _finalizationCommissionRange = InclusiveRange (makeAmountFraction 300) (makeAmountFraction 400),
          _bakingCommissionRange = InclusiveRange (makeAmountFraction 500) (makeAmountFraction 600)
        }

-- | Commission ranges that include the full range of possible commission rates.
fullCommissionRanges :: CommissionRanges
fullCommissionRanges =
    CommissionRanges
        { _transactionCommissionRange = fullRange,
          _finalizationCommissionRange = fullRange,
          _bakingCommissionRange = fullRange
        }
  where
    fullRange = InclusiveRange (makeAmountFraction 0) (makeAmountFraction 100_000)

-- | Test updating a validator in various possible ways.
testUpdateValidator ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    -- | If 'True', test all cases.
    Bool ->
    Spec
testUpdateValidator spv True = describe "bsoUpdateValidator" $ do
    forM_ validatorUpdateCases $ \conf -> do
        it (show conf) $ runUpdateValidatorTest spv narrowCommissionRanges conf
testUpdateValidator spv False = do
    it "bsoUpdateValidator (random cases)" $
        withMaxSuccess 1000 $
            forAll (elements validatorUpdateCases) $
                runUpdateValidatorTest spv narrowCommissionRanges

-- | This test case is to detect possible confusion between the different commission rates.
testUpdateValidatorOverlappingCommissions ::
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    SpecWith ()
testUpdateValidatorOverlappingCommissions spv =
    it "bsoUpdateValidator - overlapping commissions" $
        forAll genCases $
            runUpdateValidatorTest spv fullCommissionRanges
  where
    genCases = do
        let options = [Nothing, Just (AmountFraction 150), Just (AmountFraction 350), Just (AmountFraction 550), Just (AmountFraction 100_000)]
        vuTransactionFeeCommission <- elements options
        vuBakingRewardCommission <- elements options
        vuFinalizationRewardCommission <- elements options
        let vucValidatorUpdate =
                ValidatorUpdate
                    { vuKeys = Nothing,
                      vuCapital = Nothing,
                      vuRestakeEarnings = Nothing,
                      vuOpenForDelegation = Nothing,
                      vuMetadataURL = Nothing,
                      vuSuspend = Nothing,
                      ..
                    }
        let vucPendingChangeOrCooldown = False
        let vucValidatorConditions =
                ValidatorConditions
                    { vcUnderThreshold = False,
                      vcTransactionFeeNotInRange = False,
                      vcBakingRewardNotInRange = False,
                      vcFinalizationRewardNotInRange = False,
                      vcAggregationKeyDuplicate = False
                    }
        let vucDescription =
                "transaction fee: "
                    <> show vuTransactionFeeCommission
                    <> ", baking reward: "
                    <> show vuBakingRewardCommission
                    <> ", finalization reward: "
                    <> show vuFinalizationRewardCommission
        return ValidatorUpdateConfig{..}

-- | Run a test on 'bsoUpdateValidator', checking the behaviour is as expected.
runUpdateValidatorTest ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    CommissionRanges ->
    ValidatorUpdateConfig ->
    IO ()
runUpdateValidatorTest spv commissionRanges ValidatorUpdateConfig{vucValidatorUpdate = vu, vucValidatorConditions = vc, ..} = runTestBlockState @pv $ do
    initialAccounts <- mapM makeDummyAccount (updateValidatorTestAccounts vucPendingChangeOrCooldown)
    initialBS <- mkInitialState initialAccounts
    initialAccountBaker <- fmap fromJust . getAccountBaker . fromJust =<< bsoGetAccountByIndex initialBS 0
    res <- bsoUpdateValidator initialBS 1000 0 vu
    let expect
            | vcAggregationKeyDuplicate vc = Left (VCFDuplicateAggregationKey (bkuAggregationKey (fromJust $ vuKeys vu)))
            | vcTransactionFeeNotInRange vc = Left VCFTransactionFeeCommissionNotInRange
            | vcBakingRewardNotInRange vc = Left VCFBakingRewardCommissionNotInRange
            | vcFinalizationRewardNotInRange vc = Left VCFFinalizationRewardCommissionNotInRange
            | vucPendingChangeOrCooldown,
              isJust (vuCapital vu),
              not $ supportsFlexibleCooldown $ accountVersionFor $ demoteProtocolVersion (protocolVersion @pv) =
                Left VCFChangePending
            | vcUnderThreshold vc = Left VCFStakeUnderThreshold
            | otherwise = Right ()
    liftIO $ void res `shouldBe` expect
    forM_ res $ \(changes, bs) -> do
        -- We check that the changes are as expected.
        let expectChanges = execWriter $ do
                forM_ (vuKeys vu) $ \keys -> tell [BakerConfigureUpdateKeys keys]
                forM_ (vuRestakeEarnings vu) $ \restake -> tell [BakerConfigureRestakeEarnings restake]
                forM_ (vuOpenForDelegation vu) $ \open -> tell [BakerConfigureOpenForDelegation open]
                forM_ (vuMetadataURL vu) $ \url -> tell [BakerConfigureMetadataURL url]
                forM_ (vuTransactionFeeCommission vu) $ \fee -> tell [BakerConfigureTransactionFeeCommission fee]
                forM_ (vuBakingRewardCommission vu) $ \fee -> tell [BakerConfigureBakingRewardCommission fee]
                forM_ (vuFinalizationRewardCommission vu) $ \fee -> tell [BakerConfigureFinalizationRewardCommission fee]
                forM_ (vuSuspend vu) $ \suspended ->
                    tell
                        [ if suspended then BakerConfigureSuspended else BakerConfigureResumed
                          | STrue <- [hasValidatorSuspension]
                        ]
                forM_ (vuCapital vu) $ \capital ->
                    tell $
                        if capital >= initialStakedAmount
                            then [BakerConfigureStakeIncreased capital]
                            else [BakerConfigureStakeReduced capital]
        liftIO $ changes `shouldBe` expectChanges
        -- Check the active bakers are correct
        checkActiveBakers bs
        () <- case flexibleCooldown of
            STrue -> do
                -- Check that the cooldowns are correct
                newCooldowns <- checkCooldowns bs
                let bakerInitialCooldowns
                        | vucPendingChangeOrCooldown = initialTestCooldowns
                        | otherwise = emptyCooldowns
                let bakerExpectedCooldowns = case vuCapital vu of
                        Just newCapital -> case newCapital `compare` initialStakedAmount of
                            LT -> addPrePreCooldown (initialStakedAmount - newCapital) bakerInitialCooldowns
                            EQ -> bakerInitialCooldowns
                            GT -> reactivateCooldownAmount (newCapital - initialStakedAmount) bakerInitialCooldowns
                        _ -> bakerInitialCooldowns
                liftIO $ newCooldowns `shouldBe` [bakerExpectedCooldowns, emptyCooldowns, emptyCooldowns]
                when (vuCapital vu == Just 0) $ do
                    -- Check that account 2 delegates to passive now
                    acc2 <- fromJust <$> bsoGetAccountByIndex bs 2
                    getAccountDelegator acc2 >>= \case
                        Just del ->
                            liftIO $ (del ^. delegationTarget) `shouldBe` DelegatePassive
                        Nothing -> liftIO $ expectationFailure "Account 2 should have a delegator"
            SFalse -> return ()
        acc0 <- fromJust <$> bsoGetAccountByIndex bs 0
        let updateCapital newCapital
                | newCapital < initialStakedAmount,
                  SFalse <- flexibleCooldown =
                    bakerPendingChange
                        .~ (if newCapital == 0 then RemoveStake else ReduceStake newCapital)
                            (PendingChangeEffectiveV1 (24 * 60 * 60 * 1000 + 1000))
                | otherwise = stakedAmount .~ newCapital
        let updateSuspended suspend
                | STrue <- hasValidatorSuspension = accountBakerInfo . bieIsSuspended .~ suspend
                | otherwise = id
        let expectedAccountBaker
                | STrue <- flexibleCooldown, vuCapital vu == Just 0 = Nothing
                | otherwise =
                    Just $
                        initialAccountBaker
                            & maybe
                                id
                                ( \keys ->
                                    (bakerElectionVerifyKey .~ bkuElectionKey keys)
                                        . (bakerSignatureVerifyKey .~ bkuSignKey keys)
                                        . (bakerAggregationVerifyKey .~ bkuAggregationKey keys)
                                )
                                (vuKeys vu)
                            & maybe id updateCapital (vuCapital vu)
                            & maybe id (stakeEarnings .~) (vuRestakeEarnings vu)
                            & maybe id (poolOpenStatus .~) (vuOpenForDelegation vu)
                            & maybe id (poolMetadataUrl .~) (vuMetadataURL vu)
                            & maybe id (poolCommissionRates . finalizationCommission .~) (vuFinalizationRewardCommission vu)
                            & maybe id (poolCommissionRates . bakingCommission .~) (vuBakingRewardCommission vu)
                            & maybe id (poolCommissionRates . transactionCommission .~) (vuTransactionFeeCommission vu)
                            & maybe id updateSuspended (vuSuspend vu)
        actualAccountBaker <- getAccountBaker acc0
        liftIO $ actualAccountBaker `shouldBe` expectedAccountBaker
  where
    flexibleCooldown = sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv))
    hasValidatorSuspension = sSupportsValidatorSuspension (accountVersion @(AccountVersionFor pv))
    minEquity = 1_000_000_000
    chainParams =
        DummyData.dummyChainParameters @(ChainParametersVersionFor pv)
            & cpPoolParameters . ppMinimumEquityCapital .~ minEquity
            & cpPoolParameters . ppCommissionBounds
                .~ commissionRanges
    mkInitialState accounts =
        hpbsPointers
            <$> initialPersistentState @pv
                (dummySeedState (protocolVersion @pv))
                DummyData.dummyCryptographicParameters
                accounts
                DummyData.dummyIdentityProviders
                DummyData.dummyArs
                (withIsAuthorizationsVersionForPV spv DummyData.dummyKeyCollection)
                chainParams

tests :: Word -> Spec
tests lvl = parallel $ describe "Validator" $ do
    describe "P6" $ do
        testAddValidatorAllCases SP6
        testUpdateValidator SP6 (lvl > 1)
        testUpdateValidatorOverlappingCommissions SP6
    describe "P7" $ do
        testAddValidatorAllCases SP7
        testUpdateValidator SP7 (lvl > 1)
        testUpdateValidatorOverlappingCommissions SP7
    describe "P8" $ do
        testAddValidatorAllCases SP8
        testUpdateValidator SP8 (lvl > 1)
        testUpdateValidatorOverlappingCommissions SP8
