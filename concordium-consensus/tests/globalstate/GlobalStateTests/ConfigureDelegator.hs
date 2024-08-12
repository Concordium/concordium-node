{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.ConfigureDelegator where

import Concordium.GlobalState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CooldownQueue
import qualified Concordium.GlobalState.DummyData as DummyData
import Concordium.GlobalState.Persistent.BlockState
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Execution
import Concordium.Types.Option
import Concordium.Types.Parameters
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer.CPS
import Data.Bool.Singletons
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ratio
import GlobalStateTests.BlockStateHelpers
import Lens.Micro.Platform
import Test.Hspec
import Test.QuickCheck

-- | Some non-trivial cooldowns that may be set on an account for testing.
initialTestCooldowns :: Cooldowns
initialTestCooldowns =
    Cooldowns
        { inCooldown = Map.fromList [(1000, 2), (2000, 2)],
          prePreCooldown = Present 2,
          preCooldown = Present 2
        }

baseAmount :: Amount
baseAmount = 100

baseStake :: Amount
baseStake = 1

-- | Generate a list of test accounts with at least one baker, one delegator and one
--  non-staking account.
genTestAccounts :: forall av. (IsAccountVersion av, AVSupportsDelegation av) => Gen [AccountConfig av]
genTestAccounts = do
    let nAccounts = 10
    let accountIndices = [0 .. nAccounts - 1]
    nBakers <- choose (1, nAccounts - 2)
    bakerIndices <- take nBakers <$> shuffle accountIndices
    nDelegators <- choose (1, nAccounts - nBakers - 1)
    let nonBakers = filter (`notElem` bakerIndices) accountIndices
    delegatorIndices <- take nDelegators <$> shuffle nonBakers
    bakerStatuses <- vectorOf nAccounts (frequency [(8, return OpenForAll), (1, return ClosedForAll), (1, return ClosedForNew)])
    forM accountIndices $ \ai -> do
        stake <- (baseStake *) . fromInteger <$> choose (1, 10)
        delTarget <-
            elements $
                DelegatePassive
                    : [DelegateToBaker (fromIntegral bi) | bi <- bakerIndices, bakerStatuses !! bi /= ClosedForAll]
        let poolStatus = bakerStatuses !! ai
        (cooldowns, pendingChange) <- case sSupportsFlexibleCooldown (accountVersion @av) of
            STrue -> (,NoChange) <$> elements [emptyCooldowns, initialTestCooldowns]
            SFalse ->
                (emptyCooldowns,)
                    <$> oneof
                        [ return NoChange,
                          ReduceStake . fromInteger <$> choose (1, fromIntegral stake) <*> pure (PendingChangeEffectiveV1 1000),
                          return $ RemoveStake (PendingChangeEffectiveV1 1000)
                        ]
        return $
            AccountConfig
                { acAccountIndex = fromIntegral ai,
                  acAmount = baseAmount,
                  acStaking =
                    if ai `elem` bakerIndices
                        then
                            StakeDetailsBaker
                                { sdStakedCapital = stake,
                                  sdRestakeEarnings = True,
                                  sdPendingChange = pendingChange
                                }
                        else
                            if ai `elem` delegatorIndices
                                then
                                    StakeDetailsDelegator
                                        { sdStakedCapital = stake,
                                          sdRestakeEarnings = True,
                                          sdPendingChange = pendingChange,
                                          sdDelegationTarget = delTarget
                                        }
                                else StakeDetailsNone,
                  acPoolInfo = Just dummyBakerPoolInfo{_poolOpenStatus = poolStatus},
                  acCooldowns = cooldowns
                }

data DelegatorTestConfig av = DelegatorTestConfig
    { dtcAccounts :: [AccountConfig av],
      dtcUseAccount :: AccountIndex,
      dtcCapitalBound :: CapitalBound,
      dtcLeverageBound :: LeverageFactor
    }
    deriving (Show)

-- | Determine if the target is open for delegation. Returns 'Nothing' if the target is not a baker,
--  'Just True' if the target is open for delegation and 'Just False' if the target is closed for
--  further delegation.
dtcTargetOpen :: DelegatorTestConfig av -> DelegationTarget -> Maybe Bool
dtcTargetOpen _ DelegatePassive = Just True
dtcTargetOpen DelegatorTestConfig{..} (DelegateToBaker bi)
    | fromIntegral bi < length dtcAccounts,
      AccountConfig{acStaking = StakeDetailsBaker{}, ..} <- dtcAccounts !! fromIntegral bi =
        case acPoolInfo of
            Just BakerPoolInfo{_poolOpenStatus = OpenForAll} -> Just True
            Just _ -> Just False
            _ -> Nothing
    | otherwise = Nothing

dtcTotalStake :: DelegatorTestConfig av -> Amount
dtcTotalStake DelegatorTestConfig{..} =
    sum $ map (accountStakedCapital . acStaking) dtcAccounts
  where
    accountStakedCapital StakeDetailsBaker{..} = sdStakedCapital
    accountStakedCapital StakeDetailsDelegator{..} = sdStakedCapital
    accountStakedCapital _ = 0

dtcBakerStake :: DelegatorTestConfig av -> BakerId -> Maybe (Amount, Amount)
dtcBakerStake DelegatorTestConfig{..} bi = (,delegatorStakes) <$> bkrStake
  where
    bkrStake
        | fromIntegral bi < length dtcAccounts,
          AccountConfig{acStaking = StakeDetailsBaker{..}} <- dtcAccounts !! fromIntegral bi =
            Just sdStakedCapital
        | otherwise = Nothing
    delegatorStakes = sum $ map delStake dtcAccounts
    delStake AccountConfig{acStaking = StakeDetailsDelegator{..}}
        | sdDelegationTarget == DelegateToBaker bi = sdStakedCapital
    delStake _ = 0

dtcCooldowns :: DelegatorTestConfig av -> [Cooldowns]
dtcCooldowns DelegatorTestConfig{..} = map acCooldowns dtcAccounts

expectedResult :: forall av. (IsAccountVersion av) => DelegatorTestConfig av -> DelegationConfigure -> DelegationConfigureResult
expectedResult dtc@DelegatorTestConfig{..} DelegationConfigureAdd{..}
    | Just False <- targetOpen = DCPoolClosed
    | Nothing <- targetOpen,
      DelegateToBaker bid <- dcaDelegationTarget =
        DCInvalidDelegationTarget bid
    | DelegateToBaker bid <- dcaDelegationTarget,
      Just (bkrStake, delStake) <- dtcBakerStake dtc bid =
        if delStake + dcaCapital + bkrStake > applyLeverageFactor dtcLeverageBound bkrStake
            then DCPoolStakeOverThreshold
            else
                if bkrStake + delStake + dcaCapital > takeFraction (theCapitalBound dtcCapitalBound) (dtcTotalStake dtc + dcaCapital)
                    then DCPoolOverDelegated
                    else DCSuccess [] (DelegatorId dtcUseAccount)
    | otherwise = DCSuccess [] (DelegatorId dtcUseAccount)
  where
    targetOpen = dtcTargetOpen dtc dcaDelegationTarget
expectedResult dtc@DelegatorTestConfig{..} DelegationConfigureUpdate{..}
    | Just t <- dcuDelegationTarget,
      oldTarget /= t,
      Just False <- dtcTargetOpen dtc t =
        DCPoolClosed
    | Just t@(DelegateToBaker bid) <- dcuDelegationTarget,
      Nothing <- dtcTargetOpen dtc t =
        DCInvalidDelegationTarget bid
    | Just _ <- dcuCapital, changePending = DCChangePending
    | DelegateToBaker bid <- newTarget,
      newEffectiveCapital > 0,
      oldTarget /= newTarget || oldCapital < newEffectiveCapital,
      Just (bkrStake, delStake) <- dtcBakerStake dtc bid =
        if
            | applyAmountDelta deltaPool (delStake + bkrStake)
                > applyLeverageFactor dtcLeverageBound bkrStake ->
                DCPoolStakeOverThreshold
            | applyAmountDelta deltaPool (bkrStake + delStake)
                > takeFraction
                    (theCapitalBound dtcCapitalBound)
                    (applyAmountDelta delta $ dtcTotalStake dtc) ->
                DCPoolOverDelegated
            | otherwise -> DCSuccess expectChanges (DelegatorId dtcUseAccount)
    | otherwise = DCSuccess expectChanges (DelegatorId dtcUseAccount)
  where
    flexibleCooldown = case sSupportsFlexibleCooldown (accountVersion @av) of
        STrue -> True
        SFalse -> False
    senderAccount = dtcAccounts !! fromIntegral dtcUseAccount
    (oldCapital, oldTarget, changePending) = case acStaking senderAccount of
        StakeDetailsDelegator{..} -> (sdStakedCapital, sdDelegationTarget, sdPendingChange /= NoChange)
        _ -> error "Account is not a delegator"
    newEffectiveCapital
        | not flexibleCooldown,
          Just newCap <- dcuCapital,
          newCap < oldCapital =
            oldCapital
        | otherwise = fromMaybe oldCapital dcuCapital
    delta = amountDiff newEffectiveCapital oldCapital
    deltaPool
        | Just t <- dcuDelegationTarget, t /= oldTarget = amountToDelta newEffectiveCapital
        | otherwise = delta
    newTarget = fromMaybe oldTarget dcuDelegationTarget
    expectChanges = execWriter $ do
        forM_ dcuDelegationTarget $ tell . (: []) . DelegationConfigureDelegationTarget
        forM_ dcuRestakeEarnings $ tell . (: []) . DelegationConfigureRestakeEarnings
        forM_ dcuCapital $ \newCap ->
            if newCap >= oldCapital
                then tell [DelegationConfigureStakeIncreased newCap]
                else tell [DelegationConfigureStakeReduced newCap]

showExpectedResult :: DelegationConfigureResult -> String
showExpectedResult DCInvalidDelegationTarget{} = "DCInvalidDelegationTarget"
showExpectedResult DCPoolClosed = "DCPoolClosed"
showExpectedResult DCPoolStakeOverThreshold = "DCPoolStakeOverThreshold"
showExpectedResult DCPoolOverDelegated = "DCPoolOverDelegated"
showExpectedResult DCChangePending = "DCChangePending"
showExpectedResult DCSuccess{} = "Success"
showExpectedResult DCInvalidAccount = "DCInvalidAccount"
showExpectedResult DCInvalidDelegator = "DCInvalidDelegator"

runDelegatorTest ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    DelegatorTestConfig (AccountVersionFor pv) ->
    DelegationConfigure ->
    IO ()
runDelegatorTest spv dtc@DelegatorTestConfig{..} da@DelegationConfigureAdd{..} = runTestBlockState @pv $ do
    initialAccounts <- mapM makeDummyAccount dtcAccounts
    initialBS <- mkInitialState initialAccounts
    (res, bs) <- bsoConfigureDelegation initialBS dtcUseAccount da
    let expect = expectedResult dtc da
    liftIO $ res `shouldBe` expect
    case res of
        DCSuccess{} -> do
            checkActiveBakers bs
            () <- case flexibleCooldown of
                STrue -> do
                    newCooldowns <- checkCooldowns bs
                    liftIO $
                        newCooldowns
                            `shouldBe` ( dtcCooldowns dtc
                                            & ix (fromIntegral dtcUseAccount) %~ reactivateCooldownAmount dcaCapital
                                       )
                SFalse -> return ()
            acc <- fromJust <$> bsoGetAccountByIndex bs dtcUseAccount
            let expectAccountDelegation =
                    AccountDelegationV1
                        { _delegationIdentity = DelegatorId dtcUseAccount,
                          _delegationStakedAmount = dcaCapital,
                          _delegationStakeEarnings = dcaRestakeEarnings,
                          _delegationTarget = dcaDelegationTarget,
                          _delegationPendingChange = NoChange
                        }
            actualAccountDelegation <- getAccountDelegator acc
            liftIO $ actualAccountDelegation `shouldBe` Just expectAccountDelegation
        _ -> return ()
  where
    flexibleCooldown = sSupportsFlexibleCooldown (sAccountVersionFor spv)
    chainParams =
        DummyData.dummyChainParameters @(ChainParametersVersionFor pv)
            & cpPoolParameters . ppCapitalBound .~ dtcCapitalBound
            & cpPoolParameters . ppLeverageBound .~ dtcLeverageBound
    mkInitialState accounts =
        hpbsPointers
            <$> initialPersistentState @pv
                (dummySeedState spv)
                DummyData.dummyCryptographicParameters
                accounts
                DummyData.dummyIdentityProviders
                DummyData.dummyArs
                (withIsAuthorizationsVersionForPV spv DummyData.dummyKeyCollection)
                chainParams
runDelegatorTest spv dtc@DelegatorTestConfig{..} du@DelegationConfigureUpdate{..} = runTestBlockState @pv $ do
    initialAccounts <- mapM makeDummyAccount dtcAccounts
    initialBS <- mkInitialState initialAccounts
    (res, bs) <- bsoConfigureDelegation initialBS dtcUseAccount du
    let expect = expectedResult dtc du
    liftIO $ res `shouldBe` expect
    case res of
        DCSuccess{} -> do
            checkActiveBakers bs
            () <- case flexibleCooldown of
                STrue -> do
                    newCooldowns <- checkCooldowns bs
                    liftIO $
                        newCooldowns
                            `shouldBe` ( dtcCooldowns dtc
                                            & ix (fromIntegral dtcUseAccount) %~ updateCooldown
                                       )
                SFalse -> return ()
            acc <- fromJust <$> bsoGetAccountByIndex bs dtcUseAccount
            let (newPendingChange, newEffectiveCapital) = case flexibleCooldown of
                    SFalse
                        | Just newCapital <- dcuCapital,
                          newCapital == 0 ->
                            (RemoveStake (PendingChangeEffectiveV1 (24 * 60 * 60 * 1000 + 5000)), oldCapital)
                        | Just newCapital <- dcuCapital,
                          newCapital < oldCapital ->
                            (ReduceStake newCapital (PendingChangeEffectiveV1 (24 * 60 * 60 * 1000 + 5000)), oldCapital)
                    _ -> (oldPendingChange, fromMaybe oldCapital dcuCapital)

            let expectAccountDelegation' =
                    AccountDelegationV1
                        { _delegationIdentity = DelegatorId dtcUseAccount,
                          _delegationStakedAmount = newEffectiveCapital,
                          _delegationStakeEarnings = fromMaybe oldRestake dcuRestakeEarnings,
                          _delegationTarget = fromMaybe oldTarget dcuDelegationTarget,
                          _delegationPendingChange = newPendingChange
                        }
            let expectAccountDelegation = case flexibleCooldown of
                    STrue | Just newCapital <- dcuCapital, newCapital == 0 -> Nothing
                    _ -> Just expectAccountDelegation'
            actualAccountDelegation <- getAccountDelegator acc
            liftIO $ actualAccountDelegation `shouldBe` expectAccountDelegation
        _ -> return ()
  where
    flexibleCooldown = sSupportsFlexibleCooldown (sAccountVersionFor spv)
    chainParams =
        DummyData.dummyChainParameters @(ChainParametersVersionFor pv)
            & cpPoolParameters . ppCapitalBound .~ dtcCapitalBound
            & cpPoolParameters . ppLeverageBound .~ dtcLeverageBound
    (oldCapital, oldRestake, oldTarget, oldPendingChange) =
        case acStaking (dtcAccounts !! fromIntegral dtcUseAccount) of
            StakeDetailsDelegator{..} ->
                (sdStakedCapital, sdRestakeEarnings, sdDelegationTarget, sdPendingChange)
            _ ->
                error "Account is not a delegator"
    updateCooldown = case dcuCapital of
        Just newCapital
            | newCapital > oldCapital -> reactivateCooldownAmount (newCapital - oldCapital)
            | newCapital < oldCapital -> addPrePreCooldown (oldCapital - newCapital)
        _ -> id
    mkInitialState accounts =
        hpbsPointers
            <$> initialPersistentState @pv
                (dummySeedState spv)
                DummyData.dummyCryptographicParameters
                accounts
                DummyData.dummyIdentityProviders
                DummyData.dummyArs
                (withIsAuthorizationsVersionForPV spv DummyData.dummyKeyCollection)
                chainParams

testAddDelegator ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    Property
testAddDelegator spv = withMaxSuccess 1000 $ property $ do
    accounts <- genTestAccounts @(AccountVersionFor pv)
    useAccount <- elements [acc | acc <- accounts, isUnstaked acc]
    capital <-
        oneof
            [ (baseStake *) . fromInteger <$> choose (1, 20),
              fromInteger <$> choose (1, fromIntegral baseAmount)
            ]
    restake <- arbitrary
    let chooseNonBaker = elements [DelegateToBaker (BakerId (acAccountIndex acc)) | acc <- accounts, not (isBaker acc)]
    let chooseBaker = elements [DelegateToBaker (BakerId (acAccountIndex acc)) | acc <- accounts, isBaker acc]
    let chooseInvalidAccount = elements [DelegateToBaker (BakerId (fromIntegral i)) | i <- [length accounts .. length accounts + 10]]
    target <-
        frequency
            [ (1, return DelegatePassive),
              (1, chooseNonBaker),
              (8, chooseBaker),
              (1, chooseInvalidAccount)
            ]
    let delegatorAdd =
            DelegationConfigureAdd
                { dcaCapital = capital,
                  dcaRestakeEarnings = restake,
                  dcaDelegationTarget = target
                }
    capitalBound <- CapitalBound . AmountFraction . fromInteger <$> choose (1, 100_000)
    leverageDen <- choose (1, 10)
    leverageNum <- choose (leverageDen, 100)
    let leverageBound = LeverageFactor $ leverageNum % leverageDen
    let config =
            DelegatorTestConfig
                { dtcAccounts = accounts,
                  dtcUseAccount = acAccountIndex useAccount,
                  dtcCapitalBound = capitalBound,
                  dtcLeverageBound = leverageBound
                }
    let lab
            | target == DelegatePassive = "Passive delegation"
            | otherwise = showExpectedResult $ expectedResult config delegatorAdd
    return $
        label lab $
            counterexample (show config) $
                counterexample (show delegatorAdd) $
                    ioProperty $
                        runDelegatorTest spv config delegatorAdd
  where
    isUnstaked AccountConfig{acStaking = StakeDetailsNone} = True
    isUnstaked _ = False
    isBaker AccountConfig{acStaking = StakeDetailsBaker{}} = True
    isBaker _ = False

testUpdateDelegator ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    Property
testUpdateDelegator spv = withMaxSuccess 1000 $ property $ do
    accounts <- genTestAccounts @(AccountVersionFor pv)
    useAccount <- elements [acc | acc <- accounts, isDelegator acc]
    capital <-
        frequency
            [ (8, Just . (baseStake *) . fromInteger <$> choose (1, 20)),
              (4, Just . fromInteger <$> choose (1, fromIntegral baseAmount)),
              (2, return Nothing),
              (1, return $ Just 0)
            ]
    restake <- arbitrary
    let chooseNonBaker = elements [DelegateToBaker (BakerId (acAccountIndex acc)) | acc <- accounts, not (isBaker acc)]
    let chooseBaker = elements [DelegateToBaker (BakerId (acAccountIndex acc)) | acc <- accounts, isBaker acc]
    let chooseInvalidAccount = elements [DelegateToBaker (BakerId (fromIntegral i)) | i <- [length accounts .. length accounts + 10]]
    target <-
        frequency
            [ (1, return $ Just DelegatePassive),
              (1, Just <$> chooseNonBaker),
              (8, Just <$> chooseBaker),
              (1, return Nothing),
              (1, Just <$> chooseInvalidAccount)
            ]
    let delegatorUpdate =
            DelegationConfigureUpdate
                { dcuCapital = capital,
                  dcuRestakeEarnings = restake,
                  dcuDelegationTarget = target,
                  dcuSlotTimestamp = 5000
                }
    capitalBound <- CapitalBound . AmountFraction . fromInteger <$> choose (1, 100_000)
    leverageDen <- choose (1, 10)
    leverageNum <- choose (leverageDen, 100)
    let leverageBound = LeverageFactor $ leverageNum % leverageDen
    let config =
            DelegatorTestConfig
                { dtcAccounts = accounts,
                  dtcUseAccount = acAccountIndex useAccount,
                  dtcCapitalBound = capitalBound,
                  dtcLeverageBound = leverageBound
                }
    let lab
            | target == Just DelegatePassive = "Passive delegation"
            | otherwise = showExpectedResult $ expectedResult config delegatorUpdate
    return $
        label lab $
            counterexample (show config) $
                counterexample (show delegatorUpdate) $
                    ioProperty $
                        runDelegatorTest spv config delegatorUpdate
  where
    isDelegator AccountConfig{acStaking = StakeDetailsDelegator{}} = True
    isDelegator _ = False
    isBaker AccountConfig{acStaking = StakeDetailsBaker{}} = True
    isBaker _ = False

tests :: Spec
tests = parallel $ describe "Configure delegator" $ do
    describe "P6" $ do
        it "bsoAddDelegator" $ testAddDelegator SP6
        it "bsoUpdateDelegator" $ testUpdateDelegator SP6

-- describe "P7" $ do
--     it "bsoAddDelegator" $ testAddDelegator SP7
--     it "bsoUpdateDelegator" $ testUpdateDelegator SP7
