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

expectedAddResult :: DelegatorTestConfig av -> DelegatorAdd -> Either DelegatorConfigureFailure ()
expectedAddResult dtc@DelegatorTestConfig{..} DelegatorAdd{..}
    | Just False <- targetOpen = Left DCFPoolClosed
    | Nothing <- targetOpen,
      DelegateToBaker bid <- daDelegationTarget =
        Left $ DCFInvalidDelegationTarget bid
    | DelegateToBaker bid <- daDelegationTarget,
      Just (bkrStake, delStake) <- dtcBakerStake dtc bid =
        if delStake + daCapital + bkrStake > applyLeverageFactor dtcLeverageBound bkrStake
            then Left DCFPoolStakeOverThreshold
            else
                if bkrStake + delStake + daCapital > takeFraction (theCapitalBound dtcCapitalBound) (dtcTotalStake dtc + daCapital)
                    then Left DCFPoolOverDelegated
                    else Right ()
    | otherwise = Right ()
  where
    targetOpen = dtcTargetOpen dtc daDelegationTarget

expectedUpdateResult :: forall av. (IsAccountVersion av) => DelegatorTestConfig av -> DelegatorUpdate -> Either DelegatorConfigureFailure ()
expectedUpdateResult dtc@DelegatorTestConfig{..} DelegatorUpdate{..}
    | Just t <- duDelegationTarget,
      oldTarget /= t,
      Just False <- dtcTargetOpen dtc t =
        Left DCFPoolClosed
    | Just t@(DelegateToBaker bid) <- duDelegationTarget,
      Nothing <- dtcTargetOpen dtc t =
        Left $ DCFInvalidDelegationTarget bid
    | Just _ <- duCapital, changePending = Left DCFChangePending
    | DelegateToBaker bid <- newTarget,
      newEffectiveCapital > 0,
      oldTarget /= newTarget || oldCapital < newEffectiveCapital,
      Just (bkrStake, delStake) <- dtcBakerStake dtc bid =
        if
            | applyAmountDelta deltaPool (delStake + bkrStake)
                > applyLeverageFactor dtcLeverageBound bkrStake ->
                Left DCFPoolStakeOverThreshold
            | applyAmountDelta deltaPool (bkrStake + delStake)
                > takeFraction
                    (theCapitalBound dtcCapitalBound)
                    (applyAmountDelta delta $ dtcTotalStake dtc) ->
                Left DCFPoolOverDelegated
            | otherwise -> Right ()
    | otherwise = Right ()
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
          Just newCap <- duCapital,
          newCap < oldCapital =
            oldCapital
        | otherwise = fromMaybe oldCapital duCapital
    delta = amountDiff newEffectiveCapital oldCapital
    deltaPool
        | Just t <- duDelegationTarget, t /= oldTarget = amountToDelta newEffectiveCapital
        | otherwise = delta
    newTarget = fromMaybe oldTarget duDelegationTarget

showExpectedResult :: Either DelegatorConfigureFailure () -> String
showExpectedResult (Left DCFInvalidDelegationTarget{}) = "DCFInvalidDelegationTarget"
showExpectedResult (Left DCFPoolClosed) = "DCFPoolClosed"
showExpectedResult (Left DCFPoolStakeOverThreshold) = "DCFPoolStakeOverThreshold"
showExpectedResult (Left DCFPoolOverDelegated) = "DCFPoolOverDelegated"
showExpectedResult (Left DCFChangePending) = "DCFChangePending"
showExpectedResult (Right _) = "Success"

runAddDelegatorTest ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    DelegatorTestConfig (AccountVersionFor pv) ->
    DelegatorAdd ->
    IO ()
runAddDelegatorTest spv dtc@DelegatorTestConfig{..} da@DelegatorAdd{..} = runTestBlockState @pv $ do
    initialAccounts <- mapM makeDummyAccount dtcAccounts
    initialBS <- mkInitialState initialAccounts
    res <- bsoAddDelegator initialBS dtcUseAccount da
    let expect = expectedAddResult dtc da
    liftIO $ void res `shouldBe` expect
    forM_ res $ \bs -> do
        checkActiveBakers bs
        () <- case flexibleCooldown of
            STrue -> do
                newCooldowns <- checkCooldowns bs
                liftIO $
                    newCooldowns
                        `shouldBe` ( dtcCooldowns dtc
                                        & ix (fromIntegral dtcUseAccount) %~ reactivateCooldownAmount daCapital
                                   )
            SFalse -> return ()
        acc <- fromJust <$> bsoGetAccountByIndex bs dtcUseAccount
        let expectAccountDelegation =
                AccountDelegationV1
                    { _delegationIdentity = DelegatorId dtcUseAccount,
                      _delegationStakedAmount = daCapital,
                      _delegationStakeEarnings = daRestakeEarnings,
                      _delegationTarget = daDelegationTarget,
                      _delegationPendingChange = NoChange
                    }
        actualAccountDelegation <- getAccountDelegator acc
        liftIO $ actualAccountDelegation `shouldBe` Just expectAccountDelegation
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
            DelegatorAdd
                { daCapital = capital,
                  daRestakeEarnings = restake,
                  daDelegationTarget = target
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
            | otherwise = showExpectedResult $ expectedAddResult config delegatorAdd
    return $
        label lab $
            counterexample (show config) $
                counterexample (show delegatorAdd) $
                    ioProperty $
                        runAddDelegatorTest spv config delegatorAdd
  where
    isUnstaked AccountConfig{acStaking = StakeDetailsNone} = True
    isUnstaked _ = False
    isBaker AccountConfig{acStaking = StakeDetailsBaker{}} = True
    isBaker _ = False

runUpdateDelegatorTest ::
    forall pv.
    ( IsProtocolVersion pv,
      SupportsDelegation (AccountVersionFor pv) ~ 'True,
      PoolParametersVersionFor (ChainParametersVersionFor pv) ~ 'PoolParametersVersion1
    ) =>
    SProtocolVersion pv ->
    DelegatorTestConfig (AccountVersionFor pv) ->
    DelegatorUpdate ->
    IO ()
runUpdateDelegatorTest spv dtc@DelegatorTestConfig{..} du@DelegatorUpdate{..} = runTestBlockState @pv $ do
    initialAccounts <- mapM makeDummyAccount dtcAccounts
    initialBS <- mkInitialState initialAccounts
    res <- bsoUpdateDelegator initialBS 5000 dtcUseAccount du
    let expect = expectedUpdateResult dtc du
    liftIO $ void res `shouldBe` expect
    forM_ res $ \(changes, bs) -> do
        liftIO $ changes `shouldBe` expectChanges
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
                    | Just newCapital <- duCapital,
                      newCapital == 0 ->
                        (RemoveStake (PendingChangeEffectiveV1 (24 * 60 * 60 * 1000 + 5000)), oldCapital)
                    | Just newCapital <- duCapital,
                      newCapital < oldCapital ->
                        (ReduceStake newCapital (PendingChangeEffectiveV1 (24 * 60 * 60 * 1000 + 5000)), oldCapital)
                _ -> (oldPendingChange, fromMaybe oldCapital duCapital)

        let expectAccountDelegation' =
                AccountDelegationV1
                    { _delegationIdentity = DelegatorId dtcUseAccount,
                      _delegationStakedAmount = newEffectiveCapital,
                      _delegationStakeEarnings = fromMaybe oldRestake duRestakeEarnings,
                      _delegationTarget = fromMaybe oldTarget duDelegationTarget,
                      _delegationPendingChange = newPendingChange
                    }
        let expectAccountDelegation = case flexibleCooldown of
                STrue | Just newCapital <- duCapital, newCapital == 0 -> Nothing
                _ -> Just expectAccountDelegation'
        actualAccountDelegation <- getAccountDelegator acc
        liftIO $ actualAccountDelegation `shouldBe` expectAccountDelegation
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
    updateCooldown = case duCapital of
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
    expectChanges = execWriter $ do
        forM_ duDelegationTarget $ tell . (: []) . DelegationConfigureDelegationTarget
        forM_ duRestakeEarnings $ tell . (: []) . DelegationConfigureRestakeEarnings
        forM_ duCapital $ \newCap ->
            if newCap >= oldCapital
                then tell [DelegationConfigureStakeIncreased newCap]
                else tell [DelegationConfigureStakeReduced newCap]

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
            DelegatorUpdate
                { duCapital = capital,
                  duRestakeEarnings = restake,
                  duDelegationTarget = target
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
            | otherwise = showExpectedResult $ expectedUpdateResult config delegatorUpdate
    return $
        label lab $
            counterexample (show config) $
                counterexample (show delegatorUpdate) $
                    ioProperty $
                        runUpdateDelegatorTest spv config delegatorUpdate
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
    describe "P7" $ do
        it "bsoAddDelegator" $ testAddDelegator SP7
        it "bsoUpdateDelegator" $ testUpdateDelegator SP7
