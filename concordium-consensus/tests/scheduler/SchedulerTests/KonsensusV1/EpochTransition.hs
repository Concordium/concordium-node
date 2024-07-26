{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.KonsensusV1.EpochTransition where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro
import System.FilePath
import System.IO.Temp
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
import Concordium.Types
import Concordium.Types.Option
import Concordium.Types.SeedState

import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CooldownQueue
import qualified Concordium.GlobalState.DummyData as DummyData
import Concordium.GlobalState.Persistent.Account
import qualified Concordium.GlobalState.Persistent.Account.CooldownQueue as CooldownQueue
import qualified Concordium.GlobalState.Persistent.Account.StructureV1 as SV1
import Concordium.GlobalState.Persistent.Accounts
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Persistent.Cooldown as Cooldown
import qualified Concordium.GlobalState.Persistent.ReleaseSchedule as ReleaseSchedule
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Scheduler
import Concordium.Kontrol.Bakers
import Concordium.Scheduler.DummyData
import Concordium.Types.Accounts
import Concordium.Types.Execution
import Concordium.Types.Parameters
import Control.Monad
import qualified Data.Vector as Vec

dummyCooldownAccount ::
    forall av m.
    (IsAccountVersion av, MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    AccountIndex ->
    Amount ->
    Cooldowns ->
    m (PersistentAccount av)
dummyCooldownAccount ai amt cooldowns = do
    makeTestAccountFromSeed @av amt (fromIntegral ai) >>= \case
        PAV3 acc -> do
            let ed = SV1.enduringData acc
            cq <- CooldownQueue.makeCooldownQueue cooldowns
            newEnduring <- refMake =<< SV1.rehashAccountEnduringData ed{SV1.paedStakeCooldown = cq}
            return $ PAV3 acc{SV1.accountEnduringData = newEnduring}

runTestBlockState ::
    forall pv a.
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (BlobStoreT (PersistentBlockStateContext pv) (LoggerT IO))
        a ->
    IO a
runTestBlockState kont = withTempDirectory "." "blockstate" $ \dir -> do
    bracket
        ( do
            pbscBlobStore <- createBlobStore (dir </> "blockstate.dat")
            pbscAccountCache <- newAccountCache 100
            pbscModuleCache <- Modules.newModuleCache 100
            pbscAccountMap <- LMDBAccountMap.openDatabase (dir </> "accountmap")
            return PersistentBlockStateContext{..}
        )
        ( \PersistentBlockStateContext{..} -> do
            closeBlobStore pbscBlobStore
            LMDBAccountMap.closeDatabase pbscAccountMap
        )
        (runSilentLogger . runBlobStoreT (runPersistentBlockStateMonad kont))

-- | Get the 'Cooldowns' for each account, and check that the indexes for cooldowns, pre-cooldowns
--  and pre-pre-cooldowns are correct.
checkCooldowns :: (PVSupportsFlexibleCooldown pv, SupportsPersistentState pv m) => PersistentBlockState pv -> m [Cooldowns]
checkCooldowns pbs = do
    bsp <- loadPBS pbs
    (_, theCooldowns, cooldownMap, preCooldowns, prePreCooldowns) <-
        foldAccounts
            ( \(!ai, accum, cooldownMap, preCooldowns, prePreCooldowns) pa -> do
                cd <- fromMaybe emptyCooldowns <$> accountCooldowns pa
                let newCooldowns = cd : accum
                let newCooldownMap = case Map.lookupMin (inCooldown cd) of
                        Nothing -> cooldownMap
                        Just (ts, _) ->
                            Map.alter
                                ( \case
                                    Nothing -> Just (Set.singleton ai)
                                    Just s -> Just (Set.insert ai s)
                                )
                                ts
                                cooldownMap
                let newPreCooldowns = case preCooldown cd of
                        Absent -> preCooldowns
                        Present _ -> Set.insert ai preCooldowns
                let newPrePreCooldowns = case prePreCooldown cd of
                        Absent -> prePreCooldowns
                        Present _ -> Set.insert ai prePreCooldowns
                return (ai + 1, newCooldowns, newCooldownMap, newPreCooldowns, newPrePreCooldowns)
            )
            (AccountIndex 0, [], Map.empty, Set.empty, Set.empty)
            (bspAccounts bsp)
    let aic = bspAccountsInCooldown bsp ^. Cooldown.accountsInCooldown
    actualCooldownMap <- Trie.toMap (ReleaseSchedule.nrsMap $ aic ^. Cooldown.cooldown)
    liftIO $ assertEqual "Cooldown map" cooldownMap (ReleaseSchedule.theAccountSet <$> actualCooldownMap)
    actualPreCooldowns <- Set.fromList <$> Cooldown.loadAccountList (aic ^. Cooldown.preCooldown)
    liftIO $ assertEqual "Pre-cooldown set" preCooldowns actualPreCooldowns
    actualPrePreCooldowns <- Set.fromList <$> Cooldown.loadAccountList (aic ^. Cooldown.prePreCooldown)
    liftIO $ assertEqual "Pre-pre-cooldown set" prePreCooldowns actualPrePreCooldowns
    return (reverse theCooldowns)

data AccountConfig (av :: AccountVersion) = AccountConfig
    { acAccountIndex :: AccountIndex,
      acAmount :: Amount,
      acInitialStaking :: StakeDetails av,
      acUpdatedStaking :: StakeDetails av,
      acCooldowns :: Cooldowns
    }
    deriving (Show)

-- | Generate a list of 'AccountConfig's such that:
--
--    * The initial staking includes at least one baker, and there are no delegators to invalid bakers.
--    * The updated staking includes at least one baker, and there are no delegators to invalid bakers.
--    * The amounts on the account are consistent with the active and inactive stake.
--    * The cooldowns only include pre-cooldowns if the 'allowPreCooldown' flag is set.
genAccountConfigs :: (AVSupportsDelegation av) => Bool -> Gen [AccountConfig av]
genAccountConfigs allowPreCooldown = sized $ \n -> do
    let accs = [AccountIndex 0 .. fromIntegral (max 0 n)]
    let chooseBakers = do
            bkrs <- sublistOf accs
            if null bkrs
                then (: []) <$> elements accs
                else return bkrs
    initBakers <- chooseBakers
    updBakers <- chooseBakers
    let genBakerStakeDetails sdStakedCapital = do
            sdRestakeEarnings <- arbitrary
            let sdPendingChange = NoChange
            return StakeDetailsBaker{..}
        genDelegatorStakeDetails sdStakedCapital bakers = do
            sdRestakeEarnings <- arbitrary
            sdDelegationTarget <-
                oneof
                    [ pure DelegatePassive,
                      DelegateToBaker . BakerId <$> elements bakers
                    ]
            let sdPendingChange = NoChange
            return StakeDetailsDelegator{..}
        genCooldowns = do
            inCooldown <- Map.fromList <$> listOf (((,) . Timestamp <$> arbitrary) <*> arbitrary)
            preCooldown <-
                if allowPreCooldown
                    then oneof [return Absent, Present <$> arbitrary]
                    else return Absent
            prePreCooldown <- oneof [return Absent, Present <$> arbitrary]
            return Cooldowns{..}
        genAcc acAccountIndex = do
            initStakeAmount <- Amount <$> choose (1_000_000, 1_000_000_000)
            acInitialStaking <-
                if acAccountIndex `elem` initBakers
                    then genBakerStakeDetails initStakeAmount
                    else
                        oneof
                            [ genDelegatorStakeDetails initStakeAmount initBakers,
                              pure StakeDetailsNone
                            ]
            updatedStakeAmount <- Amount <$> choose (1_000_000, 1_000_000_000)
            acUpdatedStaking <-
                if acAccountIndex `elem` updBakers
                    then genBakerStakeDetails updatedStakeAmount
                    else
                        oneof
                            [ genDelegatorStakeDetails updatedStakeAmount updBakers,
                              pure StakeDetailsNone
                            ]
            acCooldowns <- genCooldowns
            bonusAmount <- Amount <$> choose (0, 1_000_000_000)
            let acAmount = cooldownTotal acCooldowns + updatedStakeAmount + bonusAmount
            return AccountConfig{..}
    mapM genAcc accs

makePersistentAccountStakeEnduring ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av, AVSupportsDelegation av, IsAccountVersion av) =>
    StakeDetails av ->
    AccountIndex ->
    m (SV1.PersistentAccountStakeEnduring av, Amount)
makePersistentAccountStakeEnduring StakeDetailsNone _ = return (SV1.PersistentAccountStakeEnduringNone, 0)
makePersistentAccountStakeEnduring StakeDetailsBaker{..} ai = do
    let (fulBaker, _, _, _) = DummyData.mkFullBaker (fromIntegral ai) (BakerId ai)
    paseBakerInfo <-
        refMake
            BakerInfoExV1
                { _bieBakerInfo = fulBaker ^. bakerInfo,
                  _bieBakerPoolInfo = poolInfo
                }
    return
        ( SV1.PersistentAccountStakeEnduringBaker
            { paseBakerRestakeEarnings = sdRestakeEarnings,
              paseBakerPendingChange = NoChange,
              ..
            },
          sdStakedCapital
        )
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
makePersistentAccountStakeEnduring StakeDetailsDelegator{..} ai = do
    return
        ( SV1.PersistentAccountStakeEnduringDelegator
            { paseDelegatorId = DelegatorId ai,
              paseDelegatorRestakeEarnings = sdRestakeEarnings,
              paseDelegatorTarget = sdDelegationTarget,
              paseDelegatorPendingChange = NoChange
            },
          sdStakedCapital
        )

-- | Create a dummy 'PersistentAccount' from an 'AccountConfig'.
makeDummyAccount ::
    forall av m.
    ( IsAccountVersion av,
      MonadBlobStore m,
      AVSupportsFlexibleCooldown av,
      AVSupportsDelegation av
    ) =>
    AccountConfig av ->
    m (PersistentAccount av)
makeDummyAccount AccountConfig{..} = do
    makeTestAccountFromSeed @av acAmount (fromIntegral acAccountIndex) >>= \case
        PAV3 acc -> do
            let ed = SV1.enduringData acc
            cq <- CooldownQueue.makeCooldownQueue acCooldowns
            (staking, stakeAmount) <- makePersistentAccountStakeEnduring acInitialStaking acAccountIndex
            newEnduring <-
                refMake
                    =<< SV1.rehashAccountEnduringData
                        ed{SV1.paedStakeCooldown = cq, SV1.paedStake = staking}
            return $
                PAV3
                    acc{SV1.accountEnduringData = newEnduring, SV1.accountStakedAmount = stakeAmount}

-- | Construct an initial state for testing based on the account configuration provided.
makeInitialState ::
    forall pv m.
    ( SupportsPersistentState pv m,
      PVSupportsFlexibleCooldown pv,
      IsConsensusV1 pv,
      BlockStateOperations m,
      UpdatableBlockState m ~ PersistentBlockState pv,
      IsSupported 'PTTimeParameters (ChainParametersVersionFor pv) ~ 'True
    ) =>
    -- | Initial configuration of accounts.
    [AccountConfig (AccountVersionFor pv)] ->
    -- | Initial seed state.
    SeedState (SeedStateVersionFor pv) ->
    -- | Length of the reward period.
    RewardPeriodLength ->
    m (PersistentBlockState pv)
makeInitialState accs seedState rpLen = withIsAuthorizationsVersionForPV (protocolVersion @pv) $ do
    initialAccounts <- mapM makeDummyAccount accs
    let chainParams :: ChainParameters pv
        chainParams = DummyData.dummyChainParameters & cpTimeParameters . tpRewardPeriodLength .~ rpLen
    initialBS <-
        initialPersistentState
            seedState
            DummyData.dummyCryptographicParameters
            initialAccounts
            DummyData.dummyIdentityProviders
            DummyData.dummyArs
            DummyData.dummyKeyCollection
            chainParams
    let pbs0 = hpbsPointers initialBS
    (activeBakers, passiveDelegators) <- bsoGetActiveBakersAndDelegators pbs0
    let BakerStakesAndCapital{..} = computeBakerStakesAndCapital (chainParams ^. cpPoolParameters) activeBakers passiveDelegators
    pbs1 <- bsoSetNextEpochBakers pbs0 bakerStakes (chainParams ^. cpFinalizationCommitteeParameters)
    capDist <- capitalDistributionM
    pbs2 <- bsoSetNextCapitalDistribution pbs1 capDist
    pbs <- bsoRotateCurrentCapitalDistribution =<< bsoRotateCurrentEpochBakers pbs2

    bsp <- loadPBS pbs
    -- Now we update the accounts with the updated staking information.
    let
        updateAccountStake ::
            AccountConfig (AccountVersionFor pv) ->
            PersistentAccount (AccountVersionFor pv) ->
            m (PersistentAccount (AccountVersionFor pv))
        updateAccountStake AccountConfig{..} (PAV3 pa) = do
            (staking, newStakeAmount) <- makePersistentAccountStakeEnduring acUpdatedStaking acAccountIndex
            let ed = SV1.enduringData pa
            newEnduring <- refMake =<< SV1.rehashAccountEnduringData ed{SV1.paedStake = staking}
            return $ PAV3 pa{SV1.accountEnduringData = newEnduring, SV1.accountStakedAmount = newStakeAmount}
    newAccounts <-
        foldM
            (\a ac -> updateAccountsAtIndex' (updateAccountStake ac) (acAccountIndex ac) a)
            (bspAccounts bsp)
            accs
    accts <- foldAccountsDesc (\l acc -> return (acc : l)) [] newAccounts
    newBirkParameters <- initialBirkParameters accts seedState (chainParams ^. cpFinalizationCommitteeParameters)
    storePBS pbs bsp{bspAccounts = newAccounts, bspBirkParameters = newBirkParameters}

transitionalSeedState :: Epoch -> Timestamp -> SeedState SeedStateVersion1
transitionalSeedState curEpoch triggerTime =
    (initialSeedStateV1 (Hash.hash "NONCE") triggerTime)
        { ss1Epoch = curEpoch,
          ss1EpochTransitionTriggered = True
        }

-- | Test an epoch transition with no payday or snapshot.
testEpochTransitionNoPaydayNoSnapshot :: [AccountConfig 'AccountV3] -> Assertion
testEpochTransitionNoPaydayNoSnapshot accountConfigs = runTestBlockState @P7 $ do
    bs0 <- makeInitialState accountConfigs (transitionalSeedState startEpoch startTriggerTime) 2
    initCapDist <- bsoGetCurrentCapitalDistribution bs0
    initBakers <- bsoGetCurrentEpochFullBakersEx bs0
    bs1 <- bsoSetPaydayEpoch bs0 (startEpoch + 10)
    (mPaydayParams, resState) <- doEpochTransition True hour bs1
    liftIO $ assertEqual "Payday parameters" Nothing mPaydayParams
    newCooldowns <- checkCooldowns resState
    liftIO $ assertEqual "Cooldowns should be unchanged" (map acCooldowns accountConfigs) newCooldowns
    ss <- bsoGetSeedState resState
    case ss of
        SeedStateV1{..} -> liftIO $ do
            assertEqual "Epoch should be updated" (startEpoch + 1) ss1Epoch
            assertEqual
                "Trigger time should be updated"
                (addDuration startTriggerTime hour)
                ss1TriggerBlockTime
            assertEqual
                "Epoch transition should no longer be triggered"
                False
                ss1EpochTransitionTriggered
    finalCapDist <- bsoGetCurrentCapitalDistribution resState
    liftIO $ assertEqual "Capital distribution should be unchanged" initCapDist finalCapDist
    finalBakers <- bsoGetCurrentEpochFullBakersEx resState
    liftIO $ assertEqual "Bakers should be unchanged" initBakers finalBakers
    updBakers <- bsoRotateCurrentCapitalDistribution =<< bsoRotateCurrentEpochBakers resState
    finalNECapDist <- bsoGetCurrentCapitalDistribution updBakers
    liftIO $ assertEqual "Next-epoch capital distribution should be unchanged" initCapDist finalNECapDist
    finalNEBakers <- bsoGetCurrentEpochFullBakersEx updBakers
    liftIO $ assertEqual "Next-epoch bakers should be unchanged" initBakers finalNEBakers
  where
    hour = Duration 3_600_000
    startEpoch = 10
    startTriggerTime = 1000

-- | Test a payday epoch transition.
testEpochTransitionPaydayOnly :: [AccountConfig 'AccountV3] -> Assertion
testEpochTransitionPaydayOnly accountConfigs = runTestBlockState @P7 $ do
    bs0 <- makeInitialState accountConfigs (transitionalSeedState startEpoch startTriggerTime) 2
    initCapDist <- bsoGetCurrentCapitalDistribution bs0
    initBakers <- bsoGetCurrentEpochFullBakersEx bs0
    bs1 <- bsoSetPaydayEpoch bs0 (startEpoch + 1)
    (mPaydayParams, resState) <- doEpochTransition True hour bs1
    liftIO $ case mPaydayParams of
        Just PaydayParameters{..} -> do
            assertEqual "Payday capital distribution" initCapDist paydayCapitalDistribution
            assertEqual "Payday bakers" initBakers paydayBakers
        Nothing -> do
            assertFailure "Expected payday parameters"
    newCooldowns <- checkCooldowns resState
    let processCooldownAtPayday =
            processPreCooldown (startTriggerTime `addDurationSeconds` cooldownDuration)
                . processCooldowns startTriggerTime
    liftIO $
        assertEqual
            "Expired cooldowns should be processed and pre-cooldowns should be moved to cooldowns"
            (map (processCooldownAtPayday . acCooldowns) accountConfigs)
            newCooldowns
    ss <- bsoGetSeedState resState
    case ss of
        SeedStateV1{..} -> liftIO $ do
            assertEqual "Epoch should be updated" (startEpoch + 1) ss1Epoch
            assertEqual
                "Trigger time should be updated"
                (addDuration startTriggerTime hour)
                ss1TriggerBlockTime
            assertEqual
                "Epoch transition should no longer be triggered"
                False
                ss1EpochTransitionTriggered
    finalCapDist <- bsoGetCurrentCapitalDistribution resState
    liftIO $ assertEqual "Capital distribution should be unchanged" initCapDist finalCapDist
    finalBakers <- bsoGetCurrentEpochFullBakersEx resState
    liftIO $ assertEqual "Bakers should be unchanged" initBakers finalBakers
    updBakers <- bsoRotateCurrentCapitalDistribution =<< bsoRotateCurrentEpochBakers resState
    finalNECapDist <- bsoGetCurrentCapitalDistribution updBakers
    liftIO $ assertEqual "Next-epoch capital distribution should be unchanged" initCapDist finalNECapDist
    finalNEBakers <- bsoGetCurrentEpochFullBakersEx updBakers
    liftIO $ assertEqual "Next-epoch bakers should be unchanged" initBakers finalNEBakers
  where
    hour = Duration 3_600_000
    startEpoch = 10
    startTriggerTime = 1000
    cooldownDuration =
        DummyData.dummyChainParameters @ChainParametersV2
            ^. cpCooldownParameters . cpUnifiedCooldown

-- | Test an snapshot epoch transition.
testEpochTransitionSnapshotOnly :: [AccountConfig 'AccountV3] -> Assertion
testEpochTransitionSnapshotOnly accountConfigs = runTestBlockState @P7 $ do
    bs0 <- makeInitialState accountConfigs (transitionalSeedState startEpoch startTriggerTime) 2
    initCapDist <- bsoGetCurrentCapitalDistribution bs0
    initBakers <- bsoGetCurrentEpochFullBakersEx bs0
    bs1 <- bsoSetPaydayEpoch bs0 (startEpoch + 2)
    (activeBakers, activeDelegators) <- bsoGetActiveBakersAndDelegators bs1
    let BakerStakesAndCapital{..} =
            computeBakerStakesAndCapital
                (chainParams ^. cpPoolParameters)
                activeBakers
                activeDelegators
    updatedCapitalDistr <- capitalDistributionM
    let mkFullBaker (ref, stake) = do
            loadPersistentBakerInfoRef @_ @'AccountV3 ref <&> \case
                (BakerInfoExV1 info extra) ->
                    FullBakerInfoEx
                        { _exFullBakerInfo = FullBakerInfo info stake,
                          _bakerPoolCommissionRates = extra ^. poolCommissionRates
                        }
    bkrs <- mapM mkFullBaker bakerStakes
    let updatedBakerStakes = FullBakersEx (Vec.fromList bkrs) (sum $ snd <$> bakerStakes)

    (mPaydayParams, resState) <- doEpochTransition True hour bs1
    liftIO $ assertEqual "Payday parameters" Nothing mPaydayParams
    newCooldowns <- checkCooldowns resState
    liftIO $
        assertEqual
            "Expired cooldowns should be processed and pre-cooldowns should be moved to cooldowns"
            (map (processPrePreCooldown . acCooldowns) accountConfigs)
            newCooldowns
    ss <- bsoGetSeedState resState
    case ss of
        SeedStateV1{..} -> liftIO $ do
            assertEqual "Epoch should be updated" (startEpoch + 1) ss1Epoch
            assertEqual
                "Trigger time should be updated"
                (addDuration startTriggerTime hour)
                ss1TriggerBlockTime
            assertEqual
                "Epoch transition should no longer be triggered"
                False
                ss1EpochTransitionTriggered
    finalCapDist <- bsoGetCurrentCapitalDistribution resState
    liftIO $ assertEqual "Capital distribution should be unchanged" initCapDist finalCapDist
    finalBakers <- bsoGetCurrentEpochFullBakersEx resState
    liftIO $ assertEqual "Bakers should be unchanged" initBakers finalBakers
    updBakers <- bsoRotateCurrentCapitalDistribution =<< bsoRotateCurrentEpochBakers resState
    finalNECapDist <- bsoGetCurrentCapitalDistribution updBakers
    liftIO $ assertEqual "Next-epoch capital distribution should be updated" updatedCapitalDistr finalNECapDist
    finalNEBakers <- bsoGetCurrentEpochFullBakersEx updBakers
    liftIO $ assertEqual "Next-epoch bakers should be updated" updatedBakerStakes finalNEBakers
  where
    hour = Duration 3_600_000
    startEpoch = 10
    startTriggerTime = 1000
    chainParams = DummyData.dummyChainParameters @ChainParametersV2

-- | Test two successive epoch transitions where the first is a snapshot and the second is a payday.
testEpochTransitionSnapshotPayday :: [AccountConfig 'AccountV3] -> Assertion
testEpochTransitionSnapshotPayday accountConfigs = runTestBlockState @P7 $ do
    bs0 <- makeInitialState accountConfigs (transitionalSeedState startEpoch startTriggerTime) 2
    initCapDist <- bsoGetCurrentCapitalDistribution bs0
    initBakers <- bsoGetCurrentEpochFullBakersEx bs0
    bs1 <- bsoSetPaydayEpoch bs0 (startEpoch + 2)
    (activeBakers, activeDelegators) <- bsoGetActiveBakersAndDelegators bs1
    let BakerStakesAndCapital{..} =
            computeBakerStakesAndCapital
                (chainParams ^. cpPoolParameters)
                activeBakers
                activeDelegators
    updatedCapitalDistr <- capitalDistributionM
    let mkFullBaker (ref, stake) = do
            loadPersistentBakerInfoRef @_ @'AccountV3 ref <&> \case
                (BakerInfoExV1 info extra) ->
                    FullBakerInfoEx
                        { _exFullBakerInfo = FullBakerInfo info stake,
                          _bakerPoolCommissionRates = extra ^. poolCommissionRates
                        }
    bkrs <- mapM mkFullBaker bakerStakes
    let updatedBakerStakes = FullBakersEx (Vec.fromList bkrs) (sum $ snd <$> bakerStakes)

    (mPaydayParams, snapshotState) <- doEpochTransition True hour bs1
    liftIO $ assertEqual "Payday parameters" Nothing mPaydayParams
    newCooldowns <- checkCooldowns snapshotState
    let expectCooldowns1 = processPrePreCooldown . acCooldowns <$> accountConfigs
    liftIO $
        assertEqual
            "Pre-pre-cooldowns should be moved to pre-cooldown"
            expectCooldowns1
            newCooldowns
    ss <- bsoGetSeedState snapshotState
    case ss of
        SeedStateV1{..} -> liftIO $ do
            assertEqual "Epoch should be updated" (startEpoch + 1) ss1Epoch
            assertEqual
                "Trigger time should be updated"
                (addDuration startTriggerTime hour)
                ss1TriggerBlockTime
            assertEqual
                "Epoch transition should no longer be triggered"
                False
                ss1EpochTransitionTriggered
    snapshotCapDist <- bsoGetCurrentCapitalDistribution snapshotState
    liftIO $ assertEqual "Capital distribution should be unchanged" initCapDist snapshotCapDist
    snapshotBakers <- bsoGetCurrentEpochFullBakersEx snapshotState
    liftIO $ assertEqual "Bakers should be unchanged" initBakers snapshotBakers

    (mPaydayParams', resState) <- doEpochTransition True hour snapshotState
    liftIO $ case mPaydayParams' of
        Just PaydayParameters{..} -> do
            assertEqual "Payday capital distribution" initCapDist paydayCapitalDistribution
            assertEqual "Payday bakers" initBakers paydayBakers
        Nothing -> do
            assertFailure "Expected payday parameters"
    newCooldowns' <- checkCooldowns resState
    let paydayTime = startTriggerTime `addDuration` hour
    let processCooldownAtPayday =
            processPreCooldown (paydayTime `addDurationSeconds` cooldownDuration)
                . processCooldowns paydayTime
    let expectCooldowns2 = processCooldownAtPayday <$> expectCooldowns1
    liftIO $
        assertEqual
            "Expired cooldowns should be processed and pre-cooldowns should be moved to cooldowns"
            expectCooldowns2
            newCooldowns'
    finalCapDist <- bsoGetCurrentCapitalDistribution resState
    liftIO $ assertEqual "Capital distribution should be updated" updatedCapitalDistr finalCapDist
    finalBakers <- bsoGetCurrentEpochFullBakersEx resState
    liftIO $ assertEqual "Bakers should be updated" updatedBakerStakes finalBakers

    updBakers <- bsoRotateCurrentCapitalDistribution =<< bsoRotateCurrentEpochBakers resState
    finalNECapDist <- bsoGetCurrentCapitalDistribution updBakers
    liftIO $ assertEqual "Next-epoch capital distribution should be updated" updatedCapitalDistr finalNECapDist
    finalNEBakers <- bsoGetCurrentEpochFullBakersEx updBakers
    liftIO $ assertEqual "Next-epoch bakers should be updated" updatedBakerStakes finalNEBakers
  where
    hour = Duration 3_600_000
    startEpoch = 10
    startTriggerTime = 1000
    chainParams = DummyData.dummyChainParameters @ChainParametersV2
    cooldownDuration = chainParams ^. cpCooldownParameters . cpUnifiedCooldown

-- | Test epoch transitions for two successive transitions where the payday length is one epoch.
-- In this case, both the snapshot and payday processing occur on each transition, so this tests
-- that they are correctly ordered.
testEpochTransitionSnapshotPaydayCombo :: [AccountConfig 'AccountV3] -> Assertion
testEpochTransitionSnapshotPaydayCombo accountConfigs = runTestBlockState @P7 $ do
    -- Setup the initial state.
    bs0 <- makeInitialState accountConfigs (transitionalSeedState startEpoch startTriggerTime) 1
    initCapDist <- bsoGetCurrentCapitalDistribution bs0
    initBakers <- bsoGetCurrentEpochFullBakersEx bs0
    bs1 <- bsoSetPaydayEpoch bs0 (startEpoch + 1)
    (activeBakers, activeDelegators) <- bsoGetActiveBakersAndDelegators bs1
    let BakerStakesAndCapital{..} =
            computeBakerStakesAndCapital
                (chainParams ^. cpPoolParameters)
                activeBakers
                activeDelegators
    updatedCapitalDistr <- capitalDistributionM
    let mkFullBaker (ref, stake) = do
            loadPersistentBakerInfoRef @_ @'AccountV3 ref <&> \case
                (BakerInfoExV1 info extra) ->
                    FullBakerInfoEx
                        { _exFullBakerInfo = FullBakerInfo info stake,
                          _bakerPoolCommissionRates = extra ^. poolCommissionRates
                        }
    bkrs <- mapM mkFullBaker bakerStakes
    let updatedBakerStakes = FullBakersEx (Vec.fromList bkrs) (sum $ snd <$> bakerStakes)

    -- First epoch transition.
    (mPaydayParams, snapshotState) <- doEpochTransition True hour bs1
    liftIO $ case mPaydayParams of
        Just PaydayParameters{..} -> do
            assertEqual "Payday capital distribution (1)" initCapDist paydayCapitalDistribution
            assertEqual "Payday bakers (1)" initBakers paydayBakers
        Nothing -> do
            assertFailure "Expected payday parameters (1)"
    newCooldowns1 <- checkCooldowns snapshotState
    let processPaydayCooldowns paydayTime =
            processPrePreCooldown
                . processPreCooldown (paydayTime `addDurationSeconds` cooldownDuration)
                . processCooldowns paydayTime
    let expectedCooldowns1 = processPaydayCooldowns startTriggerTime . acCooldowns <$> accountConfigs
    liftIO $
        assertEqual
            "Cooldowns should be processed at payday (1)"
            expectedCooldowns1
            newCooldowns1
    ss <- bsoGetSeedState snapshotState
    case ss of
        SeedStateV1{..} -> liftIO $ do
            assertEqual "Epoch should be updated (1)" (startEpoch + 1) ss1Epoch
            assertEqual
                "Trigger time should be updated (1)"
                (addDuration startTriggerTime hour)
                ss1TriggerBlockTime
            assertEqual
                "Epoch transition should no longer be triggered (1)"
                False
                ss1EpochTransitionTriggered
    snapshotCapDist <- bsoGetCurrentCapitalDistribution snapshotState
    liftIO $ assertEqual "Capital distribution should be unchanged (1)" initCapDist snapshotCapDist
    snapshotBakers <- bsoGetCurrentEpochFullBakersEx snapshotState
    liftIO $ assertEqual "Bakers should be unchanged (1)" initBakers snapshotBakers

    -- Second epoch transition.
    let payday2Time = startTriggerTime `addDuration` hour
    (mPaydayParams', resState) <- doEpochTransition True hour snapshotState
    liftIO $ case mPaydayParams' of
        Just PaydayParameters{..} -> do
            assertEqual "Payday capital distribution" initCapDist paydayCapitalDistribution
            assertEqual "Payday bakers" initBakers paydayBakers
        Nothing -> do
            assertFailure "Expected payday parameters"
    newCooldowns2 <- checkCooldowns snapshotState
    let expectedCooldowns2 = processPaydayCooldowns payday2Time <$> expectedCooldowns1
    liftIO $
        assertEqual
            "Cooldowns should be processed at payday (2)"
            expectedCooldowns2
            newCooldowns2
    bsoGetSeedState snapshotState >>= \case
        SeedStateV1{..} -> liftIO $ do
            assertEqual "Epoch should be updated (2)" (startEpoch + 2) ss1Epoch
            assertEqual
                "Trigger time should be updated (2)"
                (addDuration payday2Time hour)
                ss1TriggerBlockTime
            assertEqual
                "Epoch transition should no longer be triggered (2)"
                False
                ss1EpochTransitionTriggered
    finalCapDist <- bsoGetCurrentCapitalDistribution resState
    liftIO $ assertEqual "Capital distribution should be updated (2)" updatedCapitalDistr finalCapDist
    finalBakers <- bsoGetCurrentEpochFullBakersEx resState
    liftIO $ assertEqual "Bakers should be updated (2)" updatedBakerStakes finalBakers
    updBakers <- bsoRotateCurrentCapitalDistribution =<< bsoRotateCurrentEpochBakers resState
    finalNECapDist <- bsoGetCurrentCapitalDistribution updBakers
    liftIO $ assertEqual "Next-epoch capital distribution should be updated (2)" updatedCapitalDistr finalNECapDist
    finalNEBakers <- bsoGetCurrentEpochFullBakersEx updBakers
    liftIO $ assertEqual "Next-epoch bakers should be updated (2)" updatedBakerStakes finalNEBakers
  where
    hour = Duration 3_600_000
    startEpoch = 10
    startTriggerTime = 1000
    chainParams = DummyData.dummyChainParameters @ChainParametersV2
    cooldownDuration = chainParams ^. cpCooldownParameters . cpUnifiedCooldown

tests :: Spec
tests = parallel $ describe "EpochTransition" $ do
    it "testEpochTransitionNoPaydayNoSnapshot" $
        forAll (genAccountConfigs True) testEpochTransitionNoPaydayNoSnapshot
    it "testEpochTransitionPaydayOnly" $
        forAll (genAccountConfigs True) testEpochTransitionPaydayOnly
    it "testEpochTransitionSnapshotOnly" $
        forAll (genAccountConfigs False) testEpochTransitionSnapshotOnly
    it "testEpochTransitionSnapshotPayday" $
        forAll (genAccountConfigs False) testEpochTransitionSnapshotPayday
    it "testEpochTransitionSnapshotPaydayCombo" $
        forAll (genAccountConfigs False) testEpochTransitionSnapshotPaydayCombo
