{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.BlockStateHelpers where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro
import System.FilePath
import System.IO.Temp
import Test.HUnit

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Execution
import Concordium.Types.Option

import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
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
import Concordium.Scheduler.DummyData

import GlobalStateTests.Accounts (NoLoggerT (..), runNoLoggerT)

-- | Construct a dummy account with the specified cooldowns.
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

data AccountConfig (av :: AccountVersion) = AccountConfig
    { acAccountIndex :: AccountIndex,
      acAmount :: Amount,
      acStaking :: StakeDetails av,
      acCooldowns :: Cooldowns
    }
    deriving (Show)

makePersistentAccountStakeEnduring ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av, AVSupportsDelegation av, IsAccountVersion av) =>
    StakeDetails av ->
    AccountIndex ->
    m (SV1.PersistentAccountStakeEnduring av, Amount)
makePersistentAccountStakeEnduring StakeDetailsNone _ = return (SV1.PersistentAccountStakeEnduringNone, 0)
makePersistentAccountStakeEnduring StakeDetailsBaker{..} ai = do
    let fulBaker = DummyData.mkFullBaker (fromIntegral ai) (BakerId ai) ^. _1
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
      SupportsFlexibleCooldown av ~ 'True
    ) =>
    AccountConfig av ->
    m (PersistentAccount av)
makeDummyAccount AccountConfig{..} = do
    makeTestAccountFromSeed @av acAmount (fromIntegral acAccountIndex) >>= \case
        PAV3 acc -> do
            let ed = SV1.enduringData acc
            cq <- CooldownQueue.makeCooldownQueue acCooldowns
            (staking, stakeAmount) <- makePersistentAccountStakeEnduring acStaking acAccountIndex
            newEnduring <-
                refMake
                    =<< SV1.rehashAccountEnduringData
                        ed{SV1.paedStakeCooldown = cq, SV1.paedStake = staking}
            return $
                PAV3
                    acc{SV1.accountEnduringData = newEnduring, SV1.accountStakedAmount = stakeAmount}

-- | Run a block state computation using a temporary directory for the blob store and account map.
runTestBlockState ::
    forall pv a.
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (BlobStoreT (PersistentBlockStateContext pv) (NoLoggerT IO))
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
        (runNoLoggerT . runBlobStoreT (runPersistentBlockStateMonad kont))

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
