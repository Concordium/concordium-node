{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Common functions for testing operations on the block state.
module GlobalStateTests.BlockStateHelpers where

import Control.Exception
import Control.Monad.IO.Class
import Data.Bool.Singletons
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

-- | A configuration for an account, specifying the account index, amount, staking details and
--  cooldowns. This is used to create accounts for testing.
data AccountConfig (av :: AccountVersion) = AccountConfig
    { acAccountIndex :: AccountIndex,
      acAmount :: Amount,
      acStaking :: StakeDetails av,
      acCooldowns :: Cooldowns
    }
    deriving (Show)

-- | Set the staking details for an account.
setAccountStakeDetails ::
    (MonadBlobStore m, AVSupportsDelegation av, IsAccountVersion av) =>
    AccountIndex ->
    StakeDetails av ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountStakeDetails _ StakeDetailsNone acc = return acc
setAccountStakeDetails ai StakeDetailsBaker{..} acc =
    setAccountStakePendingChange sdPendingChange
        =<< addAccountBakerV1 bie sdStakedCapital sdRestakeEarnings acc
  where
    bie =
        BakerInfoExV1
            { _bieBakerInfo = fulBaker ^. bakerInfo,
              _bieBakerPoolInfo = poolInfo
            }
    fulBaker = DummyData.mkFullBaker (fromIntegral ai) (BakerId ai) ^. _1
    poolInfo =
        BakerPoolInfo
            { _poolOpenStatus = OpenForAll,
              _poolMetadataUrl = UrlText "Some URL",
              _poolCommissionRates =
                -- Note: these commission rates are significant for the ConfigureValidator tests
                CommissionRates
                    { _finalizationCommission = makeAmountFraction 350,
                      _bakingCommission = makeAmountFraction 550,
                      _transactionCommission = makeAmountFraction 150
                    }
            }
setAccountStakeDetails ai StakeDetailsDelegator{..} acc =
    setAccountStakePendingChange sdPendingChange =<< addAccountDelegator del acc
  where
    del =
        AccountDelegationV1
            { _delegationTarget = sdDelegationTarget,
              _delegationStakedAmount = sdStakedCapital,
              _delegationStakeEarnings = sdRestakeEarnings,
              _delegationPendingChange = NoChange,
              _delegationIdentity = DelegatorId ai
            }

-- | Create a dummy 'PersistentAccount' from an 'AccountConfig'.
makeDummyAccount ::
    forall av m.
    ( IsAccountVersion av,
      MonadBlobStore m,
      SupportsDelegation av ~ 'True
    ) =>
    AccountConfig av ->
    m (PersistentAccount av)
makeDummyAccount AccountConfig{..} = do
    acc0 <- makeTestAccountFromSeed @av acAmount (fromIntegral acAccountIndex)
    acc1 <- setAccountStakeDetails acAccountIndex acStaking acc0
    case sSupportsFlexibleCooldown (accountVersion @av) of
        STrue -> case acc1 of
            PAV3 acc -> do
                let ed = SV1.enduringData acc
                cq <- CooldownQueue.makeCooldownQueue acCooldowns
                newEnduring <-
                    refMake
                        =<< SV1.rehashAccountEnduringData
                            ed{SV1.paedStakeCooldown = cq}
                return $
                    PAV3
                        acc{SV1.accountEnduringData = newEnduring}
        SFalse -> return acc1

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
