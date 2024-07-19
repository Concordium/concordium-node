{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the processing of cooldowns in the global state.
--  Specifically, it tests the 'bsoProcessPrePreCooldowns' and 'bsoProcessCooldowns' functions.
module GlobalStateTests.CooldownProcessing where

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
import Concordium.Types
import Concordium.Types.Option
import Concordium.Types.SeedState

import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
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
import Concordium.Scheduler.DummyData

import GlobalStateTests.Accounts (NoLoggerT (..), runNoLoggerT)
import GlobalStateTests.CooldownQueue (genCooldowns)

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

-- | Test 'bsoProcessCooldowns' with a state where the account cooldowns are as given.
propProcessPrePreCooldowns :: [Cooldowns] -> Assertion
propProcessPrePreCooldowns cds = runTestBlockState @P7 $ do
    let mkAcct (i, cd) = dummyCooldownAccount i (cooldownTotal cd + 1000) cd
    initialAccts <- mapM mkAcct (zip [0 ..] cds)
    initialBS <-
        initialPersistentState
            (initialSeedStateV1 (Hash.hash "NONCE") 1000)
            DummyData.dummyCryptographicParameters
            initialAccts
            DummyData.dummyIdentityProviders
            DummyData.dummyArs
            DummyData.dummyKeyCollection
            DummyData.dummyChainParameters
    bs' <- bsoProcessPrePreCooldowns (hpbsPointers initialBS)
    newCooldowns <- checkCooldowns bs'
    liftIO $ assertEqual "Cooldowns" (processPrePreCooldown <$> cds) newCooldowns

propProcessCooldowns :: [Cooldowns] -> Timestamp -> Timestamp -> Assertion
propProcessCooldowns cds expire new = runTestBlockState @P7 $ do
    let mkAcct (i, cd) = dummyCooldownAccount i (cooldownTotal cd + 1000) cd
    initialAccts <- mapM mkAcct (zip [0 ..] cds)
    initialBS <-
        initialPersistentState
            (initialSeedStateV1 (Hash.hash "NONCE") 1000)
            DummyData.dummyCryptographicParameters
            initialAccts
            DummyData.dummyIdentityProviders
            DummyData.dummyArs
            DummyData.dummyKeyCollection
            DummyData.dummyChainParameters
    bs' <- bsoProcessCooldowns (hpbsPointers initialBS) expire new
    newCooldowns <- checkCooldowns bs'
    liftIO $ assertEqual "Cooldowns" (processPreCooldown new . processCooldowns expire <$> cds) newCooldowns

-- | Generate a 'Cooldowns' with no pre-cooldown.
genCooldownsNoPre :: Gen Cooldowns
genCooldownsNoPre = do
    cooldown <- genCooldowns
    return cooldown{preCooldown = Absent}

-- | Test 'bsoProcessPrePreCooldowns'.
testProcessPrePreCooldowns :: Spec
testProcessPrePreCooldowns = do
    it "10 accounts, no cooldowns" $ propProcessPrePreCooldowns (replicate 10 emptyCooldowns)
    it "5 accounts no cooldowns, 5 accounts with pre-pre-cooldown" $
        propProcessPrePreCooldowns $
            replicate 5 emptyCooldowns ++ replicate 5 (emptyCooldowns{prePreCooldown = Present 1000})
    it "accounts with arbitray cooldowns (but no pre-cooldown)" $
        forAll (listOf genCooldownsNoPre) propProcessPrePreCooldowns

-- | Test 'bsoProcessCooldowns'.
testProcessCooldowns :: Spec
testProcessCooldowns = do
    it "accounts with arbitrary cooldowns" $
        forAll (listOf genCooldowns) $ \cds -> do
            forAll arbitrary $ \expire ->
                forAll arbitrary $ \new ->
                    propProcessCooldowns cds (Timestamp expire) (Timestamp new)

tests :: Spec
tests = describe "CooldownProcessing" $ do
    describe "bsoProcessPrePreCooldowns" testProcessPrePreCooldowns
    describe "bsoProcessCooldowns" testProcessCooldowns
