{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the processing of cooldowns in the global state.
--  Specifically, it tests the 'bsoProcessPrePreCooldowns' and 'bsoProcessCooldowns' functions.
module GlobalStateTests.CooldownProcessing where

import Control.Monad.IO.Class
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Option
import Concordium.Types.SeedState

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CooldownQueue
import qualified Concordium.GlobalState.DummyData as DummyData
import Concordium.GlobalState.Persistent.BlockState

import GlobalStateTests.Accounts ()
import GlobalStateTests.BlockStateHelpers
import GlobalStateTests.CooldownQueue (genCooldowns)

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
