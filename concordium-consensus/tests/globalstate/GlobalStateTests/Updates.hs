{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module GlobalStateTests.Updates (tests) where

{- What I want to test:
1. starting from an empty blockstate with the dummy parameters, try to register a baker with not enough stake. (MUST FAIL)
2. starting from an empty blockstate with the dummy parameters, register a baker with enough stake and
    2.1. decrease the stake below the limit. (MUST FAIL)
    2.2. decrease the stake above the limit. (MUST SUCCEED)
    2.3. increase the stake. (MUST SUCCEED)
3. starting from an empty blockstate with the dummy parameters, register a baker with enough stake, increase the limit and
    3.1. decrease the stake any amount (MUST FAIL)
    3.2. increase the stake below limit (MUST SUCCEED)
    3.3. increase the stake over limit (MUST SUCCEED)
-}

import Concordium.GlobalState.Account
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.Types
import Concordium.Types.Execution
import Control.Monad.RWS.Strict as RWS hiding (state)
import Data.IORef
import Data.Maybe (fromJust)
import Lens.Micro.Platform
import Test.Hspec

import Concordium.Common.Time
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Persistent.BlockState.Updates as PU
import Concordium.ID.DummyData
import Concordium.ID.Parameters
import Concordium.Types.DummyData
import Concordium.Types.SeedState (initialSeedState)
import Test.HUnit (assertEqual)

--------------------------------------------------------------------------------
--                                                                            --
--                        Concrete monads for the test                        --
--                                                                            --
--------------------------------------------------------------------------------

type PV = 'P5

type ThisMonadConcrete pv =
    PBS.PersistentBlockStateMonad
        pv
        (PBS.PersistentBlockStateContext pv)
        (BlobStoreM' (PBS.PersistentBlockStateContext pv))

--------------------------------------------------------------------------------
--                                                                            --
--                           Create and destroy GS                            --
--                                                                            --
--------------------------------------------------------------------------------

createGS :: ThisMonadConcrete PV (PBS.PersistentBlockState PV)
createGS = do
    PBS.hpbsPointers
        <$> PBS.initialPersistentState
            (initialSeedState (Hash.hash "") 1_000)
            dummyCryptographicParameters
            []
            dummyIdentityProviders
            dummyArs
            dummyKeyCollection
            dummyChainParameters

--------------------------------------------------------------------------------
--                                                                            --
--                                 Constants                                  --
--                                                                            --
--------------------------------------------------------------------------------

limit :: Amount
limit = dummyChainParameters @'ChainParametersV0 ^. cpPoolParameters . ppBakerStakeThreshold
limitDelta :: AmountDelta
limitDelta = fromIntegral limit

-- --------------------------------------------------------------------------------
-- --                                                                            --
-- --                           Block state operations                           --
-- --                                                                            --
-- --------------------------------------------------------------------------------

-- | Create the thomasAccount with a dummy credential and the provided amount. Return the account index of the newly
-- created account and the updated block state. Note that this account is valid, it is not a baker nor a delegator.
createAccountWith :: AmountDelta -> PBS.PersistentBlockState PV -> ThisMonadConcrete PV (PBS.PersistentBlockState PV, AccountIndex)
createAccountWith a bs = do
    (_, bs') <-
        bsoCreateAccount
            bs
            dummyGlobalContext
            thomasAccount
            ( dummyCredential
                dummyGlobalContext
                thomasAccount
                thomasVK
                (YearMonth 2021 01)
                (YearMonth 2021 12)
            )
    accIndex <- fromJust <$> bsoGetAccountIndex bs' thomasAccount
    (,accIndex) <$> bsoModifyAccount bs' (emptyAccountUpdate accIndex & auAmount ?~ a)

-- | Add a baker with the given staked amount.
addBakerWith :: Amount -> (PBS.PersistentBlockState PV, AccountIndex) -> ThisMonadConcrete PV (BakerConfigureResult, (PBS.PersistentBlockState PV, AccountIndex))
addBakerWith am (bs, ai) = do
    a <- BlockSig.verifyKey <$> liftIO BlockSig.newKeyPair
    b <- Bls.derivePublicKey <$> liftIO Bls.generateSecretKey
    c <- VRF.publicKey <$> liftIO VRF.newKeyPair
    let conf =
            BakerConfigureAdd
                { bcaKeys = BakerKeyUpdate a b c,
                  bcaCapital = am,
                  bcaRestakeEarnings = False,
                  bcaOpenForDelegation = ClosedForAll,
                  bcaMetadataURL = emptyUrlText,
                  bcaTransactionFeeCommission = makeAmountFraction 0,
                  bcaBakingRewardCommission = makeAmountFraction 0,
                  bcaFinalizationRewardCommission = makeAmountFraction 0
                }
    (bar, bs') <- bsoConfigureBaker bs ai conf
    return (bar, (bs', ai))

-- |Modify the staked amount to the given value.
modifyStakeTo :: Amount -> (PBS.PersistentBlockState PV, AccountIndex) -> ThisMonadConcrete PV (BakerConfigureResult, (PBS.PersistentBlockState PV, AccountIndex))
modifyStakeTo a (bs, ai) = do
    let conf =
            BakerConfigureUpdate
                { bcuSlotTimestamp = 0,
                  bcuKeys = Nothing,
                  bcuCapital = Just a,
                  bcuRestakeEarnings = Nothing,
                  bcuOpenForDelegation = Nothing,
                  bcuMetadataURL = Nothing,
                  bcuTransactionFeeCommission = Nothing,
                  bcuBakingRewardCommission = Nothing,
                  bcuFinalizationRewardCommission = Nothing
                }
    (bsur, bs') <- bsoConfigureBaker bs ai conf
    return (bsur, (bs', ai))

-- |Increase the current threshold for baking. This uses some trickery to run a
-- side monad that will be a MonadBlobStore that can retrieve the required
-- fields from the persistent block state and write them again after modifying
-- them. This is quite ad-hoc and probably should not be replicated in other
-- tests.
increaseLimit :: Amount -> (PBS.PersistentBlockState PV, AccountIndex) -> ThisMonadConcrete PV (PBS.PersistentBlockState PV, AccountIndex)
increaseLimit newLimit (bs2, ai) = do
    -- load the block from the IORef
    bsp <- refLoad =<< liftIO (readIORef bs2)
    -- load the updates field
    updates <- refLoad (PBS.bspUpdates bsp)
    -- load the current parameters
    currentParams <- unStoreSerialized <$> refLoad (PU.currentParameters updates)
    -- store the new parameters
    newParams <- refMake $ StoreSerialized (currentParams & cpPoolParameters . ppMinimumEquityCapital .~ newLimit)
    -- store the new updates
    newUpdates <- refMake (updates{PU.currentParameters = newParams})
    -- store the new block in the IORef
    liftIO . writeIORef bs2 =<< refMake (bsp{PBS.bspUpdates = newUpdates})
    return (bs2, ai)

-- --------------------------------------------------------------------------------
-- --                                                                            --
-- --                                   Tests                                    --
-- --                                                                            --
-- --------------------------------------------------------------------------------

testing1 :: ThisMonadConcrete PV ()
-- starting from an empty blockstate with the dummy parameters, try to register
-- a baker with not enough stake. (MUST FAIL)
testing1 = do
    (res, _) <-
        createGS
            >>= createAccountWith (limitDelta `div` 2)
            >>= addBakerWith (limit `div` 2)
    case res of
        BCStakeUnderThreshold -> return ()
        e -> error $ "Got (" ++ show e ++ ") but wanted BCStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake and decrease the stake below the limit. (MUST FAIL)
testing2'1 :: ThisMonadConcrete PV ()
testing2'1 = do
    (res, _) <-
        createGS
            >>= createAccountWith limitDelta
            >>= addBakerWith limit
            >>= \case
                -- this always happens, since when
                -- \* the account is valid;
                -- \* the account is not a baker;
                -- \* the account is not a delegator;
                -- \* the account has sufficient balance to cover the stake,
                -- @(BCSuccess [], _)@ is returned, see `bsoConfigureBaker`.
                (BCSuccess _ _, a) -> modifyStakeTo (limit - 1) a
                t -> return t
    case res of
        BCStakeUnderThreshold -> return ()
        e -> error $ "Got (" ++ show e ++ ") but wanted BCStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake and decrease the stake above the limit. (MUST SUCCEED)
testing2'2 :: ThisMonadConcrete PV ()
testing2'2 = do
    (res, _) <-
        createGS
            >>= createAccountWith (limitDelta + 100)
            >>= addBakerWith (limit + 100)
            >>= \case
                -- this always happens, since when
                -- \* the account is valid;
                -- \* the account is not a baker;
                -- \* the account is not a delegator;
                -- \* the account has sufficient balance to cover the stake,
                -- @(BCSuccess [], _)@ is returned, see `bsoConfigureBaker`.
                (BCSuccess _ _, a) -> modifyStakeTo limit a
                _ -> error "result of modifyStakeTo should be BCSuccess"
    case res of
        BCSuccess [BakerConfigureStakeReduced newStake] _ -> liftIO (assertEqual "new stake" limit newStake)
        e -> error $ "Got (" ++ show e ++ ") but wanted BakerConfigureStakeReduced"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake and increase the stake. (MUST SUCCEED)
testing2'3 :: ThisMonadConcrete PV ()
testing2'3 = do
    (res, _) <-
        createGS
            >>= createAccountWith (limitDelta + 100)
            >>= addBakerWith limit
            >>= \case
                -- this always happens, since when
                -- \* the account is valid;
                -- \* the account is not a baker;
                -- \* the account is not a delegator;
                -- \* the account has sufficient balance to cover the stake,
                -- @(BCSuccess [], _)@ is returned, see `bsoConfigureBaker`.
                (BCSuccess _ _, a) -> modifyStakeTo (limit + 100) a
                _ -> error "result of modifyStakeTo should be BCSuccess"
    case res of
        BCSuccess [BakerConfigureStakeIncreased newAmount] _ -> liftIO (assertEqual "new stake" (limit + 100) newAmount)
        e -> error $ "Got (" ++ show e ++ ") but wanted BakerConfigureStakeIncreased"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake, increase the limit and decrease the stake any amount (MUST
-- FAIL)
testing3'1 :: ThisMonadConcrete PV ()
testing3'1 = do
    (res, _) <-
        createGS
            >>= createAccountWith limitDelta
            >>= addBakerWith limit
            >>= ( \case
                    -- this always happens, since when
                    -- \* the account is valid;
                    -- \* the account is not a baker;
                    -- \* the account is not a delegator;
                    -- \* the account has sufficient balance to cover the stake,
                    -- @(BCSuccess [], _)@ is returned, see `bsoConfigureBaker`.
                    (BCSuccess _ _, a) -> increaseLimit (limit * 2) a
                    (_, bsAccIdx) -> return bsAccIdx
                )
            >>= modifyStakeTo (limit - 1)
    case res of
        BCStakeUnderThreshold -> return ()
        e -> error $ "Got (" ++ show e ++ ") but wanted BCStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake, increase the limit and increase the stake below limit
-- (MUST FAIL)
-- Note, this is a departure from the behaviour prior to P4, where this would succeed.
testing3'2 :: ThisMonadConcrete PV ()
testing3'2 = do
    (res, _) <-
        createGS
            >>= createAccountWith limitDelta
            >>= addBakerWith limit
            >>= ( \case
                    -- this always happens, since when
                    -- \* the account is valid;
                    -- \* the account is not a baker;
                    -- \* the account is not a delegator;
                    -- \* the account has sufficient balance to cover the stake,
                    -- @(BCSuccess [], _)@ is returned, see `bsoConfigureBaker`.
                    (BCSuccess _ _, a) -> increaseLimit (limit * 2) a
                    (_, bsAccIdx) -> return bsAccIdx
                )
            >>= modifyStakeTo (limit + 1)
    case res of
        BCStakeUnderThreshold -> return ()
        e -> error $ "Got (" ++ show e ++ ") but wanted BCStakeUnderThreshold"

-- starting from an empty blockstate with the dummy parameters, register a baker
-- with enough stake, increase the limit and increase the stake over limit (MUST
-- SUCCEED)
testing3'3 :: ThisMonadConcrete PV ()
testing3'3 = do
    (res, _) <-
        createGS
            >>= createAccountWith limitDelta
            >>= addBakerWith limit
            >>= ( \case
                    -- this always happens, since when
                    -- \* the account is valid;
                    -- \* the account is not a baker;
                    -- \* the account is not a delegator;
                    -- \* the account has sufficient balance to cover the stake,
                    -- @(BCSuccess [], _)@ is returned, see `bsoConfigureBaker`.
                    (BCSuccess _ _, a) -> increaseLimit (limit * 2) a
                    _ -> error "result of increaseLimit should be BCSuccess"
                )
            >>= modifyStakeTo (limit * 2 + 1)
    case res of
        BCSuccess [BakerConfigureStakeIncreased newStake] _ -> liftIO (assertEqual "new stake" (limit * 2 + 1) newStake)
        e -> error $ "Got (" ++ show e ++ ") but wanted BakerConfigureStakeIncreased"

tests :: Spec
tests = do
    let wtdgs :: String -> ThisMonadConcrete PV () -> Spec
        wtdgs s t =
            specify s $
                runBlobStoreTemp "." $
                    PBS.withNewAccountCache 1_000 $
                        PBS.runPersistentBlockStateMonad t
    describe "GlobalState.Updates - BakerStakeThreshold" $ do
        wtdgs "not enough stake - must fail" testing1
        wtdgs "enough stake >decrease> not enough - must fail" testing2'1
        wtdgs "enough stake >decrease> enough - must succeed" testing2'2
        wtdgs "enough stake >increase> enough - must succeed" testing2'3
        wtdgs "enough stake >> increase limit >decrease> not enough - must fail" testing3'1
        wtdgs "enough stake >> increase limit >increase> not enough - must succeed" testing3'2
        wtdgs "enough stake >> increase limit >increase> enough - must succeed" testing3'3
