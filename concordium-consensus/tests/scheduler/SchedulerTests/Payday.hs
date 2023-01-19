{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |Tests for the payday-related functionality.
module SchedulerTests.Payday (tests) where

import Control.Exception (bracket)
import Control.Monad.Trans.State

import Control.Monad (join)
import Data.Foldable (toList)
import Data.List (maximumBy, sortBy)
import Data.Map ((!))
import Data.Proxy
import Data.Ratio
import qualified Data.Vector as Vec
import System.FilePath.Posix
import System.IO.Temp
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState
import Concordium.GlobalState.DummyData
import Concordium.Logger
import Concordium.Scheduler.TreeStateEnvironment
import Concordium.Scheduler.Types
import Concordium.Types.DummyData
import Concordium.Types.SeedState

import Concordium.Birk.Bake
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.PoolRewards (BakerPoolRewardDetails (transactionFeesAccrued))
import Concordium.GlobalState.BlockPointer (BlockPointer (_bpState))
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import qualified Concordium.GlobalState.DummyData as DummyData
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState
import Concordium.Startup
import qualified SchedulerTests.Helpers as Helpers

foundationAccount :: AccountAddress
foundationAccount = accountAddressFrom 0

testDoMintingP4 :: Spec
testDoMintingP4 = do
    it "no updates" $
        runTest [] [mintAmts0]
    it "last minute update" $
        runTest [(400, UVMintDistribution md1)] [mintAmts1]
    it "late update" $
        runTest [(401, UVMintDistribution md1)] [mintAmts0]
    it "two updates" $
        runTest [(0, UVMintDistribution md1), (100, UVMintDistribution md2)] [mintAmts2]
    it "two updates, one late" $
        runTest [(0, UVMintDistribution md1), (401, UVMintDistribution md2)] [mintAmts1]
    -- Note: the minting should not be affected by any updates to the time parameters
    it "mint rate update" $
        runTest [(0, UVTimeParameters (TimeParametersV1 100 (MintRate 2 0)))] [mintAmts0]
  where
    initialTotalSupply = 1_000_000_000
    epochLength = 100
    -- Initial block state, the important information here is the epoch length and the initial total
    -- supply.
    initialBlockState = do
        account <- Helpers.makeTestAccountFromSeed initialTotalSupply 0
        BS.initialPersistentState
            (initialSeedState (Hash.hash "NONCE") epochLength)
            DummyData.dummyCryptographicParameters
            [account]
            DummyData.dummyIdentityProviders
            DummyData.dummyArs
            DummyData.dummyKeyCollection
            DummyData.dummyChainParameters
    -- Run a test of doMintingP4. It is provided a list of updates (paired with effective slot time)
    -- and a list of expected special transaction outcomes to have been produced.
    runTest :: [(Slot, UpdateValue 'ChainParametersV1)] -> [SpecialTransactionOutcome] -> Assertion
    runTest updates expectedSpecialOutcomes = join $ Helpers.runTestBlockState $ do
        initialState :: PersistentBlockState 'P4 <- thawBlockState =<< initialBlockState
        newState <- doMintingP4 dummyChainParameters targetEpoch mintRate foundationAccount updates initialState
        specialOutcomes <- getSpecialOutcomes =<< hashBlockState newState
        return $ assertEqual "Incorrect special outcomes are produced" expectedSpecialOutcomes $ toList specialOutcomes
    mintRate = MintRate 1 0 -- 100% mint rate
    targetEpoch = 4
    mintAmts0 =
        Mint
            { stoMintBakingReward = 600_000_000,
              stoMintFinalizationReward = 300_000_000,
              stoMintPlatformDevelopmentCharge = 100_000_000,
              stoFoundationAccount = foundationAccount
            }
    mintAmts1 =
        Mint
            { stoMintBakingReward = 1_000_000_000,
              stoMintFinalizationReward = 0,
              stoMintPlatformDevelopmentCharge = 0,
              stoFoundationAccount = foundationAccount
            }
    mintAmts2 =
        Mint
            { stoMintBakingReward = 0,
              stoMintFinalizationReward = 1_000_000_000,
              stoMintPlatformDevelopmentCharge = 0,
              stoFoundationAccount = foundationAccount
            }
    md1 = MintDistribution MintPerSlotForCPV0None (makeAmountFraction 100_000) (makeAmountFraction 0)
    md2 = MintDistribution MintPerSlotForCPV0None (makeAmountFraction 0) (makeAmountFraction 100_000)

-- rewards distributed after minting are equal to the minted amount
propMintAmountsEqNewMint :: MintDistribution 'ChainParametersV1 -> MintRate -> Amount -> Bool
propMintAmountsEqNewMint md mr amt = mintTotal (doCalculatePaydayMintAmounts md mr amt) == mintAmount mr amt

instance Arbitrary (UpdateValue 'ChainParametersV1) where
    arbitrary = UVMintDistribution <$> arbitrary

-- the most recent update of mint distribution parameters is used to determine mint distribution
propMintDistributionMostRecent :: MintDistribution 'ChainParametersV1 -> MintRate -> Slot -> [(Slot, UpdateValue 'ChainParametersV1)] -> Amount -> Bool
propMintDistributionMostRecent md mr ps updates amt =
    let updatesSorted = sortBy (\(a, _) (b, _) -> compare a b) $ map (& (_1 +~ 1)) updates
        chainParams =
            snd
                . maximumBy (\(a, _) (b, _) -> compare a b)
                . takeWhile ((<= ps) . fst)
                . filter
                    ( \case
                        (_, UVMintDistribution _) -> True
                        _ -> error "pair should match (a, UVMintDistribution)"
                    )
                $ (0, UVMintDistribution md) : updatesSorted
        mostRecentMintDistribution = case chainParams of
            (UVMintDistribution mintDist) -> mintDist
            -- this does not happen due to how `chainParams` is constructed
            _ -> error "chainParams should be UVMintDistribution"
    in  calculatePaydayMintAmounts md mr ps updatesSorted amt == doCalculatePaydayMintAmounts mostRecentMintDistribution mr amt

-- `mintAndReward` doesn't change the baking and finalization reward accounts balance
propMintDistributionImmediate ::
    forall m.
    (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m) =>
    -- |Block state
    UpdatableBlockState m ->
    -- |Parent block
    BlockPointerType m ->
    -- |Block slot
    Slot ->
    -- |Baker ID
    BakerId ->
    -- |Epoch of the new block
    Epoch ->
    -- |Info on finalization committee for included record, if any
    Maybe FinalizerInfo ->
    -- |New seed state
    SeedState ->
    -- |Transaction fees
    Amount ->
    -- |Number of "free" transactions of each type
    FreeTransactionCounts ->
    -- |Ordered chain updates since the last block
    [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))] ->
    m Bool
propMintDistributionImmediate bs0 blockParent slotNumber bid newEpoch mfinInfo newSeedState transFees freeCounts updates = do
    oldChainParameters <- bsoGetChainParameters bs0
    (mintRewardParams, bs1) <- updateBirkParameters newSeedState bs0 oldChainParameters []
    rewardsBefore <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs1
    bs2 <- mintAndReward bs1 blockParent slotNumber bid newEpoch mintRewardParams mfinInfo transFees freeCounts updates
    rewardsAfter <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs2
    return $
        rewardsBefore ^. bakingRewardAccount + rewardsBefore ^. finalizationRewardAccount
            == rewardsAfter ^. bakingRewardAccount + rewardsAfter ^. finalizationRewardAccount

-- distributed block rewards are equal to the amount withdrawn from reward accounts + transaction fees
propTransactionFeesDistributionP4 ::
    forall m.
    (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, AccountVersionFor (MPV m) ~ 'AccountV1) =>
    -- |Transaction fees paid
    Amount ->
    -- |Counts of unpaid transactions
    FreeTransactionCounts ->
    -- |Block baker
    BakerId ->
    -- |Block state
    UpdatableBlockState m ->
    m Bool
propTransactionFeesDistributionP4 transFees freeCounts bid bs0 = do
    rewardAccountsBefore <- (^. rewardAccounts) <$> bsoGetBankStatus bs0
    atfPassiveBefore <- bsoGetAccruedTransactionFeesPassive bs0
    atfFoundationAccountBefore <- bsoGetAccruedTransactionFeesFoundationAccount bs0
    bakerPoolRewardDetailsBefore <- bsoGetBakerPoolRewardDetails bs0
    bs1 <- doBlockRewardP4 transFees freeCounts bid bs0
    rewardAccountsAfter <- (^. rewardAccounts) <$> bsoGetBankStatus bs1
    atfPassiveAfter <- bsoGetAccruedTransactionFeesPassive bs1
    atfFoundationAccountAfter <- bsoGetAccruedTransactionFeesFoundationAccount bs1
    bakerPoolRewardDetailsAfter <- bsoGetBakerPoolRewardDetails bs1
    return $
        rewardsTotal rewardAccountsBefore + atfPassiveBefore + atfFoundationAccountBefore + transactionFeesAccrued (bakerPoolRewardDetailsBefore ! bid) + transFees
            == rewardsTotal rewardAccountsAfter + atfPassiveAfter + atfFoundationAccountAfter + transactionFeesAccrued (bakerPoolRewardDetailsAfter ! bid)

initialPersistentBlockState ::
    IsProtocolVersion pv => Helpers.PersistentBSM pv (HashedPersistentBlockState pv)
initialPersistentBlockState =
    Helpers.createTestBlockStateWithAccountsM $ fmap (Helpers.makeTestAccountFromSeed initBal) [0 .. 10]
  where
    initBal = 10 ^ (12 :: Int) :: Amount

genesis :: (IsProtocolVersion pv) => Word -> (GenesisData pv, [(BakerIdentity, FullBakerInfo)], Amount)
genesis nBakers =
    makeGenesisData
        0
        nBakers
        1_000
        defaultFinalizationParameters
        dummyCryptographicParameters
        emptyIdentityProviders
        dummyArs
        []
        1_234
        dummyKeyCollection
        dummyChainParameters

type MyPersistentTreeState pv = SkovPersistentData pv (HashedPersistentBlockState pv)
type MyPersistentMonad pv =
    PersistentTreeStateMonad
        (HashedPersistentBlockState pv)
        ( MGSTrans
            (StateT (MyPersistentTreeState pv))
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (BlobStoreT (PersistentBlockStateContext pv) LogIO)
            )
        )

fakeLogMethod :: LogMethod IO
fakeLogMethod _ _ _ = return ()

withPersistentState :: (IsProtocolVersion pv) => PersistentBlockStateContext pv -> MyPersistentTreeState pv -> (HashedPersistentBlockState pv -> MyPersistentMonad pv a) -> IO (a, MyPersistentTreeState pv)
withPersistentState pbsc is f =
    (`runLoggerT` fakeLogMethod)
        . (`runBlobStoreT` pbsc)
        . runPersistentBlockStateMonad
        . (`runStateT` is)
        . (\(MGSTrans z) -> z)
        . runPersistentTreeStateMonad
        $ f (_bpState . Concordium.GlobalState.Persistent.TreeState._focusBlock $ is)

-- `createGlobalState` and `destroyGlobalState` are adapted from GlobalStateTests.PersistentTreeState

createGlobalState :: (IsProtocolVersion pv) => FilePath -> IO (PersistentBlockStateContext pv, MyPersistentTreeState pv)
createGlobalState dbDir = do
    let
        n = 5
        config = DTDBConfig defaultRuntimeParameters dbDir (dbDir </> "blockstate" <.> "dat")
    (x, y) <- runSilentLogger $ initialiseGlobalState (genesis n ^. _1) config
    return (x, y)

destroyGlobalState :: (IsProtocolVersion pv) => (PersistentBlockStateContext pv, MyPersistentTreeState pv) -> IO ()
destroyGlobalState (c, s) = shutdownGlobalState protocolVersion (Proxy :: Proxy DiskTreeDiskBlockConfig) c s

withPersistentState' :: (IsProtocolVersion pv) => (HashedPersistentBlockState pv -> MyPersistentMonad pv a) -> IO (a, MyPersistentTreeState pv)
withPersistentState' f = withTempDirectory "." "test-directory" $
    \dbDir -> bracket (createGlobalState dbDir) destroyGlobalState $
        \(pbsc, mySkovPersistentData) ->
            withPersistentState pbsc mySkovPersistentData f

testRewardDistribution :: Spec
testRewardDistribution = do
    it "splits the minted amount three ways" $
        withMaxSuccess 10_000 $
            property propMintAmountsEqNewMint
    it "chooses the most recent mint distribution before payday" $
        withMaxSuccess 10_000 $
            property propMintDistributionMostRecent
    it "does not change after mint distribution (persistent)" $ do
        ipbs :: HashedPersistentBlockState 'P4 <- Helpers.runTestBlockState initialPersistentBlockState
        blockParentPersistent :: PersistentBlockPointer 'P4 (HashedPersistentBlockState 'P4) <-
            makeGenesisPersistentBlockPointer (genesisConfiguration gd) ipbs
        (resultPersistent, _) <-
            withPersistentState'
                ( \x ->
                    propMintDistributionImmediate
                        (hpbsPointers x)
                        blockParentPersistent
                        slot
                        bid
                        epoch
                        mfinInfo
                        newSeedState
                        transFees
                        freeCounts
                        updates
                )
        assertBool "persistent" resultPersistent
    it "does not change after block reward distribution (persistent)" $ do
        (resultPersistent, _ :: MyPersistentTreeState 'P4) <-
            withPersistentState'
                (propTransactionFeesDistributionP4 transFees freeCounts bid . hpbsPointers)
        assertBool "persistent" resultPersistent
  where
    gd = genesis 5 ^. _1 :: GenesisData 'P4
    slot = 400
    bid = BakerId 1
    epoch = 4
    mfinInfo = Nothing
    newSeedState = initialSeedState (Hash.hash "qwerty") 100
    transFees = 123
    freeCounts = FreeTransactionCounts 10 10 1
    updates = []

-- |Test that 'scaleAmount' performs correctly by multiplying by the denominator and adding the remainder.
testScaleAmount1 :: Property
testScaleAmount1 = property $ \(a :: Amount) b c ->
    let num = min a b
        den = max 1 (max a b)
        cTimesNum = toInteger c * toInteger num
    in  toInteger (scaleAmount num den c) * toInteger den + cTimesNum `mod` toInteger den === cTimesNum

-- |Test that 'scaleAmount' performs correctly
testScaleAmount2 :: Property
testScaleAmount2 = property $ \(a :: Amount) b c ->
    let num = min a b
        den = max 1 (max a b)
    in  scaleAmount num den c === floor ((toInteger num % toInteger den) * toRational c)

-- |A test case for 'rewardDelegators'.
data DelegatorRewardTestCase = DelegatorRewardTestCase
    { -- |Name to identify the case
      drtcName :: String,
      -- |Baking reward distributed to delegators
      drtcBakingReward :: Amount,
      -- |Finalization reward distributed to delegators
      drtcFinalizationReward :: Amount,
      -- |Transaction fee reward distributed to delegators
      drtcTransactionFeeReward :: Amount,
      -- |Capital held by the pool owner
      drtcBakerCapital :: Amount,
      -- |Capital held by each delegator
      drtcDelegatorCapitals :: [Amount],
      -- |Expected baking, finalization and transaction fee rewards to each delegator.
      drtcDelegatorExpectedRewards :: [(Amount, Amount, Amount)]
    }

-- |Test cases for 'rewardDelegators'.
drtcs :: [DelegatorRewardTestCase]
drtcs =
    [ DelegatorRewardTestCase
        { -- This tests for potential overflow in distributing the baking reward
          drtcName = "big baker reward, 1 big delegator",
          drtcBakingReward = 1_000_000_000_000_000,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards = [(999_999_999_999_999, 0, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for precision in distributing the baking reward
          drtcName = "big baker reward, 1 big delegator (2)",
          drtcBakingReward = 1_000_000_000_000_000_000,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [999_999_999_999_999_999],
          drtcDelegatorExpectedRewards = [(999_999_999_999_999_999, 0, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for precision in distributing the baking reward
          drtcName = "big baker reward, 1 big delegator (3)",
          drtcBakingReward = 1_000_000_000_000_000_000,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [999_999_999_999_999_998],
          drtcDelegatorExpectedRewards = [(999_999_999_999_999_998, 0, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for potential overflow in distributing the finalization reward
          drtcName = "big finalization reward, 1 big delegator",
          drtcBakingReward = 0,
          drtcFinalizationReward = 1_000_000_000_000_000,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards = [(0, 999_999_999_999_999, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for potential overflow in distributing the transaction fee reward
          drtcName = "big transaction fee reward, 1 big delegator",
          drtcBakingReward = 0,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 1_000_000_000_000_000,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards = [(0, 0, 999_999_999_999_999)]
        },
      DelegatorRewardTestCase
        { drtcName = "big rewards, 1 big delegator",
          drtcBakingReward = 1_000_000_000_000_000,
          drtcFinalizationReward = 1_000_000_000_000_000,
          drtcTransactionFeeReward = 1_000_000_000_000_000,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards =
            [ (999_999_999_999_999, 999_999_999_999_999, 999_999_999_999_999)
            ]
        },
      DelegatorRewardTestCase
        { drtcName = "big rewards, 2 big delegators",
          drtcBakingReward = 2_000_000_000_000_000,
          drtcFinalizationReward = 2_000_000_000_000_000,
          drtcTransactionFeeReward = 2_000_000_000_000_000,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000, 1_000_000_000_000_000],
          drtcDelegatorExpectedRewards =
            [ (999_999_999_999_999, 999_999_999_999_999, 999_999_999_999_999),
              (999_999_999_999_999, 999_999_999_999_999, 999_999_999_999_999)
            ]
        }
    ]

-- | Run a DelegatorRewardTestCase
runTestCase :: DelegatorRewardTestCase -> Spec
runTestCase DelegatorRewardTestCase{..} = it drtcName $ do
    (DelegatorRewardOutcomes{..}, _) <- Helpers.runTestBlockState $ do
        initialState :: PersistentBlockState 'P4 <- thawBlockState =<< initialPersistentBlockState
        rewardDelegators initialState drtcFinalizationReward drtcBakingReward drtcTransactionFeeReward totCap dels
    assertEqual "Total baking rewards" (sum $ drtcDelegatorExpectedRewards ^.. each . _1) _delegatorAccumBaking
    assertEqual "Total finalization rewards" (sum $ drtcDelegatorExpectedRewards ^.. each . _2) _delegatorAccumFinalization
    assertEqual "Total transaction fee rewards" (sum $ drtcDelegatorExpectedRewards ^.. each . _3) _delegatorAccumTransaction
    let makeSTOs delid (bak, fin, tran) =
            PaydayAccountReward
                { stoAccount = accountAddressFrom delid,
                  stoBakerReward = bak,
                  stoFinalizationReward = fin,
                  stoTransactionFees = tran
                }
    assertEqual "Special transaction outcomes" (zipWith makeSTOs [0 ..] drtcDelegatorExpectedRewards) (_delegatorOutcomes ^.. each)
  where
    totCap = drtcBakerCapital + sum (dcDelegatorCapital <$> dels)
    dels = Vec.fromList (zipWith DelegatorCapital [0 ..] drtcDelegatorCapitals)

tests :: Spec
tests = describe "Payday" $ do
    describe "Minting" testDoMintingP4
    describe "Reward balance" testRewardDistribution
    describe "scaleAmount" $ do
        it "div-mod" testScaleAmount1
        it "via Rational" testScaleAmount2
    describe "rewardDelegators" $ mapM_ runTestCase drtcs
