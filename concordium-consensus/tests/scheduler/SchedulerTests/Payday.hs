{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |Tests for the payday-related functionality.
module SchedulerTests.Payday where

import Control.Exception ( bracket )
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import qualified Data.Vector as Vec
import Data.Ratio
import Data.Either ( fromRight )
import Data.List ( maximumBy, sortBy )
import Data.Map ( (!) )
import Data.Proxy
import System.Random
import System.FilePath.Posix
import System.IO.Temp
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
import Concordium.GlobalState
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.TreeStateEnvironment
import Concordium.Scheduler.Types
import Concordium.Types.DummyData
import Concordium.Types.SeedState
import Concordium.Crypto.SignatureScheme
import Concordium.Crypto.DummyData

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.CapitalDistribution
import GlobalStateMock
import Concordium.GlobalState.Basic.BlockPointer (makeGenesisBasicBlockPointer)
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TransactionTable (TransactionTable, emptyTransactionTable)
import Concordium.ID.Types
import SchedulerTests.TestUtils ()
import Concordium.GlobalState.BlockPointer (BlockPointer (_bpState))
import Concordium.GlobalState.BakerInfo
import Concordium.Birk.Bake
import Concordium.Startup
import Concordium.GlobalState.Basic.BlockState.PoolRewards (BakerPoolRewardDetails(transactionFeesAccrued))

foundationAccount :: AccountAddress
foundationAccount = accountAddressFrom 0

testDoMintingP4 :: Spec
testDoMintingP4 = do
    it "no updates" $ assertEqual "" (runMock (events mintAmts0) (op [])) result
    it "last minute update" $ assertEqual "" (runMock (events mintAmts1) (op [(400, UVMintDistribution md1)])) result
    it "late update" $ assertEqual "" (runMock (events mintAmts0) (op [(401, UVMintDistribution md1)])) result
    it "two updates" $ assertEqual "" (runMock (events mintAmts2) (op [(0, UVMintDistribution md1), (100, UVMintDistribution md2)])) result
    it "two updates, one late" $ assertEqual "" (runMock (events mintAmts1) (op [(0, UVMintDistribution md1), (401, UVMintDistribution md2)])) result
    -- Note: the minting should not be affected by any updates to the time parameters
    it "mint rate update" $ assertEqual "" (runMock (events mintAmts0) (op [(0, UVTimeParameters (TimeParametersV1 100 (MintRate 2 0)))])) result
  where
    events :: MintAmounts -> [WithResult (Action 'P4)]
    events amts =
        [ BSO (BsoGetBankStatus (bs 0)) :-> bank,
          BSO (BsoGetSeedState (bs 0)) :-> seedState,
          BSO (BsoMint (bs 0) amts) :-> bs 1,
          BSO (BsoAddSpecialTransactionOutcome (bs 1) (mintSto amts)) :-> bs 2
        ]
    op upds = doMintingP4 dummyChainParameters targetEpoch mintRate foundationAccount upds (bs 0)
    result = bs 2
    bs = MockUpdatableBlockState
    mintRate = MintRate 1 0 -- 100% mint rate
    bank = emptyBankStatus{_totalGTU = 1000000000}
    targetEpoch = 4
    epochLength = 100
    seedState = initialSeedState (Hash.hash "NONCE") epochLength
    mintAmts0 =
        MintAmounts
            { mintBakingReward = 600000000,
              mintFinalizationReward = 300000000,
              mintDevelopmentCharge = 100000000
            }
    mintAmts1 =
        MintAmounts
            { mintBakingReward = 1000000000,
              mintFinalizationReward = 0,
              mintDevelopmentCharge = 0
            }
    mintAmts2 =
        MintAmounts
            { mintBakingReward = 0,
              mintFinalizationReward = 1000000000,
              mintDevelopmentCharge = 0
            }
    md1 = MintDistribution MintPerSlotForCPV0None (makeAmountFraction 100000) (makeAmountFraction 0)
    md2 = MintDistribution MintPerSlotForCPV0None (makeAmountFraction 0) (makeAmountFraction 100000)
    mintSto amts =
        Mint
            { stoMintBakingReward = mintBakingReward amts,
              stoMintFinalizationReward = mintFinalizationReward amts,
              stoMintPlatformDevelopmentCharge = mintDevelopmentCharge amts,
              stoFoundationAccount = foundationAccount
            }

-- rewards distributed after minting are equal to the minted amount
propMintAmountsEqNewMint :: MintDistribution 'ChainParametersV1 -> MintRate -> Amount -> Bool
propMintAmountsEqNewMint md mr amt = mintTotal (doCalculatePaydayMintAmounts md mr amt) == mintAmount mr amt

instance Arbitrary (UpdateValue 'ChainParametersV1) where
  arbitrary = UVMintDistribution <$> arbitrary

-- the most recent update of mint distribution parameters is used to determine mint distribution
propMintDistributionMostRecent :: MintDistribution 'ChainParametersV1 -> MintRate -> Slot -> [(Slot, UpdateValue 'ChainParametersV1)] -> Amount -> Bool
propMintDistributionMostRecent md mr ps updates amt =
  let updatesSorted = sortBy (\(a, _) (b, _) -> compare a b) $ map (& (_1 +~ 1)) updates
      (UVMintDistribution mostRecentMintDistribution) = snd
        . maximumBy (\(a, _) (b, _) -> compare a b)
        . takeWhile ((<= ps) . fst)
        . filter (\(_, UVMintDistribution _) -> True)
        $ (0, UVMintDistribution md) : updatesSorted
  in calculatePaydayMintAmounts md mr ps updatesSorted amt == doCalculatePaydayMintAmounts mostRecentMintDistribution mr amt

-- `mintAndReward` doesn't change the baking and finalization reward accounts balance
propMintDistributionImmediate :: forall m. (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m)
    => UpdatableBlockState m
    -- ^Block state
    -> BlockPointerType m
    -- ^Parent block
    -> Slot
    -- ^Block slot
    -> BakerId
    -- ^Baker ID
    -> Epoch
    -- ^Epoch of the new block
    -> Maybe FinalizerInfo
    -- ^Info on finalization committee for included record, if any
    -> SeedState
    -- ^New seed state
    -> Amount
    -- ^Transaction fees
    -> FreeTransactionCounts
    -- ^Number of "free" transactions of each type
    -> [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))]
    -- ^Ordered chain updates since the last block
    -> m Bool
propMintDistributionImmediate bs0 blockParent slotNumber bid newEpoch mfinInfo newSeedState transFees freeCounts updates = do
  oldChainParameters <- bsoGetChainParameters bs0
  (mintRewardParams, bs1) <- updateBirkParameters newSeedState bs0 oldChainParameters []
  rewardsBefore <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs1
  bs2 <- mintAndReward bs1 blockParent slotNumber bid newEpoch mintRewardParams mfinInfo transFees freeCounts updates
  rewardsAfter <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs2
  return $ rewardsBefore ^. bakingRewardAccount + rewardsBefore ^. finalizationRewardAccount
    == rewardsAfter ^. bakingRewardAccount + rewardsAfter ^. finalizationRewardAccount

-- distributed block rewards are equal to the amount withdrawn from reward accounts + transaction fees
propTransactionFeesDistributionP4 :: forall m. (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, AccountVersionFor (MPV m) ~ 'AccountV1)
  => Amount
  -- ^Transaction fees paid
  -> FreeTransactionCounts
  -- ^Counts of unpaid transactions
  -> BakerId
  -- ^Block baker
  -> UpdatableBlockState m
  -- ^Block state
  -> m Bool
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
  return $ rewardsTotal rewardAccountsBefore + atfPassiveBefore + atfFoundationAccountBefore + transactionFeesAccrued (bakerPoolRewardDetailsBefore ! bid) + transFees
    == rewardsTotal rewardAccountsAfter + atfPassiveAfter + atfFoundationAccountAfter + transactionFeesAccrued (bakerPoolRewardDetailsAfter ! bid)

-- Creates some test accounts with random keys to be used with the initial block state.
-- Adapted from SchedulerTests.RandomBakerTransactions.

initialAccounts :: (IsProtocolVersion pv) => Accounts pv
initialAccounts = foldr addAcc emptyAccounts (take numAccounts staticKeys)
    where
        addAcc (kp, addr) = putAccountWithRegIds (mkAccount (correspondingVerifyKey kp) addr initBal)
        staticKeys = ks (mkStdGen 1333)
          where
            ks g = let (k, g') = randomEd25519KeyPair g
                       (addr, g'') = randomAccountAddress g'
                   in (uncurry KeyPairEd25519 k, addr) : ks g''
        initBal = 10^(12::Int) :: Amount
        numAccounts = 10 :: Int

-- I snatched `initial(Pure|Persistent)BlockState` and `genesis` from other tests, many seem to have done the same over the years. 

initialPureBlockState :: (IsProtocolVersion pv) => HashedBlockState pv
initialPureBlockState = Concordium.GlobalState.Basic.BlockState.hashBlockState $ createBlockState initialAccounts

initialPersistentBlockState :: (IsProtocolVersion pv, MonadBlobStore m) => m (HashedPersistentBlockState pv)
initialPersistentBlockState = makePersistent . _unhashedBlockState $ initialPureBlockState

genesis :: (IsProtocolVersion pv) => Word -> (GenesisData pv, [(BakerIdentity, FullBakerInfo)], Amount)
genesis nBakers =
    makeGenesisData
    0
    nBakers
    1000
    defaultFinalizationParameters
    dummyCryptographicParameters
    emptyIdentityProviders
    dummyArs
    []
    1234
    dummyKeyCollection
    dummyChainParameters

-- this is, perhaps, the smallest effect type that makes these tests run

type MyPureBlockState pv = HashedBlockState pv
type MyPureTreeState pv = SkovData pv (MyPureBlockState pv)
type MyPureMonad pv = PureTreeStateMonad (MyPureBlockState pv) (PureBlockStateMonad pv (StateT (MyPureTreeState pv) IO))

runMyPureMonad :: (IsProtocolVersion pv) => MyPureTreeState pv -> MyPureMonad pv a -> IO (a, MyPureTreeState pv)
runMyPureMonad is = (`runStateT` is) . runPureBlockStateMonad . runPureTreeStateMonad

runMyPureMonad' :: (IsProtocolVersion pv) => GenesisData pv -> TransactionTable -> MyPureMonad pv a -> IO (a, MyPureTreeState pv)
runMyPureMonad' gd genTT = runPureBlockStateMonad (initialSkovDataDefault gd initialPureBlockState genTT) >>= runMyPureMonad

type MyPersistentBlockState pv = HashedPersistentBlockState pv
type MyPersistentTreeState pv = SkovPersistentData pv (MyPersistentBlockState pv)
type MyPersistentMonad pv = PersistentTreeStateMonad (MyPersistentBlockState pv)
                              (MGSTrans (StateT (MyPersistentTreeState pv))
                                 (PersistentBlockStateMonad pv BlobStore (ReaderT BlobStore LogIO)))

fakeLogMethod :: LogMethod IO
fakeLogMethod _ _ _ = return ()

withPersistentState :: (IsProtocolVersion pv) => BlobStore -> MyPersistentTreeState pv -> (MyPersistentBlockState pv -> MyPersistentMonad pv a) -> IO (a, MyPersistentTreeState pv)
withPersistentState bSt is f = (`runLoggerT` fakeLogMethod) . (`runReaderT` bSt) . runPersistentBlockStateMonad
                             . (`runStateT` is) . (\(MGSTrans z) -> z)
                             . runPersistentTreeStateMonad $ f (_bpState . Concordium.GlobalState.Persistent.TreeState._focusBlock $ is)

-- `createGlobalState` and `destroyGlobalState` are adapted from GlobalStateTests.PersistentTreeState        

createGlobalState :: (IsProtocolVersion pv) => FilePath -> IO (PersistentBlockStateContext, MyPersistentTreeState pv)
createGlobalState dbDir = do
  let
    n = 5
    config = DTDBConfig defaultRuntimeParameters dbDir (dbDir </> "blockstate" <.> "dat")
  (x, y) <- runSilentLogger $ initialiseGlobalState (genesis n ^. _1) config
  return (x, y)

destroyGlobalState :: (IsProtocolVersion pv) => (PersistentBlockStateContext, MyPersistentTreeState pv) -> IO ()
destroyGlobalState (c, s) = shutdownGlobalState protocolVersion (Proxy :: Proxy DiskTreeDiskBlockConfig) c s

withPersistentState' :: (IsProtocolVersion pv) => (MyPersistentBlockState pv -> MyPersistentMonad pv a) -> IO (a, MyPersistentTreeState pv)
withPersistentState' f = withTempDirectory "." "test-directory"
    $ \dbDir -> bracket (createGlobalState dbDir) destroyGlobalState $
                \ (PersistentBlockStateContext myBlobStore, mySkovPersistentData) ->
                  withPersistentState myBlobStore mySkovPersistentData f

testRewardDistribution :: Spec
testRewardDistribution = do
  it "splits the minted amount three ways" $ withMaxSuccess 10000 $ property propMintAmountsEqNewMint
  it "chooses the most recent mint distribution before payday" $ withMaxSuccess 10000 $ property propMintDistributionMostRecent
  it "does not change after mint distribution (in-memory)" $ do
    (resultPure, _) <- runMyPureMonad' gd genTT (propMintDistributionImmediate ibs blockParentPure slot bid epoch mfinInfo newSeedState transFees freeCounts updates)
    assertBool "in-memory" resultPure
  it "does not change after mint distribution (persistent)" $ do
    ipbs :: MyPersistentBlockState 'P4 <- runBlobStoreTemp "." initialPersistentBlockState
    blockParentPersistent :: PersistentBlockPointer 'P4 (MyPersistentBlockState 'P4) <- makeGenesisPersistentBlockPointer gd ipbs
    (resultPersistent, _) <- withPersistentState' (\x -> propMintDistributionImmediate (hpbsPointers x) blockParentPersistent slot bid epoch mfinInfo newSeedState transFees freeCounts updates)
    assertBool "persistent" resultPersistent
  it "does not change after block reward distribution (in-memory)" $ do
    (resultPure, _) <- runMyPureMonad' gd genTT (propTransactionFeesDistributionP4 transFees freeCounts bid ibs)
    assertBool "in-memory" resultPure
  it "does not change after block reward distribution (persistent)" $ do
    (resultPersistent, _ :: MyPersistentTreeState 'P4) <- withPersistentState' (propTransactionFeesDistributionP4 transFees freeCounts bid . hpbsPointers)
    assertBool "persistent" resultPersistent
      where gd = genesis 5 ^._1 :: GenesisData 'P4
            (ibs, genTT) = case genesisState gd of
              Right x -> x
              Left _ -> (_unhashedBlockState initialPureBlockState :: Concordium.GlobalState.Basic.BlockState.BlockState 'P4, emptyTransactionTable)
            blockParentPure = makeGenesisBasicBlockPointer gd initialPureBlockState
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
     in toInteger (scaleAmount num den c) * toInteger den + cTimesNum `mod` toInteger den === cTimesNum

-- |Test that 'scaleAmount' performs correctly
testScaleAmount2 :: Property
testScaleAmount2 = property $ \(a :: Amount) b c ->
    let num = min a b
        den = max 1 (max a b)
     in scaleAmount num den c === floor ((toInteger num % toInteger den) * toRational c)

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

testRewardDelegators :: Spec
testRewardDelegators = describe "rewardDelegators" $ mapM_ p drtcs
  where
    p DelegatorRewardTestCase{..} = it drtcName $ do
        let (DelegatorRewardOutcomes{..}, _) =
                runMock events $
                    rewardDelegators (bs 0) drtcFinalizationReward drtcBakingReward drtcTransactionFeeReward totCap dels
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
        bs = MockUpdatableBlockState
        events :: [WithResult (Action 'P4)]
        events =
            zipWith
                ( \i amts ->
                    BSO (BsoRewardAccount (bs i) (fromIntegral i) (sum (amts ^.. each)))
                        :-> (Just (accountAddressFrom (fromIntegral i)), bs (i + 1))
                )
                [0 ..]
                drtcDelegatorExpectedRewards
        totCap = drtcBakerCapital + sum (dcDelegatorCapital <$> dels)
        dels = Vec.fromList (zipWith DelegatorCapital [0 ..] drtcDelegatorCapitals)


tests :: Spec
tests = describe "Payday" $ do
    describe "Minting" testDoMintingP4
    describe "Reward balance" testRewardDistribution
    describe "scaleAmount" $ do
        it "div-mod" testScaleAmount1
        it "via Rational" testScaleAmount2
    testRewardDelegators
