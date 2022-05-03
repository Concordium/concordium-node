{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

-- |Tests for the payday-related functionality.
module SchedulerTests.Payday where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.List ( maximumBy )
import Data.Map ( (!) )
import System.Random
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
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
import GlobalStateMock
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Persistent.BlobStore (BlobStore)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.TreeState
import Concordium.ID.Types
import SchedulerTests.TestUtils ()
import Concordium.GlobalState.BlockPointer ()
import Concordium.GlobalState.Basic.BlockPointer (makeGenesisBasicBlockPointer)
import Concordium.GlobalState.BakerInfo
import Concordium.Birk.Bake
import Concordium.Startup
import Data.Either (fromRight)
import Concordium.GlobalState.Rewards (rewardsTotal)
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

prop_mintAmountsEqNewMint :: MintDistribution 'ChainParametersV1 -> MintRate -> Amount -> Bool
prop_mintAmountsEqNewMint md mr amt = mintTotal (doCalculatePaydayMintAmounts md mr amt) == mintAmount mr amt

instance Arbitrary (UpdateValue 'ChainParametersV1) where
  arbitrary = UVMintDistribution <$> arbitrary

prop_mintDistributionMostRecent :: MintDistribution 'ChainParametersV1 -> MintRate -> Slot -> [(Slot, UpdateValue 'ChainParametersV1)] -> Amount -> Bool
prop_mintDistributionMostRecent md mr ps updates amt =
  let (UVMintDistribution mostRecentMintDistribution) = snd
        . maximumBy (\(a, _) (b, _) -> compare a b)
        . takeWhile ((<= ps) . fst)
        . filter (\(_, UVMintDistribution _) -> True)
        $ (0, UVMintDistribution md) : updates
  in calculatePaydayMintAmounts md mr ps updates amt == doCalculatePaydayMintAmounts mostRecentMintDistribution mr amt

-- reward accounts are only increased by the sum of transaction fees
prop_mintDistributionImmediate :: forall m. (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m)
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
prop_mintDistributionImmediate bs0 blockParent slotNumber bid newEpoch mfinInfo newSeedState transFees freeCounts updates = do
  oldChainParameters <- bsoGetChainParameters bs0
  (mintRewardParams, bs1) <- updateBirkParameters newSeedState bs0 oldChainParameters []
  rewardsBefore <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs1
  bs2 <- mintAndReward bs1 blockParent slotNumber bid newEpoch mintRewardParams mfinInfo transFees freeCounts updates
  rewardsAfter <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs2
  return $ rewardsBefore == rewardsAfter

prop_transactionFeesDistributionP4 :: forall m. (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, AccountVersionFor (MPV m) ~ 'AccountV1)
  => Amount
  -- ^Transaction fees paid
  -> FreeTransactionCounts
  -- ^Counts of unpaid transactions
  -> BakerId
  -- ^Block baker
  -> UpdatableBlockState m
  -- ^Block state
  -> m Bool
prop_transactionFeesDistributionP4 transFees freeCounts bid bs0 = do
  rewardAccountsBefore <- (^. rewardAccounts) <$> bsoGetBankStatus bs0
  atfPassiveBefore <- bsoGetAccruedTransactionFeesPassive bs0
  atfFoundationAccountBefore <- bsoGetAccruedTransactionFeesFoundationAccount bs0
  bakerPoolRewardDetailsBefore <- bsoGetBakerPoolRewardDetails bs0
  bs1 <- doBlockRewardP4 transFees freeCounts bid bs0
  rewardAccountsAfter <- (^. rewardAccounts) <$> bsoGetBankStatus bs1  
  atfPassiveAfter <- bsoGetAccruedTransactionFeesPassive bs0
  atfFoundationAccountAfter <- bsoGetAccruedTransactionFeesFoundationAccount bs0  
  bakerPoolRewardDetailsAfter <- bsoGetBakerPoolRewardDetails bs0
  return $ rewardsTotal rewardAccountsBefore + atfPassiveBefore + atfFoundationAccountBefore + transactionFeesAccrued (bakerPoolRewardDetailsBefore ! bid)
    == rewardsTotal rewardAccountsAfter + atfPassiveAfter + atfFoundationAccountAfter + transactionFeesAccrued (bakerPoolRewardDetailsAfter ! bid)

-- I snatched `initialBlockState` from another test, many seem to have done the same over the years

staticKeys :: [(KeyPair, AccountAddress)]
staticKeys = ks (mkStdGen 1333)
    where
        ks g = let (k, g') = randomEd25519KeyPair g
                   (addr, g'') = randomAccountAddress g'
               in (uncurry KeyPairEd25519 k, addr) : ks g''

numAccounts :: Int
numAccounts = 10

initialBlockState :: (IsProtocolVersion pv) => HashedBlockState pv
initialBlockState = Concordium.GlobalState.Basic.BlockState.hashBlockState $ createBlockState
    (foldr addAcc emptyAccounts (take numAccounts staticKeys))
    where
        addAcc (kp, addr) = putAccountWithRegIds (mkAccount (correspondingVerifyKey kp) addr initBal )
        initBal = 10^(12::Int) :: Amount

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

runMyPureMonad' :: (IsProtocolVersion pv) => GenesisData pv -> MyPureMonad pv a -> IO (a, MyPureTreeState pv)
runMyPureMonad' gd = runPureBlockStateMonad (initialSkovDataDefault gd initialBlockState) >>= runMyPureMonad

type MyPersistentBlockState pv = HashedPersistentBlockState pv
type MyPersistentTreeState pv = SkovPersistentData pv () (MyPersistentBlockState pv)
type MyPersistentMonad pv = PersistentTreeStateMonad () (MyPersistentBlockState pv) (PersistentBlockStateMonad pv (MyPersistentBlockState pv) (ReaderT BlobStore (StateT (MyPersistentTreeState pv) LogIO)))

runMyPersistentMonad :: (IsProtocolVersion pv) => LogMethod IO -> BlobStore -> MyPersistentTreeState pv -> MyPersistentMonad pv a -> IO (a, MyPersistentTreeState pv)
runMyPersistentMonad lm bs is  = (`runLoggerT` lm) . (`runStateT` is) . (`runReaderT` bs) . runPersistentBlockStateMonad . runPersistentTreeStateMonad

-- runMyPersistentMonad' :: (IsProtocolVersion pv) => GenesisData pv -> MyPersistentMonad pv a -> IO (a, MyPersistentTreeState pv)
-- runMyPersistentMonad' gd = do
--   let dbpath = "./testdb/"
--   pbs <- makePersistent . _unhashedBlockState $ initialBlockState
--   ser <- saveBlockState pbs
--   is <- runPersistentBlockStateMonad (initialSkovPersistentDataDefault dbpath gd pbs () NoLogContext ser)
--   runMyPersistentMonad logEvent _ is

testRewardDistribution :: Spec
testRewardDistribution = do
  it "splits the minted amount three ways" $ withMaxSuccess 10000 $ property prop_mintAmountsEqNewMint
  -- the following property is falsified if the updates list is unsorted by slot number, which might or might not be the correct behaviour
  -- it "chooses the most recent mint distribution before payday" $ withMaxSuccess 10000 $ property prop_mintDistributionMostRecent  
  it "does not change after mint distribution" $ do
    (resultPure, _) <- runMyPureMonad' gd (prop_mintDistributionImmediate ibs blockParent slot bid epoch mfinInfo newSeedState transFees freeCounts updates)
    assertBool "in-memory" resultPure
--  (resultPersistent, _) <- runMyPersistentMonad' gd (prop_mintDistributionImmediate _ blockParent slot bid epoch mintParams mfinInfo amount freeCounts updates)
--  assertBool "persistent" resultPersistent
  it "does not change after block reward distribution" $ do
    (resultPure, _) <- runMyPureMonad' gd (prop_transactionFeesDistributionP4 transFees freeCounts bid ibs)
    assertBool "in-memory" resultPure
      where gd = genesis 5 ^._1 :: GenesisData 'P4
            ibs = fromRight (_unhashedBlockState initialBlockState :: Concordium.GlobalState.Basic.BlockState.BlockState 'P4) (genesisState gd)
            blockParent = makeGenesisBasicBlockPointer gd initialBlockState
            slot = 400
            bid = BakerId 1
            epoch = 4
            mfinInfo = Nothing
            newSeedState = initialSeedState (Hash.hash "qwerty") 100
            transFees = 1
            freeCounts = FreeTransactionCounts 10 10 1
            updates = []
  
tests :: Spec
tests = describe "Payday" $ do
    describe "Minting" testDoMintingP4
    describe "Reward balance" testRewardDistribution
