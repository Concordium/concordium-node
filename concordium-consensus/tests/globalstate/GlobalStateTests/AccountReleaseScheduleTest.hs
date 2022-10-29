{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.AccountReleaseScheduleTest where

import Concordium.GlobalState
import Concordium.Types.AnonymityRevokers
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseScheduleV1 as TARSV1
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import qualified Concordium.GlobalState.Basic.BlockState.ReleaseSchedule as BRS
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.DummyData
import Concordium.Types.IdentityProviders
import Concordium.GlobalState.Paired
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.Accounts
import qualified Concordium.GlobalState.Persistent.Account
import qualified Concordium.GlobalState.Persistent.Account.StructureV1
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.ReleaseSchedule as PRS
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict as RWS hiding (state)
import qualified  Control.Monad.Reader.Class as R
import Data.Aeson (eitherDecode)
import Data.Foldable
import Data.IORef
import Data.List (nub, sort)
import qualified Data.Map.Strict as OrdMap
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

--------------------------------- Monad Types ----------------------------------

-- |Protocol version.
type PV = 'P5

type PairedGSContext = PairGSContext () (PBS.PersistentBlockStateContext PV)

type PairedGState = PairGState
                      (SkovData PV (HashedBlockState PV))
                      (SkovPersistentData PV (PBS.HashedPersistentBlockState PV))

type ThisMonadConcrete = BlockStateM
                PV
                PairedGSContext
                (Identity PairedGSContext)
                PairedGState
                (Identity PairedGState)
                (RWST (Identity PairedGSContext) () (Identity PairedGState) LogIO)

--------------------------------- Test values ----------------------------------

genTimestamp :: Gen Timestamp
genTimestamp = Timestamp <$> arbitrary

dummyTransactionHash :: TransactionHash
dummyTransactionHash = case eitherDecode "\"f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868\"" of
  Left e -> error e
  Right v -> v

transactions :: Int -> IO [[(Timestamp, Amount)]]
transactions numOfSchedules = do
  let numReleasesInEach = map getPositive <$> vectorOf numOfSchedules arbitrary
      listsOfTimestamps = fmap (sort . nub . map (`mod` 200)) . flip vectorOf genTimestamp
      listsOfTuples = mapM (flip fmap ((`mod` 200) . getPositive <$> arbitrary) . (,))
  generate $ numReleasesInEach >>= mapM (listsOfTimestamps >=> listsOfTuples)

setOfTransactions :: Int -> IO ([[(Timestamp,Amount)]], OrdMap.Map Timestamp Amount, [Timestamp])
setOfTransactions numTxs = do
  txs <- transactions numTxs
  let amounts = OrdMap.fromListWith (+) . concat $ txs
  let timestamps = OrdMap.keys amounts
  return (txs, amounts, timestamps)

------------------------------- Helper functions -------------------------------

createGS :: FilePath -> IO (Identity PairedGSContext, Identity PairedGState)
createGS dbDir = do
  now <- utcTimeToTimestamp <$> getCurrentTime
  let
    n = 3
    genesis = makeTestingGenesisDataP5 now n 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters emptyIdentityProviders emptyAnonymityRevokers maxBound dummyKeyCollection dummyChainParameters
    rp = defaultRuntimeParameters
    config = PairGSConfig (MTMBConfig rp, DTDBConfig rp dbDir (dbDir </> "blockstate" <.> "dat"))
  (x, y) <- runSilentLogger $ initialiseGlobalState genesis config
  return (Identity x, Identity y)

destroyGS :: (Identity PairedGSContext, Identity PairedGState) -> IO ()
destroyGS (Identity c, Identity s) = shutdownGlobalState (protocolVersion @PV) (Proxy :: Proxy (PairGSConfig MemoryTreeMemoryBlockConfig DiskTreeDiskBlockConfig)) c s

------------------------------------- Test -------------------------------------

testing :: ThisMonadConcrete ()
testing = do
  (!txsA, !amountsA, !timestampsA) <- liftIO $ setOfTransactions 3
  (!txsB, !amountsB, !timestampsB) <- liftIO $ setOfTransactions 2
  bs <- get
  let -- get the blockstates
      bs1 = _unhashedBlockState $ _bpState (bs ^. pairStateLeft . Concordium.GlobalState.Basic.TreeState.focusBlock)
      bs2 = PBS.hpbsPointers $ _bpState (bs ^. pairStateRight . Concordium.GlobalState.Persistent.TreeState.focusBlock)

  let [(accA, aiA), (accB, aiB)] = take 2 $ map (\(ai, ac) -> (ac ^. accountAddress, ai)) $ AT.toList $ accountTable (bs1 ^. blockAccounts)
  bs'' <- foldlM (\b e@((_, ai), rels) -> do
                     newB <- bsoModifyAccount b ((emptyAccountUpdate ai) { _auReleaseSchedule = Just [(rels, dummyTransactionHash)] })
                     checkCorrectAccountReleaseSchedule newB b e
                     return newB
                 ) (bs1, bs2) (map ((accA, aiA),) txsA ++ map ((accB, aiB),) txsB)
  let expectedRS = OrdMap.unionWith (<>)
        (OrdMap.singleton (head timestampsA) (Set.singleton aiA))
        (OrdMap.singleton (head timestampsB) (Set.singleton aiB))
  checkEqualBlockReleaseSchedule expectedRS bs''
  let taggedAmounts = OrdMap.toAscList $ OrdMap.unionWith (++) (OrdMap.map ((:[]) . (accA,)) amountsA) (OrdMap.map ((:[]) . (accB,)) amountsB)
  _ <- foldlM (\b (ts, accs) -> do
            newB <- bsoProcessReleaseSchedule b ts
            checkCorrectShrinkingAccountReleaseSchedule newB b ts accs
            return newB) bs'' taggedAmounts
  return ()


tests :: Spec
tests = do
  describe "GlobalState.AccountReleaseScheduleTest"
    $ specify "correct releases"
    $ withTempDirectory "." "test-directory"
    $ \dbDir -> bracket (createGS dbDir) destroyGS $ \a -> do
    _ <- runSilentLogger . uncurry (runRWST (runBlockStateM testing)) $ a
    return ()

------------------------------------ Checks ------------------------------------

-- | Check that the two implementations of blockstate have the same Block release schedule,
-- and that this is the same as the expected schedule.
checkEqualBlockReleaseSchedule :: OrdMap.Map Timestamp (Set.Set AccountIndex) -> (BS.BlockState PV, PBS.PersistentBlockState PV) -> ThisMonadConcrete ()
checkEqualBlockReleaseSchedule expected (blockstateBasic, blockStatePersistent) = do
  let brs = BRS.rsMap (_blockReleaseSchedule blockstateBasic)
  ctx <- _pairContextRight <$> R.ask
  brsP <- liftIO $ runBlobStoreM (do
                                 blockStatePersistent' <- loadBufferedRef =<< liftIO (readIORef  blockStatePersistent)
                                 let resolveAcc addr = do
                                        fromJust <$> Concordium.GlobalState.Persistent.Accounts.getAccountIndex addr (PBS.bspAccounts blockStatePersistent')
                                 PRS.releasesMap resolveAcc (PBS.bspReleaseSchedule blockStatePersistent')
                                 ) ctx
  liftIO $ assertEqual "Basic & expected release schedules" brs expected
  liftIO $ assertEqual "Basic & persistent release schedules" brs brsP
  
-- | Check that an account has the same Account release schedule in the two implementations of the blockstate
checkEqualAccountReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> AccountAddress -> ThisMonadConcrete ()
checkEqualAccountReleaseSchedule (blockStateBasic, blockStatePersistent) acc = do
  let Just newBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (blockStateBasic ^. blockAccounts)
  ctx <- _pairContextRight <$> R.ask
  newPersistentAccountReleaseScheduleHash <- liftIO $ runBlobStoreM (do
                                                  blockStatePersistent' <- loadBufferedRef =<< liftIO (readIORef  blockStatePersistent)
                                                  Concordium.GlobalState.Persistent.Accounts.getAccount acc (PBS.bspAccounts blockStatePersistent') >>= \case
                                                    Nothing -> return Nothing
                                                    Just (Concordium.GlobalState.Persistent.Account.PAV2 a) -> Just . getHash <$> Concordium.GlobalState.Persistent.Account.StructureV1.getReleaseSchedule a) ctx :: ThisMonadConcrete (Maybe TARSV1.AccountReleaseScheduleHashV1)
  liftIO $ assertEqual "Basic & persistent account release schedule hashes"
      (Just (getHash (newBasicAccount ^. accountReleaseSchedule)))
      newPersistentAccountReleaseScheduleHash

-- | Check that an an account was correctly updated in the two blockstates with the given release schedule
checkCorrectAccountReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> (BS.BlockState PV, PBS.PersistentBlockState PV) -> ((AccountAddress, AccountIndex), [(Timestamp, Amount)]) -> ThisMonadConcrete ()
checkCorrectAccountReleaseSchedule (blockStateBasic, blockStatePersistent) (oldBlockStateBasic, _) ((acc, _), rel) = do
  let Just newBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (blockStateBasic ^. blockAccounts)
  let Just oldBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (oldBlockStateBasic ^. blockAccounts)
  checkEqualAccountReleaseSchedule (blockStateBasic, blockStatePersistent) acc
  liftIO $ assertEqual "Old amount + sum of new releases == new amount"
      (oldBasicAccount ^. accountAmount + sum (map snd rel)) (newBasicAccount ^. accountAmount)
  let oldReleaseSchedule = oldBasicAccount ^. accountReleaseSchedule . to (TARSV1.arsReleases . theAccountReleaseSchedule)
  let newReleaseSchedule = newBasicAccount ^. accountReleaseSchedule . to (TARSV1.arsReleases . theAccountReleaseSchedule)
  let testEntry TARSV1.ReleaseScheduleEntry{..} = toList (TARSV1.relReleases rseReleases) == rel
  -- Ensure that the new release schedule contains a new release entry which contains exactly the releases specified.
  let comp b (x:xs) (y:ys)
        | x == y = comp b xs ys
        | otherwise = do
            assertBool "Test release" $ testEntry y
            comp True (x:xs) ys
      comp False [] [y] = assertBool "Release not equal" $ testEntry y
      comp b [] [] = assertBool "New release not found" $ b
      comp _ left right = assertFailure $ "Lists of unexpected length: " ++ show oldReleaseSchedule ++ ", " ++ show newReleaseSchedule ++ ", " ++ show rel ++ ", " ++ show left ++ ", " ++ show right
  liftIO (comp False oldReleaseSchedule newReleaseSchedule)

-- | Check that the implementations have the same data and that the difference between the old state and the new state after unlocking amounts at the given timestamp is the amount given in the last argument
-- also check that all the timestamps for this account are over the unlocked timestamps.
checkCorrectShrinkingAccountReleaseSchedule :: (BS.BlockState PV, PBS.PersistentBlockState PV) -> (BS.BlockState PV, PBS.PersistentBlockState PV) -> Timestamp -> [(AccountAddress, Amount)] -> ThisMonadConcrete ()
checkCorrectShrinkingAccountReleaseSchedule (blockStateBasic, blockStatePersistent) (oldBlockStateBasic, _) ts accs =
  let f (acc, rel) = do
        let Just newBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (blockStateBasic ^. blockAccounts)
        let Just oldBasicAccount = Concordium.GlobalState.Basic.BlockState.Accounts.getAccount acc (oldBlockStateBasic ^. blockAccounts)
        checkEqualAccountReleaseSchedule (blockStateBasic, blockStatePersistent) acc
        liftIO $ assertEqual "New locked balance + released amount == old locked balance"
          (newBasicAccount ^. accountReleaseSchedule . totalLockedUpBalance + rel)
          (oldBasicAccount ^. accountReleaseSchedule . totalLockedUpBalance)
        let rs = newBasicAccount ^. accountReleaseSchedule . to (TARSV1.arsReleases . theAccountReleaseSchedule)
        liftIO $ assertEqual "Sum of releases == locked balance"
          (foldl' (\accum TARSV1.ReleaseScheduleEntry{..} -> sum (fmap snd (TARSV1.relReleases rseReleases)) + accum) 0 rs)
          (newBasicAccount ^. accountReleaseSchedule . totalLockedUpBalance)
        liftIO $ assertBool "Remaining releases are after release time" (all (\TARSV1.ReleaseScheduleEntry{..} -> any (\(t, _) -> t > ts) (TARSV1.relReleases rseReleases)) rs)
  in
    mapM_ f accs
